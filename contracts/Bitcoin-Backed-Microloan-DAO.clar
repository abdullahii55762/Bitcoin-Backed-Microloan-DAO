(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_LOAN_NOT_FOUND (err u102))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_LOAN_NOT_ACTIVE (err u105))
(define-constant ERR_ALREADY_VOTED (err u106))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u107))
(define-constant ERR_VOTING_ENDED (err u108))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u109))
(define-constant ERR_LOAN_OVERDUE (err u110))

(define-data-var total-pool uint u0)
(define-data-var next-loan-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var dao-fee-rate uint u500)

(define-map pool-contributors principal uint)
(define-map credit-scores principal uint)
(define-map loans uint {
    borrower: principal,
    amount: uint,
    collateral: uint,
    interest-rate: uint,
    duration: uint,
    start-block: uint,
    status: (string-ascii 20),
    repaid-amount: uint
})

(define-map loan-applications uint {
    borrower: principal,
    requested-amount: uint,
    collateral-amount: uint,
    business-description: (string-ascii 500),
    duration: uint,
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    status: (string-ascii 20)
})

(define-map application-voters {proposal-id: uint, voter: principal} bool)
(define-map borrower-loans principal (list 10 uint))
(define-map repayment-history uint (list 50 {amount: uint, block-height: uint}))

(define-public (contribute-to-pool (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-pool (+ (var-get total-pool) amount))
        (map-set pool-contributors tx-sender 
            (+ (default-to u0 (map-get? pool-contributors tx-sender)) amount))
        (ok amount)
    )
)

(define-public (apply-for-loan (amount uint) (collateral uint) (description (string-ascii 500)) (duration uint))
    (let ((proposal-id (var-get next-proposal-id)))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (>= collateral (* amount u150)) ERR_INSUFFICIENT_COLLATERAL)
        (asserts! (> duration u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
        (map-set loan-applications proposal-id {
            borrower: tx-sender,
            requested-amount: amount,
            collateral-amount: collateral,
            business-description: description,
            duration: duration,
            votes-for: u0,
            votes-against: u0,
            voting-deadline: (+ stacks-block-height u1440),
            status: "pending"
        })
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-application (proposal-id uint) (vote bool))
    (let ((application (unwrap! (map-get? loan-applications proposal-id) ERR_PROPOSAL_NOT_FOUND))
          (contributor-amount (default-to u0 (map-get? pool-contributors tx-sender))))
        (asserts! (> contributor-amount u0) ERR_UNAUTHORIZED)
        (asserts! (< stacks-block-height (get voting-deadline application)) ERR_VOTING_ENDED)
        (asserts! (is-none (map-get? application-voters {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        (map-set application-voters {proposal-id: proposal-id, voter: tx-sender} true)
        (if vote
            (map-set loan-applications proposal-id 
                (merge application {votes-for: (+ (get votes-for application) contributor-amount)}))
            (map-set loan-applications proposal-id 
                (merge application {votes-against: (+ (get votes-against application) contributor-amount)}))
        )
        (ok true)
    )
)

(define-public (finalize-loan-application (proposal-id uint))
    (let ((application (unwrap! (map-get? loan-applications proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (>= stacks-block-height (get voting-deadline application)) ERR_VOTING_ENDED)
        (asserts! (is-eq (get status application) "pending") ERR_LOAN_NOT_ACTIVE)
        (if (> (get votes-for application) (get votes-against application))
            (begin
                (try! (approve-loan proposal-id))
                (ok "approved")
            )
            (begin
                (try! (reject-loan proposal-id))
                (ok "rejected")
            )
        )
    )
)

(define-private (approve-loan-internal (proposal-id uint))
    (let ((application (unwrap! (map-get? loan-applications proposal-id) ERR_PROPOSAL_NOT_FOUND))
          (loan-id (var-get next-loan-id))
          (amount (get requested-amount application))
          (interest-rate (calculate-interest-rate (get borrower application))))
        (asserts! (>= (var-get total-pool) amount) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (get borrower application))))
        (var-set total-pool (- (var-get total-pool) amount))
        (map-set loans loan-id {
            borrower: (get borrower application),
            amount: amount,
            collateral: (get collateral-amount application),
            interest-rate: interest-rate,
            duration: (get duration application),
            start-block: stacks-block-height,
            status: "active",
            repaid-amount: u0
        })
        (map-set loan-applications proposal-id (merge application {status: "approved"}))
        (try! (update-borrower-loans (get borrower application) loan-id))
        (var-set next-loan-id (+ loan-id u1))
        (ok loan-id)
    )
)

(define-private (reject-loan (proposal-id uint))
    (let ((application (unwrap! (map-get? loan-applications proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (try! (as-contract (stx-transfer? (get collateral-amount application) tx-sender (get borrower application))))
        (map-set loan-applications proposal-id (merge application {status: "rejected"}))
        (ok true)
    )
)

(define-public (repay-loan (loan-id uint) (amount uint))
    (let ((loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get borrower loan)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (let ((new-repaid (+ (get repaid-amount loan) amount))
              (total-due (calculate-total-due loan-id)))
            (var-set total-pool (+ (var-get total-pool) amount))
            (try! (update-repayment-history loan-id amount))
            (if (>= new-repaid total-due)
                (begin
                    (map-set loans loan-id (merge loan {repaid-amount: new-repaid, status: "completed"}))
                    (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
                    (update-credit-score (get borrower loan) true)
                    (ok new-repaid)
                )
                (begin 
                    (map-set loans loan-id (merge loan {repaid-amount: new-repaid}))
                    (ok new-repaid)
                )
            )
        )
    )
)

(define-public (liquidate-loan (loan-id uint))
    (let ((loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND)))
        (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
        (asserts! (is-loan-overdue loan-id) ERR_LOAN_NOT_ACTIVE)
        (var-set total-pool (+ (var-get total-pool) (get collateral loan)))
        (map-set loans loan-id (merge loan {status: "liquidated"}))
        (update-credit-score (get borrower loan) false)
        (ok (get collateral loan))
    )
)

(define-private (update-borrower-loans (borrower principal) (loan-id uint))
    (let ((current-loans (default-to (list) (map-get? borrower-loans borrower))))
        (ok (map-set borrower-loans borrower (unwrap! (as-max-len? (append current-loans loan-id) u10) (err u999))))
    )
)

(define-private (update-repayment-history (loan-id uint) (amount uint))
    (let ((current-history (default-to (list) (map-get? repayment-history loan-id)))
          (new-entry {amount: amount, block-height: stacks-block-height}))
        (ok (map-set repayment-history loan-id 
            (unwrap! (as-max-len? (append current-history new-entry) u50) (err u999))))
    )
)

(define-private (calculate-loan-interest-rate (borrower principal))
    (let ((credit-score (default-to u500 (map-get? credit-scores borrower))))
        (if (>= credit-score u800)
            u800
            (if (>= credit-score u600)
                u1200
                u1800
            )
        )
    )
)

(define-private (calculate-total-due (loan-id uint))
    (let ((loan (unwrap! (map-get? loans loan-id) u0)))
        (+ (get amount loan) 
           (/ (* (get amount loan) (get interest-rate loan)) u10000))
    )
)

(define-private (update-credit-score (borrower principal) (positive bool))
    (let ((current-score (default-to u500 (map-get? credit-scores borrower))))
        (if positive
            (map-set credit-scores borrower (if (> (+ current-score u50) u1000) u1000 (+ current-score u50)))
            (map-set credit-scores borrower (if (< (- current-score u100) u100) u100 (- current-score u100)))
        )
    )
)

(define-private (is-loan-overdue (loan-id uint))
    (let ((loan (unwrap! (map-get? loans loan-id) false)))
        (> stacks-block-height (+ (get start-block loan) (get duration loan)))
    )
)

(define-read-only (get-pool-balance)
    (var-get total-pool)
)

(define-read-only (get-contributor-balance (contributor principal))
    (default-to u0 (map-get? pool-contributors contributor))
)

(define-read-only (get-credit-score (borrower principal))
    (default-to u500 (map-get? credit-scores borrower))
)

(define-read-only (get-loan-details (loan-id uint))
    (map-get? loans loan-id)
)

(define-read-only (get-loan-application (proposal-id uint))
    (map-get? loan-applications proposal-id)
)

(define-read-only (get-borrower-loans (borrower principal))
    (default-to (list) (map-get? borrower-loans borrower))
)

(define-read-only (get-repayment-history (loan-id uint))
    (default-to (list) (map-get? repayment-history loan-id))
)

(define-read-only (calculate-loan-due (loan-id uint))
    (calculate-total-due loan-id)
)
(define-constant ERR_INSUFFICIENT_INSURANCE_FUNDS (err u200))
(define-constant ERR_NOT_INSURED (err u201))
(define-constant ERR_ALREADY_CLAIMED (err u202))
(define-constant INSURANCE_RATE u300)

(define-data-var total-insurance-pool uint u0)
(define-data-var insurance-claims-paid uint u0)

(define-map insurance-contributors principal uint)
(define-map insured-loans uint {
    loan-id: uint,
    insured-amount: uint,
    premium-paid: uint,
    claim-status: bool
})
(define-map loan-insurance-mapping uint uint)

(define-public (contribute-to-insurance (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-insurance-pool (+ (var-get total-insurance-pool) amount))
        (map-set insurance-contributors tx-sender 
            (+ (default-to u0 (map-get? insurance-contributors tx-sender)) amount))
        (ok amount)
    )
)

(define-public (insure-loan (loan-id uint))
    (let ((loan (unwrap! (get-loan-details loan-id) ERR_LOAN_NOT_FOUND))
          (premium (/ (* (get amount loan) INSURANCE_RATE) u10000))
          (insurance-id (var-get next-proposal-id)))
        (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
        (asserts! (is-none (map-get? loan-insurance-mapping loan-id)) ERR_LOAN_ALREADY_EXISTS)
        (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
        (var-set total-insurance-pool (+ (var-get total-insurance-pool) premium))
        (map-set insured-loans insurance-id {
            loan-id: loan-id,
            insured-amount: (get amount loan),
            premium-paid: premium,
            claim-status: false
        })
        (map-set loan-insurance-mapping loan-id insurance-id)
        (var-set next-proposal-id (+ insurance-id u1))
        (ok insurance-id)
    )
)

(define-public (claim-insurance (loan-id uint))
    (let ((insurance-id (unwrap! (map-get? loan-insurance-mapping loan-id) ERR_NOT_INSURED))
          (insurance (unwrap! (map-get? insured-loans insurance-id) ERR_NOT_INSURED))
          (loan (unwrap! (get-loan-details loan-id) ERR_LOAN_NOT_FOUND)))
        (asserts! (is-eq (get status loan) "liquidated") ERR_LOAN_NOT_ACTIVE)
        (asserts! (not (get claim-status insurance)) ERR_ALREADY_CLAIMED)
        (asserts! (>= (var-get total-insurance-pool) (get insured-amount insurance)) ERR_INSUFFICIENT_INSURANCE_FUNDS)
        (var-set total-insurance-pool (- (var-get total-insurance-pool) (get insured-amount insurance)))
        (var-set insurance-claims-paid (+ (var-get insurance-claims-paid) (get insured-amount insurance)))
        (var-set total-pool (+ (var-get total-pool) (get insured-amount insurance)))
        (map-set insured-loans insurance-id (merge insurance {claim-status: true}))
        (ok (get insured-amount insurance))
    )
)

(define-read-only (get-insurance-pool-balance)
    (var-get total-insurance-pool)
)

(define-read-only (get-insurance-contributor-balance (contributor principal))
    (default-to u0 (map-get? insurance-contributors contributor))
)

(define-read-only (get-loan-insurance (loan-id uint))
    (match (map-get? loan-insurance-mapping loan-id)
        insurance-id (map-get? insured-loans insurance-id)
        none
    )
)

(define-read-only (calculate-insurance-premium (amount uint))
    (/ (* amount INSURANCE_RATE) u10000)
)

(define-private (calculate-interest-rate (borrower principal))
    (let ((base-rate u1000)  ;; 10% base rate represented as basis points
          (credit-score (default-to u0 (map-get? credit-scores borrower))))
        (if (> credit-score u50)
            (- base-rate u200)  ;; Reduce rate by 2% for good credit
            base-rate))  ;; Keep base rate for lower credit scores
)

(define-private (approve-loan (proposal-id uint))
    (let ((application (unwrap! (map-get? loan-applications proposal-id) ERR_PROPOSAL_NOT_FOUND))
          (loan-id (var-get next-loan-id))
          (amount (get requested-amount application))
          (interest-rate (calculate-interest-rate (get borrower application))))
        (asserts! (>= (var-get total-pool) amount) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (get borrower application))))
        (var-set total-pool (- (var-get total-pool) amount))
        (map-set loans loan-id {
            borrower: (get borrower application),
            amount: amount,
            collateral: (get collateral-amount application),
            interest-rate: interest-rate,
            duration: (get duration application),
            start-block: stacks-block-height,
            status: "active",
            repaid-amount: u0
        })
        (map-set loan-applications proposal-id (merge application {status: "approved"}))
        (try! (update-borrower-loans (get borrower application) loan-id))
        (var-set next-loan-id (+ loan-id u1))
        (ok loan-id)
    )
)

(define-public (repay-loan-v2 (loan-id uint) (amount uint))
    (let ((loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get borrower loan)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status loan) "active") ERR_LOAN_NOT_ACTIVE)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (let ((new-repaid (+ (get repaid-amount loan) amount))
              (total-due (calculate-total-due loan-id)))
            (var-set total-pool (+ (var-get total-pool) amount))
            (try! (update-repayment-history loan-id amount))
            (if (>= new-repaid total-due)
                (begin
                    (map-set loans loan-id (merge loan {repaid-amount: new-repaid, status: "completed"}))
                    (try! (as-contract (stx-transfer? (get collateral loan) tx-sender (get borrower loan))))
                    (update-credit-score (get borrower loan) true)
                    (ok new-repaid)
                )
                (begin 
                    (map-set loans loan-id (merge loan {repaid-amount: new-repaid}))
                    (ok new-repaid)
                )
            )
        )
    )
)