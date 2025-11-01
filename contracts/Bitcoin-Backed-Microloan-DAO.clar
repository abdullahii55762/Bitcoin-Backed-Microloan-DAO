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

(define-constant ERR_SYSTEM_PAUSED (err u500))
(define-constant ERR_PAUSE_PROPOSAL_EXISTS (err u501))
(define-constant ERR_INSUFFICIENT_VOTING_POWER (err u502))
(define-constant ERR_PAUSE_PROPOSAL_NOT_FOUND (err u503))
(define-constant PAUSE_THRESHOLD u300000)
(define-constant RESUME_QUORUM u600000)

(define-data-var system-paused bool false)
(define-data-var pause-initiated-at uint u0)
(define-data-var pause-reason (string-ascii 200) "")
(define-data-var next-pause-proposal-id uint u1)

(define-constant ERR_CANNOT_DELEGATE_TO_SELF (err u111))
(define-constant ERR_NO_ACTIVE_DELEGATION (err u112))

(define-map voting-delegations principal principal)
(define-map delegation-revoked-at principal uint)

(define-constant ERR_NO_YIELD_TO_WITHDRAW (err u400))
(define-constant YIELD_DISTRIBUTION_RATE u7000)

(define-data-var total-yield-distributed uint u0)
(define-data-var total-yield-pool uint u0)

(define-private (min (a uint) (b uint))
    (if (< a b) a b))

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
        (map-set contributor-yield-data tx-sender {
            contribution-block: stacks-block-height,
            last-yield-calculation: stacks-block-height,
            accumulated-yield: u0,
            withdrawn-yield: u0
        })
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
            (let ((interest-portion (if (> new-repaid (get amount loan))
                     (min (- new-repaid (get amount loan)) (- total-due (get amount loan))) u0)))
                (unwrap-panic (distribute-interest-yield interest-portion)))
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

(define-constant ERR_NO_PERFORMANCE_DATA (err u300))
(define-constant PERFORMANCE_WINDOW_BLOCKS u14400)
(define-constant HIGH_RISK_THRESHOLD u300)
(define-constant MEDIUM_RISK_THRESHOLD u600)

(define-data-var total-defaults uint u0)
(define-data-var total-completed-loans uint u0)

(define-map borrower-performance principal {
    total-borrowed: uint,
    total-repaid: uint,
    loans-completed: uint,
    loans-defaulted: uint,
    avg-repayment-time: uint,
    last-loan-block: uint,
    risk-score: uint,
    performance-rating: (string-ascii 10)
})

(define-map loan-performance-data uint {
    expected-completion-block: uint,
    actual-completion-block: uint,
    repayment-efficiency: uint,
    risk-category: (string-ascii 10)
})

(define-public (update-borrower-performance (borrower principal) (loan-id uint) (completion-type (string-ascii 10)))
    (let ((loan (unwrap! (get-loan-details loan-id) ERR_LOAN_NOT_FOUND))
          (default-perf {total-borrowed: u0, total-repaid: u0, loans-completed: u0, loans-defaulted: u0, avg-repayment-time: u0, last-loan-block: u0, risk-score: u500, performance-rating: "unrated"})
          (current-perf (default-to default-perf (map-get? borrower-performance borrower))))
        (let ((new-perf (if (is-eq completion-type "completed")
                {
                    total-borrowed: (+ (get total-borrowed current-perf) (get amount loan)),
                    total-repaid: (+ (get total-repaid current-perf) (get repaid-amount loan)),
                    loans-completed: (+ (get loans-completed current-perf) u1),
                    loans-defaulted: (get loans-defaulted current-perf),
                    avg-repayment-time: u0,
                    last-loan-block: stacks-block-height,
                    risk-score: u700,
                    performance-rating: "good"
                }
                {
                    total-borrowed: (+ (get total-borrowed current-perf) (get amount loan)),
                    total-repaid: (get total-repaid current-perf),
                    loans-completed: (get loans-completed current-perf),
                    loans-defaulted: (+ (get loans-defaulted current-perf) u1),
                    avg-repayment-time: u0,
                    last-loan-block: stacks-block-height,
                    risk-score: u300,
                    performance-rating: "poor"
                })))
            (map-set borrower-performance borrower new-perf)
            (try! (update-loan-performance-data loan-id completion-type))
            (ok true))))

(define-private (update-loan-performance-data (loan-id uint) (completion-type (string-ascii 10)))
    (let ((loan (unwrap! (get-loan-details loan-id) ERR_LOAN_NOT_FOUND)))
        (let ((expected-completion (+ (get start-block loan) (get duration loan)))
              (actual-completion stacks-block-height)
              (efficiency (calculate-repayment-efficiency expected-completion actual-completion))
              (risk-cat (if (is-eq completion-type "completed") "low" "high")))
            (map-set loan-performance-data loan-id {
                expected-completion-block: expected-completion,
                actual-completion-block: actual-completion,
                repayment-efficiency: efficiency,
                risk-category: risk-cat
            })
            (ok true))))

(define-private (calculate-borrower-risk-score (borrower principal) (stats {total-borrowed: uint, total-repaid: uint, loans-completed: uint, loans-defaulted: uint, avg-repayment-time: uint, last-loan-block: uint, risk-score: uint, performance-rating: (string-ascii 10)}))
    (let ((total-loans (+ (get loans-completed stats) (get loans-defaulted stats)))
          (default-rate (if (> total-loans u0) (/ (* (get loans-defaulted stats) u1000) total-loans) u0))
          (repayment-rate (if (> (get total-borrowed stats) u0) 
                (/ (* (get total-repaid stats) u1000) (get total-borrowed stats)) u0)))
        (let ((base-score u1000)
              (default-penalty (* default-rate u2))
              (repayment-bonus (/ repayment-rate u2)))
            (if (> (+ base-score repayment-bonus) default-penalty)
                (- (+ base-score repayment-bonus) default-penalty)
                u100))))

(define-private (calculate-repayment-efficiency (expected uint) (actual uint))
    (if (<= actual expected)
        u1000
        (if (> actual (+ expected PERFORMANCE_WINDOW_BLOCKS))
            u0
            (/ (* u1000 expected) actual))))

(define-private (get-performance-rating (risk-score uint))
    (if (>= risk-score u800) "excellent"
        (if (>= risk-score MEDIUM_RISK_THRESHOLD) "good"
            (if (>= risk-score HIGH_RISK_THRESHOLD) "fair"
                "poor"))))

(define-read-only (get-borrower-analytics (borrower principal))
    (map-get? borrower-performance borrower))

(define-read-only (get-loan-analytics (loan-id uint))
    (map-get? loan-performance-data loan-id))

(define-read-only (get-portfolio-metrics)
    {
        total-defaults: (var-get total-defaults),
        total-completed: (var-get total-completed-loans),
        success-rate: (if (> (var-get total-completed-loans) u0)
            (/ (* (var-get total-completed-loans) u100) 
               (+ (var-get total-completed-loans) (var-get total-defaults)))
            u0)
    })


(define-map contributor-yield-data principal {
    contribution-block: uint,
    last-yield-calculation: uint,
    accumulated-yield: uint,
    withdrawn-yield: uint
})

(define-map yield-snapshots uint {
    block-height: uint,
    total-pool-at-snapshot: uint,
    yield-amount: uint
})

(define-public (calculate-contributor-yield (contributor principal))
    (let ((contrib-data (map-get? contributor-yield-data contributor))
          (contribution-amount (default-to u0 (map-get? pool-contributors contributor))))
        (match contrib-data
            data (let ((time-factor (- stacks-block-height (get contribution-block data)))
                       (pool-share (if (> (var-get total-pool) u0)
                           (/ (* contribution-amount u10000) (var-get total-pool)) u0))
                       (yield-earned (/ (* (var-get total-yield-pool) pool-share time-factor) u100000000)))
                (map-set contributor-yield-data contributor
                    (merge data {
                        last-yield-calculation: stacks-block-height,
                        accumulated-yield: (+ (get accumulated-yield data) yield-earned)
                    }))
                (ok yield-earned))
            (ok u0))))

(define-public (withdraw-yield)
    (let ((contrib-data (unwrap! (map-get? contributor-yield-data tx-sender) ERR_UNAUTHORIZED)))
        (unwrap-panic (calculate-contributor-yield tx-sender))
        (let ((updated-data (unwrap! (map-get? contributor-yield-data tx-sender) ERR_UNAUTHORIZED))
              (withdrawable (- (get accumulated-yield updated-data) (get withdrawn-yield updated-data))))
            (asserts! (> withdrawable u0) ERR_NO_YIELD_TO_WITHDRAW)
            (try! (as-contract (stx-transfer? withdrawable tx-sender tx-sender)))
            (map-set contributor-yield-data tx-sender
                (merge updated-data {withdrawn-yield: (+ (get withdrawn-yield updated-data) withdrawable)}))
            (var-set total-yield-distributed (+ (var-get total-yield-distributed) withdrawable))
            (ok withdrawable))))

(define-private (distribute-interest-yield (interest-amount uint))
    (let ((yield-share (/ (* interest-amount YIELD_DISTRIBUTION_RATE) u10000)))
        (var-set total-yield-pool (+ (var-get total-yield-pool) yield-share))
        (map-set yield-snapshots stacks-block-height {
            block-height: stacks-block-height,
            total-pool-at-snapshot: (var-get total-pool),
            yield-amount: yield-share
        })
        (ok yield-share)))

(define-read-only (get-contributor-yield-info (contributor principal))
    (let ((contrib-data (map-get? contributor-yield-data contributor)))
        (match contrib-data
            data {
                contribution-block: (get contribution-block data),
                accumulated-yield: (get accumulated-yield data),
                withdrawn-yield: (get withdrawn-yield data),
                pending-yield: (- (get accumulated-yield data) (get withdrawn-yield data))
            }
            {contribution-block: u0, accumulated-yield: u0, withdrawn-yield: u0, pending-yield: u0})))

(define-read-only (get-yield-metrics)
    {
        total-yield-pool: (var-get total-yield-pool),
        total-distributed: (var-get total-yield-distributed),
        current-apy: (if (> (var-get total-pool) u0)
            (/ (* (var-get total-yield-pool) u10000) (var-get total-pool)) u0)
    })

(define-public (delegate-voting-power (delegate principal))
    (let ((contributor-amount (default-to u0 (map-get? pool-contributors tx-sender))))
        (asserts! (> contributor-amount u0) ERR_UNAUTHORIZED)
        (asserts! (not (is-eq tx-sender delegate)) ERR_CANNOT_DELEGATE_TO_SELF)
        (map-set voting-delegations tx-sender delegate)
        (map-delete delegation-revoked-at tx-sender)
        (ok delegate)
    )
)

(define-public (revoke-delegation)
    (let ((current-delegate (map-get? voting-delegations tx-sender)))
        (asserts! (is-some current-delegate) ERR_NO_ACTIVE_DELEGATION)
        (map-delete voting-delegations tx-sender)
        (map-set delegation-revoked-at tx-sender stacks-block-height)
        (ok true)
    )
)

(define-private (calculate-effective-voting-power (voter principal) (contributors-list (list 100 principal)) (accumulated-power uint))
    (fold check-delegation-to-voter-closure contributors-list accumulated-power)
)

(define-private (check-delegation-to-voter-closure (contributor principal) (current-power uint))
    (let ((delegate-target (map-get? voting-delegations contributor))
          (contribution (default-to u0 (map-get? pool-contributors contributor))))
        (if (and (is-some delegate-target) (is-eq (unwrap-panic delegate-target) tx-sender))
            (+ current-power contribution)
            current-power
        )
    )
)

(define-private (check-delegation-to-voter (contributor principal) (current-power uint) (target-voter principal))
    (let ((delegate-target (map-get? voting-delegations contributor))
          (contribution (default-to u0 (map-get? pool-contributors contributor))))
        (if (is-eq (unwrap! delegate-target current-power) target-voter)
            (+ current-power contribution)
            current-power
        )
    )
)

(define-read-only (get-delegation-target (contributor principal))
    (map-get? voting-delegations contributor)
)

(define-read-only (get-effective-voting-power (voter principal))
    (let ((own-contribution (default-to u0 (map-get? pool-contributors voter))))
        own-contribution
    )
)

(define-read-only (has-active-delegation (contributor principal))
    (is-some (map-get? voting-delegations contributor))
)

(define-read-only (get-delegation-info (contributor principal))
    {
        delegated-to: (map-get? voting-delegations contributor),
        revoked-at: (default-to u0 (map-get? delegation-revoked-at contributor)),
        is-active: (is-some (map-get? voting-delegations contributor))
    }
)

(define-map pause-proposals uint {
    proposer: principal,
    reason: (string-ascii 200),
    votes-for-resume: uint,
    proposal-block: uint,
    is-active: bool
})

(define-map pause-voters {proposal-id: uint, voter: principal} bool)

(define-public (emergency-pause (reason (string-ascii 200)))
    (let ((voting-power (get-effective-voting-power tx-sender)))
        (asserts! (>= voting-power PAUSE_THRESHOLD) ERR_INSUFFICIENT_VOTING_POWER)
        (asserts! (not (var-get system-paused)) ERR_PAUSE_PROPOSAL_EXISTS)
        (var-set system-paused true)
        (var-set pause-initiated-at stacks-block-height)
        (var-set pause-reason reason)
        (ok true)
    )
)

(define-public (propose-resume)
    (let ((proposal-id (var-get next-pause-proposal-id)))
        (asserts! (var-get system-paused) ERR_PAUSE_PROPOSAL_NOT_FOUND)
        (map-set pause-proposals proposal-id {
            proposer: tx-sender,
            reason: (var-get pause-reason),
            votes-for-resume: u0,
            proposal-block: stacks-block-height,
            is-active: true
        })
        (var-set next-pause-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-resume (proposal-id uint))
    (let ((proposal (unwrap! (map-get? pause-proposals proposal-id) ERR_PAUSE_PROPOSAL_NOT_FOUND))
          (voting-power (get-effective-voting-power tx-sender)))
        (asserts! (var-get system-paused) ERR_PAUSE_PROPOSAL_NOT_FOUND)
        (asserts! (> voting-power u0) ERR_INSUFFICIENT_VOTING_POWER)
        (asserts! (is-none (map-get? pause-voters {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        (map-set pause-voters {proposal-id: proposal-id, voter: tx-sender} true)
        (let ((new-votes (+ (get votes-for-resume proposal) voting-power)))
            (map-set pause-proposals proposal-id (merge proposal {votes-for-resume: new-votes}))
            (if (>= new-votes RESUME_QUORUM)
                (begin
                    (var-set system-paused false)
                    (map-set pause-proposals proposal-id (merge proposal {is-active: false}))
                    (ok true)
                )
                (ok false)
            )
        )
    )
)

(define-read-only (is-system-paused)
    (var-get system-paused)
)

(define-read-only (get-pause-details)
    {
        is-paused: (var-get system-paused),
        paused-at: (var-get pause-initiated-at),
        reason: (var-get pause-reason)
    }
)

(define-read-only (get-resume-proposal (proposal-id uint))
    (map-get? pause-proposals proposal-id)
)