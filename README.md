# 🏦 Bitcoin-Backed Microloan DAO

> 🌍 Empowering small businesses in emerging markets through decentralized microfinance

## 📋 Overview

The Bitcoin-Backed Microloan DAO is a decentralized autonomous organization that manages pooled BTC-backed microloans for small businesses. The platform features on-chain credit scoring, community-driven loan approval, and automated repayment tracking.

## ✨ Key Features

- 💰 **Community Pool**: Contributors can add funds to the lending pool
- 📝 **Loan Applications**: Borrowers submit collateralized loan requests
- 🗳️ **DAO Voting**: Pool contributors vote on loan applications
- 📊 **Credit Scoring**: Dynamic on-chain credit scores (100-1000)
- 🔒 **Collateral Management**: 150% collateral requirement for loan security
- ⚡ **Auto-liquidation**: Overdue loans trigger collateral liquidation
- 📈 **Interest Rates**: Credit score-based interest calculation

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd Bitcoin-Backed-Microloan-DAO
clarinet check
```

## 🎯 Usage Guide

### 1. 💵 Contributing to Pool

```clarity
(contract-call? .Bitcoin-Backed-Microloan-DAO contribute-to-pool u1000000)
```

### 2. 📋 Applying for Loan

```clarity
(contract-call? .Bitcoin-Backed-Microloan-DAO apply-for-loan 
    u500000                    ;; loan amount
    u750000                    ;; collateral (150% minimum)
    "Coffee shop expansion"     ;; business description
    u52560)                    ;; duration in blocks (~1 year)
```

### 3. 🗳️ Voting on Applications

```clarity
(contract-call? .Bitcoin-Backed-Microloan-DAO vote-on-application u1 true)
```

### 4. ✅ Finalizing Applications

```clarity
(contract-call? .Bitcoin-Backed-Microloan-DAO finalize-loan-application u1)
```

### 5. 💳 Repaying Loans

```clarity
(contract-call? .Bitcoin-Backed-Microloan-DAO repay-loan u1 u100000)
```

## 📊 Credit Score System

| Score Range | Interest Rate | Description |
|-------------|---------------|-------------|
| 800-1000    | 8%           | 🌟 Excellent |
| 600-799     | 12%          | 👍 Good |
| 100-599     | 18%          | ⚠️
