;; title: YieldCraft Protocol Aggregator
;; summary: A smart contract for aggregating multiple yield protocols and managing user deposits.
;; description: 
;; The YieldCraft Protocol Aggregator allows users to deposit assets into the best yielding protocols, 
;; manage their deposits, and claim rewards. It supports adding new protocols, updating protocol stats, 
;; and ensures secure and efficient asset management. The contract includes error handling, data 
;; variables, and maps to store protocol and user information. It also provides public functions for 
;; adding protocols, depositing and withdrawing assets, and claiming rewards, as well as read-only 
;; functions for retrieving protocol and user data.

(use-trait ft-trait .sip-010-trait.sip-010-trait)
(use-trait protocol-trait .protocol-trait.protocol-trait)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PROTOCOL (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-PROTOCOL-NOT-ACTIVE (err u104))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u105))
(define-constant ERR-MAX-PROTOCOLS-REACHED (err u106))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var protocol-count uint u0)
(define-data-var min-deposit uint u1000000) ;; in smallest units
(define-data-var max-slippage uint u50) ;; 0.5%
(define-data-var platform-fee uint u10) ;; 0.1%

;; Data maps
(define-map protocols
    uint 
    {
        protocol-address: principal,
        is-active: bool,
        tvl: uint,
        apy: uint,
        protocol-type: (string-ascii 20)
    }
)

(define-map user-deposits
    { user: principal, protocol-id: uint }
    {
        amount: uint,
        rewards: uint,
        deposit-height: uint,
        last-claim: uint
    }
)

(define-map protocol-assets
    { protocol-id: uint, token: principal }
    {
        balance: uint,
        lending-rate: uint,
        borrowing-rate: uint,
        utilization: uint
    }
)


;; Public functions

;; Add a new protocol to the aggregator
(define-public (add-protocol 
    (protocol-address principal)
    (protocol-type (string-ascii 20)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (< (var-get protocol-count) u50) ERR-MAX-PROTOCOLS-REACHED)
        
        (let ((new-id (+ (var-get protocol-count) u1)))
            (map-set protocols new-id {
                protocol-address: protocol-address,
                is-active: true,
                tvl: u0,
                apy: u0,
                protocol-type: protocol-type
            })
            (var-set protocol-count new-id)
            (ok new-id))))

;; Deposit assets into the best yielding protocol
(define-public (smart-deposit 
    (token-contract <ft-trait>)
    (amount uint))
    (let (
        (best-protocol (get-best-protocol token-contract))
        (user tx-sender)
        (current-block block-height)
    )
        (asserts! (>= amount (var-get min-deposit)) ERR-INVALID-AMOUNT)
        (asserts! (is-protocol-active best-protocol) ERR-PROTOCOL-NOT-ACTIVE)
        
        ;; Transfer tokens to the contract
        (try! (contract-call? token-contract transfer amount user (as-contract tx-sender) none))
        
        ;; Update user deposits
        (map-set user-deposits 
            { user: user, protocol-id: best-protocol }
            {
                amount: (+ (get-user-deposit user best-protocol) amount),
                rewards: u0,
                deposit-height: current-block,
                last-claim: current-block
            })
        
        ;; Update protocol TVL
        (update-protocol-tvl best-protocol amount true)
        
        (ok best-protocol)))

;; Withdraw assets from a protocol
(define-public (withdraw 
    (token-contract <ft-trait>)
    (protocol-id uint)
    (amount uint))
    (let (
        (user tx-sender)
        (user-deposit (get-user-deposit user protocol-id))
    )
        (asserts! (>= user-deposit amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-protocol-active protocol-id) ERR-PROTOCOL-NOT-ACTIVE)
        
        ;; Calculate fees and rewards
        (let (
            (fee (calculate-fee amount))
            (rewards (calculate-rewards user protocol-id))
            (net-amount (- amount fee))
        )
            ;; Transfer tokens back to user
            (try! (as-contract (contract-call? token-contract transfer 
                net-amount 
                (as-contract tx-sender) 
                user 
                none)))
            
            ;; Update user deposits
            (map-set user-deposits 
                { user: user, protocol-id: protocol-id }
                {
                    amount: (- user-deposit amount),
                    rewards: rewards,
                    deposit-height: (get-deposit-height user protocol-id),
                    last-claim: block-height
                })
            
            ;; Update protocol TVL
            (update-protocol-tvl protocol-id amount false)
            
            (ok net-amount))))

;; Claim rewards from a protocol
(define-public (claim-rewards
    (protocol-id uint))
    (let (
        (user tx-sender)
        (rewards (calculate-rewards user protocol-id))
    )
        (asserts! (> rewards u0) ERR-INVALID-AMOUNT)
        (asserts! (is-protocol-active protocol-id) ERR-PROTOCOL-NOT-ACTIVE)
        
        ;; Update user rewards
        (map-set user-deposits 
            { user: user, protocol-id: protocol-id }
            {
                amount: (get-user-deposit user protocol-id),
                rewards: u0,
                deposit-height: (get-deposit-height user protocol-id),
                last-claim: block-height
            })
            
        (ok rewards)))


;; Update protocol rates and stats
(define-public (update-protocol-stats
    (protocol-id uint)
    (new-apy uint)
    (new-tvl uint))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-protocol-active protocol-id) ERR-PROTOCOL-NOT-ACTIVE)
        
        (map-set protocols protocol-id
            (merge (unwrap-panic (map-get? protocols protocol-id))
                {
                    apy: new-apy,
                    tvl: new-tvl
                }))
        (ok true)))