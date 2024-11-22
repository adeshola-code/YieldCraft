;; title: YieldCraft Protocol Aggregator
;; summary: A smart contract for aggregating multiple yield protocols and managing user deposits.
;; description: 
;; The YieldCraft Protocol Aggregator allows users to deposit assets into the best yielding protocols, 
;; manage their deposits, and claim rewards. It supports adding new protocols, updating protocol stats, 
;; and ensures secure and efficient asset management. The contract includes error handling, data 
;; variables, and maps to store protocol and user information. It also provides public functions for 
;; adding protocols, depositing and withdrawing assets, and claiming rewards, as well as read-only 
;; functions for retrieving protocol and user data.

(impl-trait .protocol-trait.protocol-trait)

;; Define traits
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
(define-constant ERR-NO-ACTIVE-PROTOCOLS (err u107))

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

;; Implementation of protocol-trait
(define-public (deposit (amount uint))
    (ok amount))

(define-public (withdraw (amount uint))
    (ok amount))

(define-public (get-apy)
    (ok u0))

(define-public (get-tvl)
    (ok u0))

(define-public (get-protocol-type)
    (ok "DEFI-AGG"))

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
        (best-protocol-result (get-best-protocol token-contract))
        (user tx-sender)
    )
        (asserts! (>= amount (var-get min-deposit)) ERR-INVALID-AMOUNT)
        (asserts! (is-ok best-protocol-result) ERR-NO-ACTIVE-PROTOCOLS)
        
        (let (
            (best-protocol (unwrap-panic best-protocol-result))
            (current-block block-height)
        )
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
            
            (ok best-protocol))))

;; Withdraw assets from a protocol
(define-public (withdraw-from-protocol 
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

;; Read-only functions
;; Get protocol details
(define-read-only (get-protocol (protocol-id uint))
    (map-get? protocols protocol-id))

;; Check if protocol is active
(define-read-only (is-protocol-active (protocol-id uint))
    (default-to false
        (get is-active (map-get? protocols protocol-id))))

;; Get protocol APY
(define-read-only (get-protocol-apy (protocol-id uint))
    (default-to u0
        (get apy (map-get? protocols protocol-id))))


;; Compare two protocols and return the better one
(define-read-only (compare-protocols (protocol-a uint) (protocol-b uint))
    (let (
        (protocol-a-details (map-get? protocols protocol-a))
        (protocol-b-details (map-get? protocols protocol-b))
    )
        (if (and
                (is-some protocol-a-details)
                (is-some protocol-b-details)
                (get is-active (unwrap! protocol-a-details protocol-b))
                (get is-active (unwrap! protocol-b-details protocol-a))
            )
            (if (> (get apy (unwrap! protocol-a-details protocol-b))
                   (get apy (unwrap! protocol-b-details protocol-a)))
                protocol-a
                protocol-b)
            (if (is-some protocol-a-details)
                (if (get is-active (unwrap! protocol-a-details protocol-b))
                    protocol-a
                    protocol-b)
                protocol-b))))

;; Find best protocol - now independent of other functions
(define-read-only (find-best-protocol (start uint) (end uint))
    (let ((current-best start))
        (filter-protocols start end current-best)))

;; Get the best protocol based on APY and TVL
(define-read-only (get-best-protocol (token-contract <ft-trait>))
    (let (
        (count (var-get protocol-count))
        (best-id (get-best-protocol-id u1 count))
    )
    (if (> count u0)
        (ok best-id)
        (err ERR-NO-ACTIVE-PROTOCOLS))))

;; Generate a sequence of numbers for fold
(define-read-only (generate-sequence (start uint) (end uint))
    (list start))

;; Helper function to get the best protocol ID
(define-read-only (get-best-protocol-id (start uint) (end uint))
    (let (
        (result (fold check-protocol 
            (generate-sequence start end)
            {
                id: u0,
                apy: u0
            }
        )))
        (get id result)))

;; Get user deposit in a protocol
(define-read-only (get-user-deposit (user principal) (protocol-id uint))
    (default-to u0
        (get amount (map-get? user-deposits { user: user, protocol-id: protocol-id }))))

;; Get deposit block height
(define-read-only (get-deposit-height (user principal) (protocol-id uint))
    (default-to u0
        (get deposit-height (map-get? user-deposits { user: user, protocol-id: protocol-id }))))

;; Private functions

;; Check each protocol and track the best one
(define-private (check-protocol (current uint) (best {id: uint, apy: uint}))
    (let (
        (current-protocol (map-get? protocols current))
    )
        (if (and
                (is-some current-protocol)
                (get is-active (unwrap-panic current-protocol))
                (> (get apy (unwrap-panic current-protocol)) (get apy best))
            )
            {
                id: current,
                apy: (get apy (unwrap-panic current-protocol))
            }
            best)))
;; Helper function to iterate through protocols
(define-private (filter-protocols (current uint) (max uint) (best-so-far uint))
    (if (> current max)
        best-so-far
        (filter-protocols 
            (+ current u1) 
            max 
            (compare-protocols current best-so-far))))

;; Find the best protocol by iterating through all protocols
(define-private (find-best-protocol (current uint) (max uint) (best-so-far uint))
    (if (> current max)
        best-so-far
        (let (
            (current-protocol (unwrap-panic (map-get? protocols current)))
            (best-protocol (unwrap-panic (map-get? protocols best-so-far)))
        )
            (if (and
                    (get is-active current-protocol)
                    (> (get apy current-protocol) (get apy best-protocol)))
                (find-best-protocol (+ current u1) max current)
                (find-best-protocol (+ current u1) max best-so-far)))))

;; Calculate rewards based on deposit amount and time
(define-private (calculate-rewards (user principal) (protocol-id uint))
    (let (
        (deposit (get-user-deposit user protocol-id))
        (deposit-height (get-deposit-height user protocol-id))
        (protocol (unwrap-panic (map-get? protocols protocol-id)))
        (blocks-elapsed (- block-height deposit-height))
    )
        (if (or (is-eq deposit u0) (is-eq blocks-elapsed u0))
            u0
            (/ (* deposit (* blocks-elapsed (get apy protocol))) u10000))))

;; Calculate platform fee
(define-private (calculate-fee (amount uint))
    (/ (* amount (var-get platform-fee)) u10000))

;; Update protocol TVL
(define-private (update-protocol-tvl (protocol-id uint) (amount uint) (is-deposit bool))
    (let ((protocol (unwrap-panic (map-get? protocols protocol-id))))
        (map-set protocols protocol-id
            (merge protocol
                {
                    tvl: (if is-deposit
                            (+ (get tvl protocol) amount)
                            (- (get tvl protocol) amount))
                }))))

;; Check if protocol is active
(define-private (is-protocol-active (protocol-id uint))
    (default-to false
        (get is-active (map-get? protocols protocol-id))))

;; Check if sender is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner)))

;; Initialize contract
(begin
    (var-set contract-owner tx-sender)
    (var-set protocol-count u0)
    (var-set min-deposit u1000000)
    (var-set max-slippage u50)
    (var-set platform-fee u10))