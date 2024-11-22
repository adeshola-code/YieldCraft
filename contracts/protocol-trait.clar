(define-trait protocol-trait
    (
        ;; Deposit tokens into the protocol
        (deposit (uint) (response uint uint))

        ;; Withdraw tokens from the protocol
        (withdraw (uint) (response uint uint))

        ;; Get current APY
        (get-apy () (response uint uint))

        ;; Get protocol TVL
        (get-tvl () (response uint uint))

        ;; Get protocol type
        (get-protocol-type () (response (string-ascii 20) uint))
    )
)