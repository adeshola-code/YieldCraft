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