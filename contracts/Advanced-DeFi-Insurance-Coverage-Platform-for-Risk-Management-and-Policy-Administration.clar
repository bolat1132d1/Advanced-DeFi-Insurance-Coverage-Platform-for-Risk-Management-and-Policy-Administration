;; Decentralized Finance Insurance Protocol for Smart Contract Coverage
;; Implements automated claim processing with risk-based premium calculations
;; Provides comprehensive protection against DeFi protocol failures and exploits

;; ===== Protocol governance and administrative constants =====

;; Insurance pool administrator with emergency powers
(define-constant protocol-admin tx-sender)
(define-constant minimum-coverage-period u144) ;; 1 day in blocks
(define-constant maximum-coverage-period u1008000) ;; ~7 years in blocks
(define-constant base-premium-rate u1000) ;; 0.1% in basis points
(define-constant claim-investigation-period u1008) ;; 1 week in blocks

;; ===== Comprehensive error response system =====

(define-constant policy-not-active-error (err u501))
(define-constant insufficient-premium-payment-error (err u502))
(define-constant coverage-amount-invalid-error (err u503))
(define-constant claim-already-submitted-error (err u504))
(define-constant claim-investigation-ongoing-error (err u505))
(define-constant unauthorized-claim-processor-error (err u506))
(define-constant insufficient-pool-funds-error (err u507))
(define-constant policy-expired-error (err u508))
(define-constant invalid-protocol-address-error (err u509))
(define-constant claim-exceeds-coverage-error (err u510))

;; ===== Core insurance protocol state management =====

(define-data-var total-insurance-pool-balance uint u0)
(define-data-var active-policies-count uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var policy-id-counter uint u0)
(define-data-var claim-id-counter uint u0)

;; ===== Primary insurance data structures =====

;; Active insurance policies with coverage details
(define-map active-insurance-policies
  { policy-id: uint }
  {
    insured-protocol: principal,
    policy-holder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    policy-start-block: uint,
    policy-end-block: uint,
    risk-score: uint,
    is-active: bool
  }
)

;; Insurance claims tracking and processing
(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    incident-description: (string-ascii 256),
    claim-submission-block: uint,
    investigation-deadline: uint,
    claim-status: (string-ascii 20),
    approved-payout: uint
  }
)

;; Protocol risk assessments for premium calculation
(define-map protocol-risk-profiles
  { protocol-address: principal }
  {
    risk-category: (string-ascii 32),
    historical-incidents: uint,
    total-value-locked: uint,
    security-audit-score: uint,
    risk-multiplier: uint
  }
)

;; Authorized claim processors and investigators
(define-map authorized-claim-processors
  { processor: principal }
  { is-authorized: bool }
)

;; ===== Private utility functions for risk assessment and calculations =====

;; Calculate dynamic premium based on coverage amount and protocol risk
(define-private (calculate-premium-amount 
  (coverage-amount uint) 
  (coverage-period-blocks uint) 
  (risk-multiplier uint))
  (let
    (
      (base-premium (* coverage-amount base-premium-rate))
      (period-factor (/ coverage-period-blocks minimum-coverage-period))
      (risk-adjusted-premium (* base-premium risk-multiplier))
    )
    (/ (* risk-adjusted-premium period-factor) u10000)
  )
)

;; Validate protocol exists and has risk profile
(define-private (is-valid-insurable-protocol (protocol-address principal))
  (is-some (map-get? protocol-risk-profiles { protocol-address: protocol-address }))
)

;; Check if policy is currently active and valid
(define-private (is-policy-currently-active (policy-id uint))
  (match (map-get? active-insurance-policies { policy-id: policy-id })
    policy-data 
      (and 
        (get is-active policy-data)
        (>= block-height (get policy-start-block policy-data))
        (<= block-height (get policy-end-block policy-data))
      )
    false
  )
)

;; Verify sufficient pool funds for potential claim payout
(define-private (verify-pool-liquidity (required-amount uint))
  (>= (var-get total-insurance-pool-balance) required-amount)
)

;; ===== Administrative functions for protocol setup =====

;; Add new protocol to insurance coverage with risk assessment
(define-public (register-insurable-protocol
  (protocol-address principal)
  (risk-category (string-ascii 32))
  (security-audit-score uint)
  (estimated-tvl uint))
  (let
    (
      ;; Calculate risk multiplier based on audit score and category
      (risk-multiplier (if (<= security-audit-score u50) u300 u100))
    )
    ;; Only protocol admin can register new protocols
    (asserts! (is-eq tx-sender protocol-admin) unauthorized-claim-processor-error)
    ;; Audit score must be between 0-100
    (asserts! (<= security-audit-score u100) coverage-amount-invalid-error)
    ;; TVL must be positive
    (asserts! (> estimated-tvl u0) coverage-amount-invalid-error)

    ;; Register protocol with risk profile
    (map-set protocol-risk-profiles
      { protocol-address: protocol-address }
      {
        risk-category: risk-category,
        historical-incidents: u0,
        total-value-locked: estimated-tvl,
        security-audit-score: security-audit-score,
        risk-multiplier: risk-multiplier
      }
    )
    (ok true)
  )
)

;; Authorize claim processors for investigation and approval
(define-public (authorize-claim-processor (processor-principal principal))
  (begin
    ;; Only admin can authorize processors
    (asserts! (is-eq tx-sender protocol-admin) unauthorized-claim-processor-error)

    (map-set authorized-claim-processors
      { processor: processor-principal }
      { is-authorized: true }
    )
    (ok true)
  )
)

;; ===== Core insurance policy management functions =====

;; Purchase insurance coverage for specified DeFi protocol
(define-public (purchase-insurance-policy
  (protocol-to-insure principal)
  (desired-coverage-amount uint)
  (coverage-duration-blocks uint))
  (let
    (
      (new-policy-id (+ (var-get policy-id-counter) u1))
      (protocol-risk (unwrap! (map-get? protocol-risk-profiles { protocol-address: protocol-to-insure })
        invalid-protocol-address-error))
      (required-premium (calculate-premium-amount 
        desired-coverage-amount 
        coverage-duration-blocks 
        (get risk-multiplier protocol-risk)))
      (policy-end-block (+ block-height coverage-duration-blocks))
    )
    ;; Validate protocol is registered for insurance
    (asserts! (is-valid-insurable-protocol protocol-to-insure) invalid-protocol-address-error)
    ;; Coverage amount must be positive and reasonable
    (asserts! (and (> desired-coverage-amount u0) (< desired-coverage-amount u1000000000000)) coverage-amount-invalid-error)
    ;; Coverage period must be within acceptable range
    (asserts! (and (>= coverage-duration-blocks minimum-coverage-period) 
                   (<= coverage-duration-blocks maximum-coverage-period)) policy-expired-error)
    ;; Verify sufficient STX sent for premium payment
    (asserts! (>= (stx-get-balance tx-sender) required-premium) insufficient-premium-payment-error)

    ;; Transfer premium to insurance pool
    (try! (stx-transfer? required-premium tx-sender (as-contract tx-sender)))

    ;; Create new insurance policy
    (map-insert active-insurance-policies
      { policy-id: new-policy-id }
      {
        insured-protocol: protocol-to-insure,
        policy-holder: tx-sender,
        coverage-amount: desired-coverage-amount,
        premium-paid: required-premium,
        policy-start-block: block-height,
        policy-end-block: policy-end-block,
        risk-score: (get risk-multiplier protocol-risk),
        is-active: true
      }
    )

    ;; Update global counters and pool balance
    (var-set policy-id-counter new-policy-id)
    (var-set active-policies-count (+ (var-get active-policies-count) u1))
    (var-set total-insurance-pool-balance (+ (var-get total-insurance-pool-balance) required-premium))

    (ok new-policy-id)
  )
)

;; Cancel active insurance policy with partial premium refund
(define-public (cancel-insurance-policy (policy-id uint))
  (let
    (
      (policy-data (unwrap! (map-get? active-insurance-policies { policy-id: policy-id })
        policy-not-active-error))
      (remaining-blocks (- (get policy-end-block policy-data) block-height))
      (total-blocks (- (get policy-end-block policy-data) (get policy-start-block policy-data)))
      (refund-amount (/ (* (get premium-paid policy-data) remaining-blocks) total-blocks))
    )
    ;; Only policy holder can cancel
    (asserts! (is-eq tx-sender (get policy-holder policy-data)) unauthorized-claim-processor-error)
    ;; Policy must be currently active
    (asserts! (is-policy-currently-active policy-id) policy-not-active-error)
    ;; Must have remaining coverage time
    (asserts! (> remaining-blocks u0) policy-expired-error)

    ;; Deactivate policy
    (map-set active-insurance-policies
      { policy-id: policy-id }
      (merge policy-data { is-active: false })
    )

    ;; Process refund if applicable
    (if (> refund-amount u0)
      (begin
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get policy-holder policy-data))))
        (var-set total-insurance-pool-balance (- (var-get total-insurance-pool-balance) refund-amount))
      )
      true
    )

    (var-set active-policies-count (- (var-get active-policies-count) u1))
    (ok refund-amount)
  )
)

;; ===== Claims processing and payout system =====

;; Submit insurance claim for protocol incident or exploit
(define-public (submit-insurance-claim
  (policy-id uint)
  (claim-amount uint)
  (incident-description (string-ascii 256)))
  (let
    (
      (new-claim-id (+ (var-get claim-id-counter) u1))
      (policy-data (unwrap! (map-get? active-insurance-policies { policy-id: policy-id })
        policy-not-active-error))
      (investigation-deadline (+ block-height claim-investigation-period))
    )
    ;; Verify policy is active and valid
    (asserts! (is-policy-currently-active policy-id) policy-not-active-error)
    ;; Only policy holder can submit claims
    (asserts! (is-eq tx-sender (get policy-holder policy-data)) unauthorized-claim-processor-error)
    ;; Claim amount cannot exceed coverage
    (asserts! (<= claim-amount (get coverage-amount policy-data)) claim-exceeds-coverage-error)
    ;; Claim amount must be positive
    (asserts! (> claim-amount u0) coverage-amount-invalid-error)
    ;; Description cannot be empty
    (asserts! (> (len incident-description) u0) coverage-amount-invalid-error)

    ;; Create new claim record
    (map-insert insurance-claims
      { claim-id: new-claim-id }
      {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-amount: claim-amount,
        incident-description: incident-description,
        claim-submission-block: block-height,
        investigation-deadline: investigation-deadline,
        claim-status: "investigating",
        approved-payout: u0
      }
    )

    (var-set claim-id-counter new-claim-id)
    (ok new-claim-id)
  )
)

;; Process claim investigation and determine payout
(define-public (process-claim-decision
  (claim-id uint)
  (approved-amount uint)
  (final-status (string-ascii 20)))
  (let
    (
      (claim-data (unwrap! (map-get? insurance-claims { claim-id: claim-id })
        policy-not-active-error))
      (policy-data (unwrap! (map-get? active-insurance-policies { policy-id: (get policy-id claim-data) })
        policy-not-active-error))
      (is-authorized (default-to false 
        (get is-authorized (map-get? authorized-claim-processors { processor: tx-sender }))))
    )
    ;; Only authorized processors can make decisions
    (asserts! (or is-authorized (is-eq tx-sender protocol-admin)) unauthorized-claim-processor-error)
    ;; Investigation period must be complete
    (asserts! (>= block-height (get investigation-deadline claim-data)) claim-investigation-ongoing-error)
    ;; Approved amount cannot exceed original claim
    (asserts! (<= approved-amount (get claim-amount claim-data)) claim-exceeds-coverage-error)
    ;; Verify sufficient pool funds for payout
    (asserts! (verify-pool-liquidity approved-amount) insufficient-pool-funds-error)

    ;; Update claim with decision
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim-data {
        claim-status: final-status,
        approved-payout: approved-amount
      })
    )

    ;; Process payout if approved
    (if (and (> approved-amount u0) (is-eq final-status "approved"))
      (begin
        (try! (as-contract (stx-transfer? approved-amount tx-sender (get claimant claim-data))))
        (var-set total-insurance-pool-balance (- (var-get total-insurance-pool-balance) approved-amount))
        (var-set total-claims-paid (+ (var-get total-claims-paid) approved-amount))
      )
      true
    )

    (ok approved-amount)
  )
)

;; ===== Read-only functions for policy and claim information =====

;; Retrieve comprehensive policy information
(define-read-only (get-policy-details (policy-id uint))
  (ok (map-get? active-insurance-policies { policy-id: policy-id }))
)

;; Get current claim status and details
(define-read-only (get-claim-information (claim-id uint))
  (ok (map-get? insurance-claims { claim-id: claim-id }))
)

;; Calculate premium quote for potential insurance purchase
(define-read-only (get-premium-quote 
  (protocol-address principal) 
  (coverage-amount uint) 
  (coverage-blocks uint))
  (match (map-get? protocol-risk-profiles { protocol-address: protocol-address })
    risk-profile (ok (calculate-premium-amount coverage-amount coverage-blocks (get risk-multiplier risk-profile)))
    invalid-protocol-address-error
  )
)

;; Comprehensive insurance pool statistics
(define-read-only (get-insurance-pool-stats)
  (ok {
    total-pool-balance: (var-get total-insurance-pool-balance),
    active-policies: (var-get active-policies-count),
    total-claims-paid: (var-get total-claims-paid),
    total-policies-issued: (var-get policy-id-counter),
    total-claims-submitted: (var-get claim-id-counter)
  })
)

;; Check protocol risk profile and insurability
(define-read-only (get-protocol-risk-assessment (protocol-address principal))
  (ok (map-get? protocol-risk-profiles { protocol-address: protocol-address }))
)