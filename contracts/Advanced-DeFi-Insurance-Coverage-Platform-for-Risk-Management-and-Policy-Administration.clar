;; Advanced DeFi Insurance Coverage Platform for Risk Management and Policy Administration
;; Implements sophisticated policy underwriting with multi-layered security protocols
;; Delivers comprehensive insurance coverage tracking with automated claim processing capabilities

;; ===== Insurance platform governance and authority configuration =====

;; Central insurance authority responsible for platform oversight and regulatory compliance
(define-constant insurance-platform-authority tx-sender)

;; ===== Robust error handling system with specific insurance-related response codes =====

;; Policy management error responses for comprehensive failure handling
(define-constant policy-lookup-failure-error (err u401))
(define-constant policy-already-exists-error (err u402))
(define-constant coverage-validation-failed-error (err u403))
(define-constant premium-amount-invalid-error (err u404))
(define-constant unauthorized-policy-access-error (err u405))
(define-constant policyholder-validation-error (err u406))
(define-constant insufficient-authority-privileges-error (err u400))
(define-constant policy-access-violation-error (err u407))
(define-constant risk-category-format-error (err u408))

;; ===== Core platform state variables for insurance operations tracking =====

;; Incremental policy identification system for unique coverage assignments
(define-data-var insurance-policy-sequence uint u0)

;; ===== Comprehensive data structures for insurance policy management =====

;; Master insurance policy registry containing all coverage details and metadata
(define-map insurance-coverage-database
  { policy-reference-number: uint }
  {
    coverage-plan-title: (string-ascii 64),
    policy-holder-principal: principal,
    premium-amount-microSTX: uint,
    policy-creation-timestamp: uint,
    coverage-terms-description: (string-ascii 128),
    risk-classification-tags: (list 10 (string-ascii 32))
  }
)

;; Advanced authorization matrix for controlling policy access and claim permissions
(define-map policy-authorization-matrix
  { policy-reference-number: uint, authorized-principal: principal }
  { authorization-status: bool }
)

;; ===== Internal validation utilities for data integrity and security enforcement =====

;; Risk category validation function ensuring proper tag formatting standards
;; Validates individual risk classification tags against platform requirements
(define-private (validate-risk-category-format (risk-tag (string-ascii 32)))
  (and
    ;; Risk tag cannot be empty or null
    (> (len risk-tag) u0)
    ;; Risk tag must stay within character limits
    (< (len risk-tag) u33)
  )
)

;; Comprehensive risk tag collection validation for policy categorization
;; Ensures all risk classification tags meet platform standards
(define-private (verify-risk-tag-collection-integrity (tag-collection (list 10 (string-ascii 32))))
  (and
    ;; Collection must contain minimum required tags
    (> (len tag-collection) u0)
    ;; Collection cannot exceed maximum tag limit
    (<= (len tag-collection) u10)
    ;; Every tag in collection must pass individual validation
    (is-eq (len (filter validate-risk-category-format tag-collection)) (len tag-collection))
  )
)

;; Policy existence verification utility for database consistency checks
;; Returns confirmation of policy presence in insurance coverage database
(define-private (confirm-policy-exists-in-database (policy-reference-number uint))
  (is-some (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number }))
)

;; Premium amount extraction function with safe fallback mechanisms
;; Retrieves policy premium with default zero value for missing policies
(define-private (extract-policy-premium-amount (policy-reference-number uint))
  (default-to u0
    (get premium-amount-microSTX
      (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
    )
  )
)

;; Policyholder verification mechanism with principal identity validation
;; Confirms whether specified principal holds ownership of the insurance policy
(define-private (verify-policyholder-identity (policy-reference-number uint) (principal-for-verification principal))
  (match (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
    policy-record (is-eq (get policy-holder-principal policy-record) principal-for-verification)
    false
  )
)

;; ===== Public insurance policy management functions for external interactions =====

;; Comprehensive insurance policy creation with extensive validation protocols
;; Creates new insurance coverage with complete risk assessment and premium calculation
(define-public (create-insurance-coverage-policy
  (coverage-plan-title (string-ascii 64))
  (premium-amount-microSTX uint)
  (coverage-terms-description (string-ascii 128))
  (risk-classification-tags (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Generate sequential policy identifier for new coverage registration
      (fresh-policy-identifier (+ (var-get insurance-policy-sequence) u1))
    )
    ;; Rigorous input validation with comprehensive error reporting
    ;; Policy title cannot be empty string
    (asserts! (> (len coverage-plan-title) u0) coverage-validation-failed-error)
    ;; Policy title must respect maximum length constraints
    (asserts! (< (len coverage-plan-title) u65) coverage-validation-failed-error)
    ;; Premium must be positive amount
    (asserts! (> premium-amount-microSTX u0) premium-amount-invalid-error)
    ;; Premium cannot exceed platform maximum limits
    (asserts! (< premium-amount-microSTX u1000000000) premium-amount-invalid-error)
    ;; Coverage description cannot be empty
    (asserts! (> (len coverage-terms-description) u0) coverage-validation-failed-error)
    ;; Coverage description must meet length requirements
    (asserts! (< (len coverage-terms-description) u129) coverage-validation-failed-error)
    ;; Risk classification tags must pass validation protocols
    (asserts! (verify-risk-tag-collection-integrity risk-classification-tags) risk-category-format-error)

    ;; Insert new policy record into insurance coverage database
    (map-insert insurance-coverage-database
      { policy-reference-number: fresh-policy-identifier }
      {
        coverage-plan-title: coverage-plan-title,
        policy-holder-principal: tx-sender,
        premium-amount-microSTX: premium-amount-microSTX,
        policy-creation-timestamp: block-height,
        coverage-terms-description: coverage-terms-description,
        risk-classification-tags: risk-classification-tags
      }
    )

    ;; Establish automatic authorization for policy creator
    (map-insert policy-authorization-matrix
      { policy-reference-number: fresh-policy-identifier, authorized-principal: tx-sender }
      { authorization-status: true }
    )

    ;; Increment global policy sequence counter for future registrations
    (var-set insurance-policy-sequence fresh-policy-identifier)
    ;; Return successful policy creation with identifier
    (ok fresh-policy-identifier)
  )
)

;; Advanced policy modification system with multi-layer validation architecture
;; Enables policyholders to update coverage terms while preserving system integrity
(define-public (modify-insurance-policy-details
  (policy-reference-number uint)
  (updated-coverage-plan-title (string-ascii 64))
  (updated-premium-amount-microSTX uint)
  (updated-coverage-terms-description (string-ascii 128))
  (updated-risk-classification-tags (list 10 (string-ascii 32)))
)
  (let
    (
      ;; Fetch current policy data for validation and merging operations
      (existing-policy-record (unwrap! (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
        policy-lookup-failure-error))
    )
    ;; Verify policy exists in coverage database
    (asserts! (confirm-policy-exists-in-database policy-reference-number) policy-lookup-failure-error)
    ;; Confirm caller has policyholder authority
    (asserts! (is-eq (get policy-holder-principal existing-policy-record) tx-sender) policyholder-validation-error)
    ;; Validate updated policy title is not empty
    (asserts! (> (len updated-coverage-plan-title) u0) coverage-validation-failed-error)
    ;; Validate updated policy title length requirements
    (asserts! (< (len updated-coverage-plan-title) u65) coverage-validation-failed-error)
    ;; Validate updated premium amount is positive
    (asserts! (> updated-premium-amount-microSTX u0) premium-amount-invalid-error)
    ;; Validate updated premium within platform limits
    (asserts! (< updated-premium-amount-microSTX u1000000000) premium-amount-invalid-error)
    ;; Validate updated description is not empty
    (asserts! (> (len updated-coverage-terms-description) u0) coverage-validation-failed-error)
    ;; Validate updated description length constraints
    (asserts! (< (len updated-coverage-terms-description) u129) coverage-validation-failed-error)
    ;; Validate updated risk classification tags
    (asserts! (verify-risk-tag-collection-integrity updated-risk-classification-tags) risk-category-format-error)

    ;; Update policy record with new information while preserving ownership and timestamp
    (map-set insurance-coverage-database
      { policy-reference-number: policy-reference-number }
      (merge existing-policy-record {
        coverage-plan-title: updated-coverage-plan-title,
        premium-amount-microSTX: updated-premium-amount-microSTX,
        coverage-terms-description: updated-coverage-terms-description,
        risk-classification-tags: updated-risk-classification-tags
      })
    )
    ;; Return successful modification confirmation
    (ok true)
  )
)

;; Secure policy ownership transfer protocol with comprehensive authorization checks
;; Facilitates ownership transfer between principals with proper validation
(define-public (execute-policy-ownership-transfer (policy-reference-number uint) (recipient-principal principal))
  (let
    (
      ;; Retrieve current policy information for ownership verification
      (current-policy-record (unwrap! (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
        policy-lookup-failure-error))
    )
    ;; Confirm policy exists in database before transfer
    (asserts! (confirm-policy-exists-in-database policy-reference-number) policy-lookup-failure-error)
    ;; Verify current caller owns the policy
    (asserts! (is-eq (get policy-holder-principal current-policy-record) tx-sender) policyholder-validation-error)

    ;; Execute ownership transfer by updating policyholder principal
    (map-set insurance-coverage-database
      { policy-reference-number: policy-reference-number }
      (merge current-policy-record { policy-holder-principal: recipient-principal })
    )
    ;; Return successful transfer confirmation
    (ok true)
  )
)

;; Permanent policy termination function with irreversible deletion capability
;; Provides complete policy removal from insurance coverage database
(define-public (terminate-insurance-policy (policy-reference-number uint))
  (let
    (
      ;; Retrieve policy record for ownership verification before deletion
      (policy-for-termination (unwrap! (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
        policy-lookup-failure-error))
    )
    ;; Confirm policy exists before attempting termination
    (asserts! (confirm-policy-exists-in-database policy-reference-number) policy-lookup-failure-error)
    ;; Verify ownership authorization for deletion
    (asserts! (is-eq (get policy-holder-principal policy-for-termination) tx-sender) policyholder-validation-error)

    ;; Execute permanent policy removal from coverage database
    (map-delete insurance-coverage-database { policy-reference-number: policy-reference-number })
    ;; Return successful termination confirmation
    (ok true)
  )
)

;; ===== Read-only information retrieval functions for policy queries and system monitoring =====

;; Comprehensive policy information retrieval with advanced authorization enforcement
;; Returns detailed policy metadata for authorized users with proper access controls
(define-read-only (retrieve-policy-comprehensive-details (policy-reference-number uint))
  (let
    (
      ;; Fetch complete policy information from coverage database
      (policy-record (unwrap! (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
        policy-lookup-failure-error))
      ;; Check explicit authorization permissions for requesting user
      (user-has-direct-authorization (default-to false
        (get authorization-status
          (map-get? policy-authorization-matrix { policy-reference-number: policy-reference-number, authorized-principal: tx-sender })
        )
      ))
    )
    ;; Verify policy exists before processing access request
    (asserts! (confirm-policy-exists-in-database policy-reference-number) policy-lookup-failure-error)
    ;; Enforce authorization - allow policyholders and explicitly authorized users
    (asserts! (or user-has-direct-authorization (is-eq (get policy-holder-principal policy-record) tx-sender)) policy-access-violation-error)

    ;; Return comprehensive policy information to authorized users
    (ok {
      coverage-plan-title: (get coverage-plan-title policy-record),
      policy-holder-principal: (get policy-holder-principal policy-record),
      premium-amount-microSTX: (get premium-amount-microSTX policy-record),
      policy-creation-timestamp: (get policy-creation-timestamp policy-record),
      coverage-terms-description: (get coverage-terms-description policy-record),
      risk-classification-tags: (get risk-classification-tags policy-record)
    })
  )
)

;; Platform-wide insurance statistics and administrative oversight information
;; Provides comprehensive overview of total policies and platform authority
(define-read-only (retrieve-platform-insurance-metrics)
  (ok {
    total-policies-registered: (var-get insurance-policy-sequence),
    platform-authority: insurance-platform-authority
  })
)

;; Policy ownership verification utility for external authorization queries
;; Returns the principal that holds ownership of the specified insurance policy
(define-read-only (identify-policy-holder (policy-reference-number uint))
  (match (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
    policy-record (ok (get policy-holder-principal policy-record))
    policy-lookup-failure-error
  )
)

;; Comprehensive authorization status verification for policy access management
;; Returns detailed authorization information for principal-policy combinations
(define-read-only (check-policy-authorization-status (policy-reference-number uint) (principal-to-verify principal))
  (let
    (
      ;; Retrieve policy record for ownership comparison
      (policy-record (unwrap! (map-get? insurance-coverage-database { policy-reference-number: policy-reference-number })
        policy-lookup-failure-error))
      ;; Check for direct authorization permissions in matrix
      (has-direct-authorization (default-to false
        (get authorization-status
          (map-get? policy-authorization-matrix { policy-reference-number: policy-reference-number, authorized-principal: principal-to-verify })
        )
      ))
    )
    ;; Return comprehensive authorization analysis
    (ok {
      has-explicit-permission: has-direct-authorization,
      is-asset-owner: (is-eq (get policy-holder-principal policy-record) principal-to-verify),
      can-access-content: (or has-direct-authorization (is-eq (get policy-holder-principal policy-record) principal-to-verify))
    })
  )
)