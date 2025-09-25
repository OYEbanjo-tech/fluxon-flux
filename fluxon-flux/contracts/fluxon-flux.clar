;; FluxonFlux - Social Impact Platform Smart Contract
;; A revolutionary platform for fair trade verification through dynamic impact scoring

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-unauthorized (err u105))

;; Define SIP-010 fungible token trait for Impact Flux Tokens (IFT)
(define-fungible-token impact-flux-token)

;; Producer data structure
(define-map producers
  { producer-id: principal }
  {
    name: (string-ascii 64),
    location: (string-ascii 64),
    impact-score: uint,
    total-tokens: uint,
    water-conservation: uint,
    fair-wage-score: uint,
    biodiversity-score: uint,
    verified: bool,
    registration-block: uint
  }
)

;; Impact thresholds for rewards
(define-map impact-thresholds
  { threshold-type: (string-ascii 32) }
  { min-score: uint, reward-multiplier: uint }
)

;; Consumer purchase tracking
(define-map purchases
  { purchase-id: uint }
  {
    consumer: principal,
    producer: principal,
    amount: uint,
    impact-tokens: uint,
    block-height: uint
  }
)

;; Global variables
(define-data-var next-purchase-id uint u1)
(define-data-var total-impact-score uint u0)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points

;; Initialize impact thresholds
(map-set impact-thresholds 
  { threshold-type: "water-conservation" }
  { min-score: u70, reward-multiplier: u150 })

(map-set impact-thresholds 
  { threshold-type: "fair-wage" }
  { min-score: u80, reward-multiplier: u120 })

(map-set impact-thresholds 
  { threshold-type: "biodiversity" }
  { min-score: u75, reward-multiplier: u130 })

;; Register a new producer
(define-public (register-producer (name (string-ascii 64)) (location (string-ascii 64)))
  (let ((producer tx-sender))
    (asserts! (is-none (map-get? producers { producer-id: producer })) err-already-exists)
    (map-set producers
      { producer-id: producer }
      {
        name: name,
        location: location,
        impact-score: u0,
        total-tokens: u0,
        water-conservation: u0,
        fair-wage-score: u0,
        biodiversity-score: u0,
        verified: false,
        registration-block: block-height
      }
    )
    (ok producer)
  )
)

;; Update impact data (only contract owner for now - in real implementation this would be validators)
(define-public (update-impact-data 
  (producer principal) 
  (water-score uint) 
  (wage-score uint) 
  (bio-score uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (match (map-get? producers { producer-id: producer })
      producer-data
      (let ((new-impact-score (calculate-total-impact-score water-score wage-score bio-score)))
        (map-set producers
          { producer-id: producer }
          (merge producer-data {
            water-conservation: water-score,
            fair-wage-score: wage-score,
            biodiversity-score: bio-score,
            impact-score: new-impact-score,
            verified: (> new-impact-score u50)
          })
        )
        ;; Mint Impact Flux Tokens based on impact score
        (mint-impact-tokens producer new-impact-score)
      )
      err-not-found
    )
  )
)

;; Calculate total impact score (weighted average)
(define-private (calculate-total-impact-score (water uint) (wage uint) (bio uint))
  (/ (+ (* water u3) (* wage u4) (* bio u3)) u10)
)

;; Mint Impact Flux Tokens based on impact score
(define-private (mint-impact-tokens (producer principal) (impact-score uint))
  (let ((tokens-to-mint (* impact-score u100))) ;; 100 tokens per impact point
    (match (ft-mint? impact-flux-token tokens-to-mint producer)
      success
      (begin
        (match (map-get? producers { producer-id: producer })
          producer-data
          (map-set producers
            { producer-id: producer }
            (merge producer-data { total-tokens: (+ (get total-tokens producer-data) tokens-to-mint) })
          )
          false
        )
        (var-set total-impact-score (+ (var-get total-impact-score) impact-score))
        (ok tokens-to-mint)
      )
      error (err error)
    )
  )
)

;; Consumer purchase function
(define-public (make-purchase (producer principal) (amount uint))
  (let (
    (purchase-id (var-get next-purchase-id))
    (consumer tx-sender)
  )
    (asserts! (> amount u0) err-invalid-amount)
    (match (map-get? producers { producer-id: producer })
      producer-data
      (let (
        (impact-tokens (* (get impact-score producer-data) u10))
        (platform-fee (/ (* amount (var-get platform-fee-rate)) u10000))
        (producer-payment (- amount platform-fee))
      )
        ;; Record the purchase
        (map-set purchases
          { purchase-id: purchase-id }
          {
            consumer: consumer,
            producer: producer,
            amount: amount,
            impact-tokens: impact-tokens,
            block-height: block-height
          }
        )
        (var-set next-purchase-id (+ purchase-id u1))
        
        ;; Transfer STX payment to producer (simplified - in real implementation would handle multiple tokens)
        (try! (stx-transfer? producer-payment consumer producer))
        
        ;; Award impact tokens to consumer
        (try! (ft-transfer? impact-flux-token impact-tokens producer consumer))
        
        (ok { purchase-id: purchase-id, impact-tokens: impact-tokens })
      )
      err-not-found
    )
  )
)

;; Distribute premium rewards when thresholds are met
(define-public (distribute-premium-rewards (producer principal))
  (match (map-get? producers { producer-id: producer })
    producer-data
    (let (
      (water-score (get water-conservation producer-data))
      (wage-score (get fair-wage-score producer-data))
      (bio-score (get biodiversity-score producer-data))
    )
      (let ((water-reward (calculate-threshold-reward "water-conservation" water-score))
            (wage-reward (calculate-threshold-reward "fair-wage" wage-score))
            (bio-reward (calculate-threshold-reward "biodiversity" bio-score)))
        (let ((total-bonus (+ (+ water-reward wage-reward) bio-reward)))
          (if (> total-bonus u0)
            (match (ft-mint? impact-flux-token total-bonus producer)
              success (ok total-bonus)
              error (err error)
            )
            (ok u0)
          )
        )
      )
    )
    err-not-found
  )
)

;; Calculate reward based on threshold achievement
(define-private (calculate-threshold-reward (threshold-type (string-ascii 32)) (score uint))
  (match (map-get? impact-thresholds { threshold-type: threshold-type })
    threshold-data
    (if (>= score (get min-score threshold-data))
      (* score (get reward-multiplier threshold-data))
      u0
    )
    u0
  )
)

;; Read-only functions

;; Get producer information
(define-read-only (get-producer-info (producer principal))
  (map-get? producers { producer-id: producer })
)

;; Get purchase information
(define-read-only (get-purchase-info (purchase-id uint))
  (map-get? purchases { purchase-id: purchase-id })
)

;; Get Impact Flux Token balance
(define-read-only (get-ift-balance (account principal))
  (ft-get-balance impact-flux-token account)
)

;; Get total supply of Impact Flux Tokens
(define-read-only (get-ift-total-supply)
  (ft-get-supply impact-flux-token)
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-impact-score: (var-get total-impact-score),
    total-ift-supply: (ft-get-supply impact-flux-token),
    next-purchase-id: (var-get next-purchase-id),
    platform-fee-rate: (var-get platform-fee-rate)
  }
)

;; Administrative functions

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-fee-rate)
    (ok new-fee-rate)
  )
)

;; Update impact threshold (owner only)
(define-public (update-impact-threshold 
  (threshold-type (string-ascii 32)) 
  (min-score uint) 
  (reward-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set impact-thresholds
      { threshold-type: threshold-type }
      { min-score: min-score, reward-multiplier: reward-multiplier }
    )
    (ok true)
  )
)