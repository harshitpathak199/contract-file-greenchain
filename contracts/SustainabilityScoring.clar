;; title: SustainabilityScoring
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; SustainabilityScoring Contract
;; ESG rating system for companies based on verifiable on-chain environmental data

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-company-not-found (err u101))
(define-constant err-invalid-score (err u102))
(define-constant err-company-already-exists (err u103))

;; Data structures
;; Company ESG data structure
(define-map companies 
  principal 
  {
    name: (string-ascii 64),
    carbon-emissions: uint,  ;; in tons CO2
    renewable-energy-percent: uint,  ;; percentage (0-100)
    waste-recycled-percent: uint,    ;; percentage (0-100)
    esg-score: uint,                 ;; calculated score (0-100)
    last-updated: uint               ;; block height
  })

;; Track total registered companies
(define-data-var total-companies uint u0)

;; Function 1: Register Company Environmental Data
;; Allows companies to submit their environmental metrics
(define-public (register-company-data 
  (company-name (string-ascii 64))
  (carbon-emissions uint)
  (renewable-energy-percent uint)
  (waste-recycled-percent uint))
  (let 
    (
      (company-principal tx-sender)
      (current-block stacks-block-height)
    )
    ;; Validate input data
    (asserts! (<= renewable-energy-percent u100) err-invalid-score)
    (asserts! (<= waste-recycled-percent u100) err-invalid-score)
    
    ;; Check if company already exists
    (asserts! (is-none (map-get? companies company-principal)) err-company-already-exists)
    
    ;; Calculate ESG score based on environmental metrics
    (let 
      (
        ;; Simple scoring algorithm:
        ;; - Lower carbon emissions = better (max 40 points)
        ;; - Higher renewable energy % = better (max 35 points) 
        ;; - Higher waste recycling % = better (max 25 points)
        (carbon-score (if (<= carbon-emissions u1000) u40 
                     (if (<= carbon-emissions u5000) u25
                     (if (<= carbon-emissions u10000) u15 u5))))
        (renewable-score (/ (* renewable-energy-percent u35) u100))
        (waste-score (/ (* waste-recycled-percent u25) u100))
        (total-esg-score (+ carbon-score (+ renewable-score waste-score)))
      )
      
      ;; Store company data
      (map-set companies company-principal
        {
          name: company-name,
          carbon-emissions: carbon-emissions,
          renewable-energy-percent: renewable-energy-percent,
          waste-recycled-percent: waste-recycled-percent,
          esg-score: total-esg-score,
          last-updated: current-block
        })
      
      ;; Increment total companies counter
      (var-set total-companies (+ (var-get total-companies) u1))
      
      (ok total-esg-score)
    )
  )
)

;; Function 2: Get Company ESG Rating
;; Returns the complete ESG profile and score for a company
(define-read-only (get-company-esg-rating (company-principal principal))
  (match (map-get? companies company-principal)
    company-data (ok company-data)
    (err err-company-not-found)
  )
)

;; Additional helper read-only functions

;; Get total registered companies
(define-read-only (get-total-companies)
  (ok (var-get total-companies))
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (ok contract-owner)
)