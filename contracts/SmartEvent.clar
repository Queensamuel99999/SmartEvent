;; title: SmartEvent
;; version: 1.0
;; summary: Event ticketing and management platform using NFTs
;; description: Allows event organizers to create token-gated events with tiered access,
;; issue NFT tickets, track attendance, and control ticket resales.

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EVENT-NOT-FOUND (err u101))
(define-constant ERR-TICKET-NOT-FOUND (err u102))
(define-constant ERR-TICKET-ALREADY-USED (err u103))
(define-constant ERR-RESALE-NOT-ALLOWED (err u104))
(define-constant ERR-INVALID-PRICE (err u105))
(define-constant ERR-EVENT-ENDED (err u106))
(define-constant ERR-SOLD-OUT (err u107))

;; Data maps
;; Event data structure
(define-map events
  { event-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    organizer: principal,
    date: uint,
    venue: (string-ascii 100),
    max-tickets: uint,
    tickets-sold: uint,
    allow-resale: bool,
    max-resale-price: uint,
    ended: bool
  }
)

;; Ticket tiers with different prices and benefits
(define-map ticket-tiers
  { event-id: uint, tier-id: uint }
  {
    name: (string-ascii 50),
    price: uint,
    max-supply: uint,
    sold: uint,
    benefits: (string-ascii 200)
  }
)

;; Tickets issued as NFTs
(define-map tickets
  { event-id: uint, ticket-id: uint }
  {
    owner: principal,
    tier-id: uint,
    used: bool,
    for-sale: bool,
    sale-price: uint
  }
)

;; Track attendance
(define-map attendance
  { event-id: uint }
  { attendees: (list 100 principal) }
)

;; Data vars
(define-data-var last-event-id uint u0)
(define-data-var last-ticket-id uint u0)

;; Public functions

;; Create a new event
(define-public (create-event 
                (name (string-ascii 100)) 
                (description (string-ascii 500))
                (date uint)
                (venue (string-ascii 100))
                (max-tickets uint)
                (allow-resale bool)
                (max-resale-price uint))
  (let ((event-id (+ (var-get last-event-id) u1)))
    (map-set events
      { event-id: event-id }
      {
        name: name,
        description: description,
        organizer: tx-sender,
        date: date,
        venue: venue,
        max-tickets: max-tickets,
        tickets-sold: u0,
        allow-resale: allow-resale,
        max-resale-price: max-resale-price,
        ended: false
      }
    )
    (var-set last-event-id event-id)
    (ok event-id)
  )
)



;; Private functions
(define-private (find-last-tier-recursive (event-id uint) (current-tier-id uint))
  (if (is-some (map-get? ticket-tiers { event-id: event-id, tier-id: current-tier-id }))
    (some (- current-tier-id u1))
    none
  )
)

(define-private (find-last-tier (event-id uint) (current-tier-id uint))
  (if (is-some (map-get? ticket-tiers { event-id: event-id, tier-id: current-tier-id }))
    (find-last-tier-recursive event-id (+ current-tier-id u1))
    (some (- current-tier-id u1))
  )
)


;; Helper to find the last tier ID for an event
(define-private (get-last-tier-id (event-id uint))
  (let ((tier-id u1))
    (find-last-tier event-id tier-id)
  )
)



;; Add a ticket tier to an event
(define-public (add-ticket-tier
                (event-id uint)
                (tier-name (string-ascii 50))
                (price uint)
                (max-supply uint)
                (benefits (string-ascii 200)))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get ended event)) ERR-EVENT-ENDED)
    
    (let ((tier-id (default-to u0 (get-last-tier-id event-id))))
      (map-set ticket-tiers
        { event-id: event-id, tier-id: (+ tier-id u1) }
        {
          name: tier-name,
          price: price,
          max-supply: max-supply,
          sold: u0,
          benefits: benefits
        }
      )
      (ok (+ tier-id u1))
    )
  )
)

;; Purchase a ticket
(define-public (purchase-ticket (event-id uint) (tier-id uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (tier (unwrap! (map-get? ticket-tiers { event-id: event-id, tier-id: tier-id }) ERR-TICKET-NOT-FOUND))
  )
    (asserts! (not (get ended event)) ERR-EVENT-ENDED)
    (asserts! (< (get tickets-sold event) (get max-tickets event)) ERR-SOLD-OUT)
    (asserts! (< (get sold tier) (get max-supply tier)) ERR-SOLD-OUT)
    
    ;; Update ticket tier sold count
    (map-set ticket-tiers
      { event-id: event-id, tier-id: tier-id }
      (merge tier { sold: (+ (get sold tier) u1) })
    )
    
    ;; Update event tickets sold
    (map-set events
      { event-id: event-id }
      (merge event { tickets-sold: (+ (get tickets-sold event) u1) })
    )
    
    ;; Create new ticket
    (let ((ticket-id (+ (var-get last-ticket-id) u1)))
      (map-set tickets
        { event-id: event-id, ticket-id: ticket-id }
        {
          owner: tx-sender,
          tier-id: tier-id,
          used: false,
          for-sale: false,
          sale-price: u0
        }
      )
      (var-set last-ticket-id ticket-id)
      (ok ticket-id)
    )
  )
)

;; Mark attendance at an event
(define-public (check-in (event-id uint) (ticket-id uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (ticket (unwrap! (map-get? tickets { event-id: event-id, ticket-id: ticket-id }) ERR-TICKET-NOT-FOUND))
  )
    ;; Only the organizer can check people in
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get used ticket)) ERR-TICKET-ALREADY-USED)
    
    ;; Mark ticket as used
    (map-set tickets
      { event-id: event-id, ticket-id: ticket-id }
      (merge ticket { used: true })
    )
    
    ;; Add to attendance
    (let ((current-attendance (default-to { attendees: (list) } (map-get? attendance { event-id: event-id }))))
      (map-set attendance
        { event-id: event-id }
        { attendees: (unwrap! (as-max-len? (append (get attendees current-attendance) (get owner ticket)) u100) ERR-NOT-AUTHORIZED) }
      )
      (ok true)
    )
  )
)

;; List a ticket for resale
(define-public (list-ticket-for-sale (event-id uint) (ticket-id uint) (price uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (ticket (unwrap! (map-get? tickets { event-id: event-id, ticket-id: ticket-id }) ERR-TICKET-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get owner ticket)) ERR-NOT-AUTHORIZED)
    (asserts! (get allow-resale event) ERR-RESALE-NOT-ALLOWED)
    (asserts! (<= price (get max-resale-price event)) ERR-INVALID-PRICE)
    (asserts! (not (get used ticket)) ERR-TICKET-ALREADY-USED)
    (asserts! (not (get ended event)) ERR-EVENT-ENDED)
    
    (map-set tickets
      { event-id: event-id, ticket-id: ticket-id }
      (merge ticket { for-sale: true, sale-price: price })
    )
    (ok true)
  )
)

;; Purchase a resale ticket
(define-public (buy-resale-ticket (event-id uint) (ticket-id uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (ticket (unwrap! (map-get? tickets { event-id: event-id, ticket-id: ticket-id }) ERR-TICKET-NOT-FOUND))
  )
    (asserts! (get for-sale ticket) ERR-NOT-AUTHORIZED)
    (asserts! (not (get used ticket)) ERR-TICKET-ALREADY-USED)
    (asserts! (not (get ended event)) ERR-EVENT-ENDED)
    
    ;; Transfer ownership
    (map-set tickets
      { event-id: event-id, ticket-id: ticket-id }
      (merge ticket { owner: tx-sender, for-sale: false, sale-price: u0 })
    )
    (ok true)
  )
)

;; End an event
(define-public (end-event (event-id uint))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    
    (map-set events
      { event-id: event-id }
      (merge event { ended: true })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get event details
(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

;; Get ticket tier details
(define-read-only (get-ticket-tier (event-id uint) (tier-id uint))
  (map-get? ticket-tiers { event-id: event-id, tier-id: tier-id })
)

;; Get ticket details
(define-read-only (get-ticket (event-id uint) (ticket-id uint))
  (map-get? tickets { event-id: event-id, ticket-id: ticket-id })
)

;; Get attendance for an event
(define-read-only (get-attendance (event-id uint))
  (default-to { attendees: (list) } (map-get? attendance { event-id: event-id }))
)

