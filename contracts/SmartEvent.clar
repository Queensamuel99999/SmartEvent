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

(define-map event-categories
  { event-id: uint }
  {
    category: (string-ascii 50),
    tags: (list 10 (string-ascii 20))
  }
)

;; Add these public functions
(define-public (set-event-category 
    (event-id uint) 
    (category (string-ascii 50))
    (event-tags (list 10 (string-ascii 20))))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (map-set event-categories
      { event-id: event-id }
      {
        category: category,
        tags: event-tags
      }
    )
    (ok true)
  )
)

(define-read-only (get-event-category (event-id uint))
  (map-get? event-categories { event-id: event-id })
)


;; Add to Data maps section
(define-map event-ratings
  { event-id: uint, user: principal }
  {
    rating: uint,
    review: (string-ascii 200)
  }
)

(define-map event-average-rating
  { event-id: uint }
  {
    total-ratings: uint,
    average-rating: uint
  }
)

;; Add these public functions
(define-public (rate-event 
    (event-id uint) 
    (rating uint) 
    (review (string-ascii 200)))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (current-avg (default-to { total-ratings: u0, average-rating: u0 } 
                  (map-get? event-average-rating { event-id: event-id })))
  )
    (asserts! (>= rating u1) (err u300))
    (asserts! (<= rating u5) (err u301))
    (asserts! (get ended event) (err u302))
    
    (map-set event-ratings
      { event-id: event-id, user: tx-sender }
      { rating: rating, review: review }
    )
    
    (map-set event-average-rating
      { event-id: event-id }
      {
        total-ratings: (+ (get total-ratings current-avg) u1),
        average-rating: (/ (+ (* (get average-rating current-avg) 
                                (get total-ratings current-avg)) 
                             rating)
                          (+ (get total-ratings current-avg) u1))
      }
    )
    (ok true)
  )
)

(define-read-only (get-event-rating (event-id uint))
  (map-get? event-average-rating { event-id: event-id })
)


;; Add to events map
(define-constant ERR-REFUND-NOT-ALLOWED (err u108))

(define-map refund-policies
  { event-id: uint }
  {
    refundable: bool,
    refund-deadline: uint,
    refund-percentage: uint
  }
)

(define-public (set-refund-policy
    (event-id uint)
    (refundable bool)
    (refund-deadline uint)
    (refund-percentage uint))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (asserts! (<= refund-percentage u100) (err u109))
    
    (map-set refund-policies
      { event-id: event-id }
      {
        refundable: refundable,
        refund-deadline: refund-deadline,
        refund-percentage: refund-percentage
      }
    )
    (ok true)
  )
)

(define-public (request-refund (event-id uint) (ticket-id uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (ticket (unwrap! (map-get? tickets { event-id: event-id, ticket-id: ticket-id }) ERR-TICKET-NOT-FOUND))
    (policy (unwrap! (map-get? refund-policies { event-id: event-id }) ERR-REFUND-NOT-ALLOWED))
  )
    (asserts! (get refundable policy) ERR-REFUND-NOT-ALLOWED)
    (asserts! (< stacks-block-height (get refund-deadline policy)) ERR-REFUND-NOT-ALLOWED)
    (asserts! (is-eq tx-sender (get owner ticket)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get used ticket)) ERR-TICKET-ALREADY-USED)
    
    ;; Process refund logic here
    (map-set tickets
      { event-id: event-id, ticket-id: ticket-id }
      (merge ticket { owner: (get organizer event), used: true })
    )
    (ok true)
  )
)

;; Add to Data maps section
(define-map event-waitlist
  { event-id: uint }
  {
    users: (list 100 principal),
    notification-sent: (list 100 bool)
  }
)

(define-public (join-waitlist (event-id uint))
  (let (
    (event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND))
    (current-waitlist (default-to { users: (list), notification-sent: (list) } 
                       (map-get? event-waitlist { event-id: event-id })))
  )
    (asserts! (not (get ended event)) ERR-EVENT-ENDED)
    
    (map-set event-waitlist
      { event-id: event-id }
      {
        users: (unwrap! (as-max-len? 
                         (append (get users current-waitlist) tx-sender) 
                         u100) 
                       (err u110)),
        notification-sent: (unwrap! (as-max-len? 
                                    (append (get notification-sent current-waitlist) false)
                                    u100)
                                  (err u110))
      }
    )
    (ok true)
  )
)

(define-read-only (get-waitlist-position (event-id uint))
  (let ((waitlist (default-to { users: (list), notification-sent: (list) }
                   (map-get? event-waitlist { event-id: event-id }))))
    (index-of (get users waitlist) tx-sender)
  )
)


;; Add to tickets map
(define-map ticket-qr-data
  { event-id: uint, ticket-id: uint }
  {
    code: (string-ascii 100),
    generated-at: uint,
    valid-until: uint
  }
)

(define-public (generate-ticket-qr (event-id uint) (ticket-id uint))
  (let (
    (ticket (unwrap! (map-get? tickets { event-id: event-id, ticket-id: ticket-id }) ERR-TICKET-NOT-FOUND))
    (current-time stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get owner ticket)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get used ticket)) ERR-TICKET-ALREADY-USED)
    
    (let ((qr-string (concat (concat (int-to-ascii event-id) "-") (int-to-ascii ticket-id))))
      (map-set ticket-qr-data
        { event-id: event-id, ticket-id: ticket-id }
        {
          code: qr-string,
          generated-at: current-time,
          valid-until: (+ current-time u1440) ;; Valid for 1440 blocks (approx. 24 hours)
        }
      )
      (ok qr-string)
    )
  )
)

(define-read-only (verify-ticket-qr 
    (event-id uint) 
    (ticket-id uint) 
    (qr-code (string-ascii 100)))
  (let ((qr-data (unwrap! (map-get? ticket-qr-data { event-id: event-id, ticket-id: ticket-id }) 
                          (err u111))))
    (ok (and
      (is-eq (get code qr-data) qr-code)
      (< stacks-block-height (get valid-until qr-data))
    ))
  )
)

;; Add to Data maps section
(define-map promo-codes
  { event-id: uint, code: (string-ascii 20) }
  {
    discount-percentage: uint,
    max-uses: uint,
    uses: uint,
    expires-at: uint
  }
)

(define-public (create-promo-code
    (event-id uint)
    (code (string-ascii 20))
    (discount-percentage uint)
    (max-uses uint)
    (expires-at uint))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (asserts! (<= discount-percentage u100) (err u112))
    
    (map-set promo-codes
      { event-id: event-id, code: code }
      {
        discount-percentage: discount-percentage,
        max-uses: max-uses,
        uses: u0,
        expires-at: expires-at
      }
    )
    (ok true)
  )
)

(define-read-only (verify-promo-code (event-id uint) (code (string-ascii 20)))
  (let ((promo (unwrap! (map-get? promo-codes { event-id: event-id, code: code }) 
                        (err u113))))
    (ok (and
      (< stacks-block-height (get expires-at promo))
      (< (get uses promo) (get max-uses promo))
    ))
  )
)


(define-map vip-passes
  { pass-id: uint }
  {
    name: (string-ascii 50),
    owner: principal,
    valid-until: uint,
    events: (list 50 uint),
    benefits: (string-ascii 200),
    transferable: bool
  }
)

(define-data-var last-pass-id uint u0)

(define-public (create-vip-pass 
    (name (string-ascii 50))
    (valid-until uint)
    (event-list (list 50 uint))
    (benefits (string-ascii 200))
    (transferable bool))
  (let ((pass-id (+ (var-get last-pass-id) u1)))
    (map-set vip-passes
      { pass-id: pass-id }
      {
        name: name,
        owner: tx-sender,
        valid-until: valid-until,
        events: event-list,
        benefits: benefits,
        transferable: transferable
      }
    )
    (var-set last-pass-id pass-id)
    (ok pass-id)
  )
)

(define-public (transfer-vip-pass (pass-id uint) (new-owner principal))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u114))))
    (asserts! (is-eq tx-sender (get owner pass)) ERR-NOT-AUTHORIZED)
    (asserts! (get transferable pass) ERR-NOT-AUTHORIZED)
    (map-set vip-passes
      { pass-id: pass-id }
      (merge pass { owner: new-owner })
    )
    (ok true)
  )
)

(define-read-only (get-vip-pass (pass-id uint))
  (map-get? vip-passes { pass-id: pass-id })
)


(define-read-only (get-vip-pass-events (pass-id uint))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u115))))
    (ok (get events pass))
  )
)
(define-read-only (get-vip-pass-benefits (pass-id uint))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u116))))
    (ok (get benefits pass))
  )
)
(define-read-only (get-vip-pass-validity (pass-id uint))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u117))))
    (ok (< stacks-block-height (get valid-until pass)))
  )
)
(define-read-only (get-vip-pass-owner (pass-id uint))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u118))))
    (ok (get owner pass))
  )
)
(define-read-only (get-vip-pass-transferable (pass-id uint))
  (let ((pass (unwrap! (map-get? vip-passes { pass-id: pass-id }) (err u119))))
    (ok (get transferable pass))
  )
)


(define-map dynamic-pricing
  { event-id: uint, tier-id: uint }
  {
    base-price: uint,
    max-price: uint,
    min-price: uint,
    current-price: uint,
    last-update: uint,
    demand-multiplier: uint
  }
)

(define-public (setup-dynamic-pricing
    (event-id uint)
    (tier-id uint)
    (base-price uint)
    (max-price uint)
    (min-price uint))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) ERR-EVENT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
    (map-set dynamic-pricing
      { event-id: event-id, tier-id: tier-id }
      {
        base-price: base-price,
        max-price: max-price,
        min-price: min-price,
        current-price: base-price,
        last-update: stacks-block-height,
        demand-multiplier: u100
      }
    )
    (ok true)
  )
)

(define-public (update-dynamic-price (event-id uint) (tier-id uint))
  (let (
    (pricing (unwrap! (map-get? dynamic-pricing { event-id: event-id, tier-id: tier-id }) (err u117)))
    (tier (unwrap! (map-get? ticket-tiers { event-id: event-id, tier-id: tier-id }) (err u117)))
  )
    (let ((new-price (calculate-dynamic-price 
                      (get base-price pricing)
                      (get max-price pricing)
                      (get min-price pricing)
                      (get sold tier)
                      (get max-supply tier))))
      (map-set dynamic-pricing
        { event-id: event-id, tier-id: tier-id }
        (merge pricing {
          current-price: new-price,
          last-update: stacks-block-height
        })
      )
      (ok new-price)
    )
  )
)

(define-private (calculate-dynamic-price (base uint) (max uint) (min uint) (sold uint) (total uint))
  (let ((demand-factor (/ (* sold u100) total)))
    (let ((price-adjustment (* base (/ demand-factor u100))))
      (if (> price-adjustment max)
        max
        (if (< price-adjustment min)
          min
          price-adjustment
        )
      )
    )
  )
)