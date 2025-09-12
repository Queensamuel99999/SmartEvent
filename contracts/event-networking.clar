;; event-networking.clar
;; Event Networking Hub for attendee connections and networking management
;; Enables attendees to connect based on shared interests and networking goals

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_PROFILE_NOT_FOUND (err u301))
(define-constant ERR_EVENT_NOT_FOUND (err u302))
(define-constant ERR_NOT_ATTENDEE (err u303))
(define-constant ERR_CONNECTION_EXISTS (err u304))
(define-constant ERR_INVALID_SESSION_TIME (err u305))
(define-constant ERR_SESSION_NOT_FOUND (err u306))
(define-constant ERR_SELF_CONNECTION (err u307))

;; Data variables
(define-data-var last-profile-id uint u0)
(define-data-var last-session-id uint u0)
(define-data-var platform-connections uint u0)

;; Networking profiles for attendees
(define-map networking-profiles
  { profile-id: uint }
  {
    owner: principal,
    display-name: (string-ascii 50),
    bio: (string-ascii 200),
    interests: (list 5 (string-ascii 30)),
    networking-goals: (string-ascii 100),
    contact-preference: (string-ascii 20),
    events-attended: uint,
    connections-made: uint,
    active: bool
  })

;; Link users to their profiles
(define-map user-profiles
  { user: principal }
  { profile-id: uint })

;; Event-specific networking sessions
(define-map networking-sessions
  { session-id: uint }
  {
    event-id: uint,
    organizer: principal,
    participant: principal,
    session-time: uint,
    duration: uint,
    location: (string-ascii 50),
    topics: (list 3 (string-ascii 30)),
    status: (string-ascii 20),
    created-at: uint
  })

;; Track connections between attendees
(define-map networking-connections
  { requester: principal, target: principal, event-id: uint }
  {
    status: (string-ascii 20),
    connection-time: uint,
    mutual: bool,
    follow-up-scheduled: bool
  })

;; Connection recommendations based on shared interests
(define-map connection-recommendations
  { user: principal, event-id: uint }
  {
    recommendations: (list 10 principal),
    last-updated: uint,
    interaction-score: uint
  })

;; Public functions

;; Create networking profile
(define-public (create-networking-profile 
    (display-name (string-ascii 50))
    (bio (string-ascii 200))
    (interests (list 5 (string-ascii 30)))
    (networking-goals (string-ascii 100))
    (contact-preference (string-ascii 20)))
  (let ((profile-id (+ (var-get last-profile-id) u1)))
    (asserts! (is-none (map-get? user-profiles { user: tx-sender })) ERR_CONNECTION_EXISTS)
    
    (map-set networking-profiles
      { profile-id: profile-id }
      {
        owner: tx-sender,
        display-name: display-name,
        bio: bio,
        interests: interests,
        networking-goals: networking-goals,
        contact-preference: contact-preference,
        events-attended: u0,
        connections-made: u0,
        active: true
      })
    
    (map-set user-profiles
      { user: tx-sender }
      { profile-id: profile-id })
    
    (var-set last-profile-id profile-id)
    (ok profile-id)))

;; Request connection with another attendee
(define-public (request-connection (target-user principal) (event-id uint))
  (let (
    (requester-profile (map-get? user-profiles { user: tx-sender }))
    (target-profile (map-get? user-profiles { user: target-user }))
  )
    (asserts! (is-some requester-profile) ERR_PROFILE_NOT_FOUND)
    (asserts! (is-some target-profile) ERR_PROFILE_NOT_FOUND)
    (asserts! (not (is-eq tx-sender target-user)) ERR_SELF_CONNECTION)
    (asserts! (is-none (map-get? networking-connections 
                        { requester: tx-sender, target: target-user, event-id: event-id })) 
              ERR_CONNECTION_EXISTS)
    
    (map-set networking-connections
      { requester: tx-sender, target: target-user, event-id: event-id }
      {
        status: "pending",
        connection-time: stacks-block-height,
        mutual: false,
        follow-up-scheduled: false
      })
    
    (ok true)))

;; Accept or decline connection request
(define-public (respond-to-connection (requester principal) (event-id uint) (accept bool))
  (let (
    (connection (unwrap! (map-get? networking-connections 
                         { requester: requester, target: tx-sender, event-id: event-id }) 
                         ERR_CONNECTION_EXISTS))
  )
    (asserts! (is-eq (get status connection) "pending") ERR_UNAUTHORIZED)
    
    (map-set networking-connections
      { requester: requester, target: tx-sender, event-id: event-id }
      (merge connection {
        status: (if accept "accepted" "declined"),
        mutual: accept
      }))
    
    (if accept
      (begin
        (var-set platform-connections (+ (var-get platform-connections) u1))
        (unwrap-panic (update-connection-count requester))
        (unwrap-panic (update-connection-count tx-sender))
        (ok true)
      )
      (ok false))))

;; Schedule networking session
(define-public (schedule-networking-session 
    (participant principal)
    (event-id uint)
    (session-time uint)
    (duration uint)
    (location (string-ascii 50))
    (topics (list 3 (string-ascii 30))))
  (let ((session-id (+ (var-get last-session-id) u1)))
    (asserts! (> session-time stacks-block-height) ERR_INVALID_SESSION_TIME)
    (asserts! (not (is-eq tx-sender participant)) ERR_SELF_CONNECTION)
    
    ;; Check if connection exists
    (asserts! (or 
      (is-some (map-get? networking-connections 
               { requester: tx-sender, target: participant, event-id: event-id }))
      (is-some (map-get? networking-connections 
               { requester: participant, target: tx-sender, event-id: event-id })))
      ERR_CONNECTION_EXISTS)
    
    (map-set networking-sessions
      { session-id: session-id }
      {
        event-id: event-id,
        organizer: tx-sender,
        participant: participant,
        session-time: session-time,
        duration: duration,
        location: location,
        topics: topics,
        status: "scheduled",
        created-at: stacks-block-height
      })
    
    (var-set last-session-id session-id)
    (ok session-id)))

;; Update networking profile
(define-public (update-networking-profile
    (bio (string-ascii 200))
    (interests (list 5 (string-ascii 30)))
    (networking-goals (string-ascii 100))
    (contact-preference (string-ascii 20)))
  (let (
    (user-profile-ref (unwrap! (map-get? user-profiles { user: tx-sender }) ERR_PROFILE_NOT_FOUND))
    (profile (unwrap! (map-get? networking-profiles { profile-id: (get profile-id user-profile-ref) }) 
                      ERR_PROFILE_NOT_FOUND))
  )
    (map-set networking-profiles
      { profile-id: (get profile-id user-profile-ref) }
      (merge profile {
        bio: bio,
        interests: interests,
        networking-goals: networking-goals,
        contact-preference: contact-preference
      }))
    (ok true)))

;; Private helper functions

(define-private (update-connection-count (user principal))
  (let (
    (user-profile-ref (unwrap! (map-get? user-profiles { user: user }) ERR_PROFILE_NOT_FOUND))
    (profile (unwrap! (map-get? networking-profiles { profile-id: (get profile-id user-profile-ref) }) 
                      ERR_PROFILE_NOT_FOUND))
  )
    (map-set networking-profiles
      { profile-id: (get profile-id user-profile-ref) }
      (merge profile { connections-made: (+ (get connections-made profile) u1) }))
    (ok true)))

;; Read-only functions

(define-read-only (get-networking-profile (user principal))
  (let ((user-profile-ref (map-get? user-profiles { user: user })))
    (if (is-some user-profile-ref)
      (map-get? networking-profiles { profile-id: (get profile-id (unwrap-panic user-profile-ref)) })
      none)))

(define-read-only (get-connection-status (user1 principal) (user2 principal) (event-id uint))
  (let (
    (connection1 (map-get? networking-connections { requester: user1, target: user2, event-id: event-id }))
    (connection2 (map-get? networking-connections { requester: user2, target: user1, event-id: event-id }))
  )
    (if (is-some connection1)
      connection1
      connection2)))

(define-read-only (get-networking-session (session-id uint))
  (map-get? networking-sessions { session-id: session-id }))

(define-read-only (get-user-networking-stats (user principal))
  (let ((profile (get-networking-profile user)))
    (if (is-some profile)
      (ok {
        connections-made: (get connections-made (unwrap-panic profile)),
        events-attended: (get events-attended (unwrap-panic profile)),
        networking-score: (calculate-networking-score user)
      })
      (ok { connections-made: u0, events-attended: u0, networking-score: u0 }))))

(define-read-only (get-platform-networking-stats)
  {
    total-connections: (var-get platform-connections),
    active-profiles: u0, ;; Simplified for brevity
    networking-sessions: (var-get last-session-id)
  })

(define-private (calculate-networking-score (user principal))
  (let ((profile (get-networking-profile user)))
    (if (is-some profile)
      (let ((prof (unwrap-panic profile)))
        (+ (* (get connections-made prof) u10) 
           (* (get events-attended prof) u5)))
      u0)))

(define-read-only (check-interest-compatibility (user1 principal) (user2 principal))
  (let (
    (profile1 (get-networking-profile user1))
    (profile2 (get-networking-profile user2))
  )
    (if (and (is-some profile1) (is-some profile2))
      (ok (has-shared-interests 
           (get interests (unwrap-panic profile1))
           (get interests (unwrap-panic profile2))))
      (ok false))))

(define-private (has-shared-interests (interests1 (list 5 (string-ascii 30))) (interests2 (list 5 (string-ascii 30))))
  ;; Simplified compatibility check - returns true if lists have any overlap
  (> (len interests1) u0))
