;; Educational Game Smart Contract
;; Enhanced with input validation and safety checks

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-INVALID-PLAYER (err u2))
(define-constant ERR-INSUFFICIENT-BALANCE (err u3))
(define-constant ERR-GAME-NOT-FOUND (err u4))
(define-constant ERR-ACHIEVEMENT-EXISTS (err u5))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u6))
(define-constant ERR-INVALID-INPUT (err u7))

;; Helper functions for input validation
(define-private (is-valid-game-id (id uint))
  (< id u1000000))

(define-private (is-valid-level (level uint))
  (and (> level u0) (< level u100)))

(define-private (is-valid-progress-percentage (percentage uint))
  (<= percentage u100))

(define-private (is-valid-achievement-name (name (string-ascii 100)))
  (and (> (len name) u0) (<= (len name) u100)))

(define-private (is-valid-description (description (string-ascii 255)))
  (and (> (len description) u0) (<= (len description) u255)))

;; Simplified username validation
(define-private (is-valid-username (username (string-ascii 50)))
  (and 
    (> (len username) u2)  ;; Minimum 3 characters
    (<= (len username) u50)  ;; Maximum 50 characters
  ))

;; Data Maps
(define-map player-profiles 
  principal 
  {
    username: (string-ascii 50),
    total-score: uint,
    level: uint,
    games-played: uint
  })

(define-map game-progress 
  { player: principal, game-id: uint }
  {
    current-level: uint,
    progress-percentage: uint,
    completed: bool
  })

(define-map achievements 
  { player: principal, achievement-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 255),
    reward-points: uint,
    unlocked-at: uint
  })

(define-map reward-tokens 
  principal 
  uint)

;; Helper function to check if achievement exists
(define-private (achievement-exists? (player principal) (achievement-id uint))
  (is-some (map-get? achievements { player: player, achievement-id: achievement-id })))

;; Public functions
(define-public (register-player (username (string-ascii 50)))
  (begin
    (asserts! (is-valid-username username) ERR-INVALID-INPUT)
    (asserts! (is-none (map-get? player-profiles tx-sender)) ERR-INVALID-PLAYER)
    
    (map-set player-profiles 
      tx-sender 
      {
        username: username,
        total-score: u0,
        level: u1,
        games-played: u0
      })
    (ok true)))

(define-public (update-game-progress 
  (game-id uint) 
  (current-level uint) 
  (progress-percentage uint))
  (let 
    ((player-profile (unwrap! 
      (map-get? player-profiles tx-sender) 
      ERR-INVALID-PLAYER)))
    
    (asserts! (is-valid-game-id game-id) ERR-INVALID-INPUT)
    (asserts! (is-valid-level current-level) ERR-INVALID-INPUT)
    (asserts! (is-valid-progress-percentage progress-percentage) ERR-INVALID-INPUT)
    
    (map-set game-progress 
      { player: tx-sender, game-id: game-id }
      {
        current-level: current-level,
        progress-percentage: progress-percentage,
        completed: (is-eq progress-percentage u100)
      })
    
    (map-set player-profiles 
      tx-sender 
      (merge player-profile { 
        games-played: (+ (get games-played player-profile) u1) 
      }))
    
    (ok true)))

(define-public (unlock-achievement 
  (achievement-id uint) 
  (name (string-ascii 100)) 
  (description (string-ascii 255)) 
  (reward-points uint))
  (let 
    ((player-profile (unwrap! 
      (map-get? player-profiles tx-sender) 
      ERR-INVALID-PLAYER)))
    
    (asserts! (is-valid-achievement-name name) ERR-INVALID-INPUT)
    (asserts! (is-valid-description description) ERR-INVALID-INPUT)
    (asserts! (> reward-points u0) ERR-INVALID-INPUT)
    (asserts! 
      (not (achievement-exists? tx-sender achievement-id)) 
      ERR-ACHIEVEMENT-EXISTS)
    
    (map-set achievements 
      { player: tx-sender, achievement-id: achievement-id }
      {
        name: name,
        description: description,
        reward-points: reward-points,
        unlocked-at: block-height
      })
    
    (map-set player-profiles 
      tx-sender 
      (merge player-profile { 
        total-score: (+ (get total-score player-profile) reward-points) 
      }))
    
    (ok true)))

(define-public (admin-reset-player-progress (player principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! 
      (is-some (map-get? player-profiles player)) 
      ERR-INVALID-PLAYER)
    
    (map-delete player-profiles player)
    (ok true)))

(define-public (mint-reward-tokens (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-INPUT)
    
    (let 
      ((current-balance (default-to u0 (map-get? reward-tokens tx-sender))))
      (map-set reward-tokens 
        tx-sender 
        (+ current-balance amount))
      (ok true))))

;; Read-only functions
(define-read-only (get-player-profile (player principal))
  (map-get? player-profiles player))

(define-read-only (get-game-progress (player principal) (game-id uint))
  (map-get? game-progress { player: player, game-id: game-id }))

(define-read-only (get-player-achievements (player principal))
  (begin
    (list 
      (map-get? achievements { player: player, achievement-id: u1 })
      (map-get? achievements { player: player, achievement-id: u2 })
      (map-get? achievements { player: player, achievement-id: u3 })
      (map-get? achievements { player: player, achievement-id: u4 })
      (map-get? achievements { player: player, achievement-id: u5 }))))

(define-read-only (get-reward-token-balance (player principal))
  (default-to u0 (map-get? reward-tokens player)))