;; Assume .sBTC is a fungible token (FT) and we have a sip-010 trait implemented for it
(use-trait ft-trait .sip-010-trait)

;; Data map to store task information including the locked funds and time limit
(define-map taskPools
  {taskId: uint}
  {
    creator: principal,
    poolSize: uint,
    payoutPerTask: uint,
    totalPaidOut: uint,
    totalLockedFunds: uint,
    timeLimit: uint
  }
)

;; Data map to store payout requests
(define-map payoutRequests
  {taskId: uint, worker: principal}
  {amount: uint}
)

;; Variable to track the next task ID
(define-data-var nextTaskId uint 0)

;; Function to create a new task pool with a time limit
(define-public (createTask (creator principal) (poolSize uint) (payoutPerTask uint) (timeLimit uint))
  (let ((taskId (var-get nextTaskId)))
    (begin
      (try! (contract-call? .sBTC ft-transfer poolSize tx-sender (as-contract tx-sender)))
      (map-insert taskPools {taskId: taskId}
        {
          creator: principal,
          poolSize: poolSize,
          payoutPerTask: payoutPerTask,
          totalPaidOut: u0,
          totalLockedFunds: poolSize,
          timeLimit: (+ (block-height) timeLimit)
        })
      (var-set nextTaskId (+ taskId u1))
      (ok taskId)
    )
  )
)

;; Function for workers to request a payout
(define-public (requestPayout (taskId uint) (receivingAddress principal) (searnSignature (buff 65)))
  (let ((task (map-get? taskPools {taskId: taskId})))
    (if (and task (is-some task))
      (let ((currentBlockHeight (block-height))
            (timeLimit (get timeLimit (unwrap-panic task))))
        (if (<= currentBlockHeight timeLimit)
          (if (verify-searn-signature searnSignature)
            (let ((payout (get payoutPerTask (unwrap-panic task))))
              (map-insert payoutRequests {taskId: taskId, worker: receivingAddress} {amount: payout})
              (ok true))
            (err "Invalid sEARN signature"))
          (err "Time limit exceeded")))
      (err "Task does not exist")))
)

;; Function to release payments for a specific task
(define-public (releasePayments (taskId uint) (taskCreatorSignature (buff 65)))
  (let ((task (map-get? taskPools {taskId: taskId})))
    (if (and task (is-some task))
      (if (verify-task-creator-signature taskId taskCreatorSignature)
        (begin
          ;; Assuming release-all-payments handles the logic of paying out
          (release-all-payments taskId)
          ;; Clear payout requests after payments are done
          (map-delete payoutRequests {taskId: taskId})
          (ok true))
        (err "Invalid task creator signature or task ID"))
      (err "Task does not exist")))
)

;; Function to withdraw the remaining task pool
(define-public (withdrawTask (taskId uint) (taskCreatorSignature (buff 65)))
  (let ((task (map-get? taskPools {taskId: taskId})))
    (if (and task (is-some task))
      (let ((currentBlockHeight (block-height))
            (timeLimit (get timeLimit (unwrap-panic task))))
        (if (and (> currentBlockHeight timeLimit)
                 (verify-task-creator-signature taskCreatorSignature))
          (begin
            (try! (contract-call? .sBTC ft-transfer
                                   (get totalLockedFunds (unwrap-panic task))
                                   (as-contract tx-sender)
                                   tx-sender))
            (map-delete taskPools {taskId: taskId})
            (ok true))
          (err "Time limit has not expired or invalid signature")))
      (err "Task does not exist")))
)

;; Helper function to verify the sEARN server signature
(define-private (verify-searn-signature (signature (buff 65)))
  ;; Signature verification logic goes here
  ;; Return true if signature is valid
  (ok true)
)

;; Helper function to verify the task creator signature
(define-private (verify-task-creator-signature (signature (buff 65)))
  ;; Task creator signature verification logic goes here
  ;; Return the taskId if signature is valid
  (ok u1)
)

;; Helper function to release all payments for a given task
(define-private (release-all-payments (taskId uint))
  ;; Implement the logic to iterate over all payout requests for the task
  ;; and send the payments
  (ok true)
)
