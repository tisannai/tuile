(define-module (tuile daemon)
  #:export
  (
   daemonize
   ))

(define (daemonize proc)
  (let ((pid (primitive-fork)))
    (cond
     ((= pid 0)          ; Child process
      (setsid)           ; Create a new session (detach from terminal)
      (proc))            ; Run the daemon task.
     (else
      (exit 0)))))  ; Parent exits immediately
