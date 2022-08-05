;; Interactive debugger
;;
;; Usage:
;; - Run guile with "--debug" option.
;; - Do: (use-modules (tuile dbg))
;; - Set breakpoint to proc: (dbg-proc display)

(define-module (tuile dbg)
  #:use-module (system repl debug)
  #:use-module (system vm vm)
  #:use-module (system vm trap-state)
  #:use-module (system repl repl)
  #:use-module (system vm program)
  #:use-module (ice-9 readline)
  #:export
  (dbg-proc
   dbg-break
   ))

;; Make Readline active.
(activate-readline)

;; Debug handler starts a REPL.
;;
;; NOTE: copied and modified from: system repl error-handling
(define (debug-trap-handler frame trap-idx trap-name)

  (define (with-saved-ports thunk)
    (let ((in (current-input-port))
          (out (current-output-port))
          (err (current-error-port)))
      (with-input-from-port in
        (lambda ()
          (with-output-to-port out
            (lambda ()
              (with-error-to-port err
                thunk)))))))

  (let* ((stack (frame->stack-vector frame))
         (error-msg (if trap-idx
                        (format #f "Trap ~d: ~a" trap-idx trap-name)
                        trap-name))
         (debug (make-debug stack 0 error-msg #t)))
    (with-saved-ports
     (lambda ()
       (if trap-idx
           (begin
             (format #t "~a~%" error-msg)
             (format #t "Entering a new prompt.  ")
             (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")))
       ((@ (system repl repl) start-repl) #:debug debug)))))


;; Set a breakpoint at proc.
;;
;; Normal Guile REPL debugging commands can be used when prompt
;; appears.
;;
;; NOTE: You must do ",next" before you are in the proc and can see locals.
;;
(define (dbg-proc proc)
  (set-vm-trace-level! 1)
  (install-trap-handler! debug-trap-handler)
  (add-trap-at-procedure-call! proc))


;; Set a breakpoint at callpoint.
;;
;; Normal Guile REPL debugging commands can be used when prompt
;; appears.
;;
;; NOTE: You must do ",next" before you are in the proc and can see locals.
;;
(define (dbg-break)
  (let ((stack (narrow-stack->vector (make-stack #t))))
    (start-repl #:debug (make-debug stack 0 "Entered debugger" #f))))
