;; Interactive debugger
;;
;; Usage:
;; - Run guile with "--debug" option.
;; - Do: (use-modules (tuile dbg))
;; - Either:
;;   - Use debug breakpoint:
;;     - Activate the "gdb-break" procedure at start of program: (dbg-with-break)
;;     - Call at debug site: (dbg-break)
;;   - Set breakpoint to a proc: (dbg-proc my-proc)

(define-module (tuile dbg)
  #:use-module (system repl debug)
  #:use-module (system vm vm)
  #:use-module (system vm trace)
  #:use-module (system vm trap-state)
  #:use-module (system repl repl)
  #:use-module (system vm program)
  #:use-module (ice-9 readline)
  #:use-module ((srfi srfi-1) #:select (drop-right))
  #:use-module (system repl common)

  #:use-module (system base syntax)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (system base language)
;;   #:use-module (system vm vm)
  #:use-module (system repl error-handling)
;;   #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (ice-9 control)

  #:export
  (
;;    dbg-fin

   dbg-proc
   dbg-trace
   dbg-trace-all
   dbg-with-break
   dbg-break
   dbg-welcome
   ))

;; Make Readline active.
(activate-readline)


;; (define (current-frame)
;;   (stack-ref (make-stack #t) 0))


(define my-start-repl #f)

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

         ;; Trial for taking out the "dbg-break" proc from stack, but
         ;; confuses the tracing.

         ;;(stack (narrow-stack->vector (make-stack frame) 1 0))

         (error-msg (if trap-idx
                        (format #f "Trap ~d: ~a" trap-idx trap-name)
                        trap-name))
         (debug (make-debug stack 0 error-msg #t)))
    (with-saved-ports
     (lambda ()
        (if trap-idx
            (begin
              (format #t "~a~%" error-msg)
;;               (format #t "Entering a new prompt.  ")
;;               (format #t "Type `,bt' for a backtrace or `,q' to continue.\n")
              ))
        ;;((@ (system repl repl) start-repl) #:debug debug)
        (print-frame frame #:index 0)
        (my-start-repl #:debug debug)
       ))))


(define (debug-frame-handler frame)
  (debug-trap-handler frame #f #f))

(define (ensure-trace)
  (when (< (vm-trace-level) 1)
    (set-vm-trace-level! 1)))


;; (define (dbg-fin)
;;   (add-ephemeral-trap-at-frame-finish! (current-frame) debug-frame-handler))


;; Set a breakpoint at proc.
;;
;; Normal Guile REPL debugging commands can be used when prompt
;; appears.
;;
;; NOTE: You must do ",next" before you are in the proc and can see locals.
;;
(define (dbg-proc proc)
  ;; (set-vm-trace-level! (1+ (vm-trace-level)))
  (ensure-trace)
  (install-trap-handler! debug-trap-handler)
  (add-trap-at-procedure-call! proc))


(define (dbg-trace . proc)
  (ensure-trace)
  (let loop ((proc proc))
    (when (pair? proc)
      (trace-calls-to-procedure (car proc))
      (loop (cdr proc)))))


(define (dbg-trace-all thunk)
  (call-with-trace thunk))


;; Set a breakpoint at callpoint.
;;
;; Normal Guile REPL debugging commands can be used when prompt
;; appears.
;;
;; NOTE: You must do ",next" before you are in the proc and can see locals.
;;
;; (define (dbg-break)
;;   ;;                                                  , take top 2 entries off.
;;   (let (
;;         (stack (narrow-stack->vector (make-stack #t)))
;;         ;; (stack (narrow-stack->vector (make-stack #t) 2 0))
;;         )
;;     (set-vm-trace-level! 1)
;;     (start-repl #:debug (make-debug stack 0 "Entered debugger" #f))))

(define (dbg-break) *unspecified*)

(define (dbg-welcome repl)
  (display "dbg-welcome\n"))

(define (dbg-with-break)
;;   (set! repl-welcome dbg-welcome)
  (set-vm-trace-level! (1+ (vm-trace-level)))
  (install-trap-handler! debug-trap-handler)
  (add-trap-at-procedure-call! dbg-break))



;; ------------------------------------------------------------
;; Procedures taken from (system repl repl) and modified for debugging
;; use.


;;; Read-Eval-Print Loop

;; Copyright (C) 2001, 2009, 2010, 2011, 2013,
;;   2014 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

;; (define-module (system repl repl)
;;   #:use-module (system base syntax)
;;   #:use-module (system base pmatch)
;;   #:use-module (system base compile)
;;   #:use-module (system base language)
;;   #:use-module (system vm vm)
;;   #:use-module (system repl error-handling)
;;   #:use-module (system repl common)
;;   #:use-module (system repl command)
;;   #:use-module (ice-9 control)
;;   #:export (start-repl run-repl))
;;

;;;
;;; Comments
;;;
;;; (You don't want a comment to force a continuation line.)
;;;

(define (read-scheme-line-comment port)
  (let lp ()
    (let ((ch (read-char port)))
      (or (eof-object? ch)
          (eqv? ch #\newline)
          (lp)))))

(define (read-scheme-datum-comment port)
  (read port))

;; ch is a peeked char
(define (read-comment lang port ch)
  (and (eq? (language-name lang) 'scheme)
       (case ch
         ((#\;)
          (read-char port)
          (read-scheme-line-comment port)
          #t)
         ((#\#)
          (read-char port)
          (case (peek-char port)
            ((#\;)
             (read-char port)
             (read-scheme-datum-comment port)
             #t)
            ;; Not doing R6RS block comments because of the possibility
            ;; of read-hash extensions.  Lame excuse.  Not doing scsh
            ;; block comments either, because I don't feel like handling
            ;; #!r6rs.
            (else
             (unread-char #\# port)
             #f)))
         (else
          #f))))

;;;
;;; Meta commands
;;;

(define meta-command-token (cons 'meta 'command))

(define (meta-reader lang env)
  (lambda* (#:optional (port (current-input-port)))
    (with-input-from-port port
      (lambda ()
        (let ((ch (flush-leading-whitespace)))
          (cond ((eof-object? ch)
                 (read-char))  ; consume the EOF and return it
                ((eqv? ch #\,)
                 (read-char)
                 meta-command-token)
                ((read-comment lang port ch)
                 *unspecified*)
                (else ((language-reader lang) port env))))))))

(define (flush-all-input)
  (if (and (char-ready?)
           (not (eof-object? (peek-char))))
      (begin
        (read-char)
        (flush-all-input))))

;; repl-reader is a function defined in boot-9.scm, and is replaced by
;; something else if readline has been activated. much of this hoopla is
;; to be able to re-use the existing readline machinery.
;;
;; Catches read errors, returning *unspecified* in that case.
;;
;; Note: although not exported, this is used by (system repl coop-server)
(define (prompting-meta-read repl)
  (catch #t
    (lambda ()
      (repl-reader (lambda () (repl-prompt repl))
                   (meta-reader (repl-language repl) (current-module))))
    (lambda (key . args)
      (case key
        ((quit)
         (apply throw key args))
        (else
         (format (current-output-port) "While reading expression:\n")
         (print-exception (current-output-port) #f key args)
         (flush-all-input)
         *unspecified*)))))

;;;
;;; The repl
;;;

(define* (my-start-repl #:optional (lang (current-language)) #:key debug)
  (my-start-repl* lang debug prompting-meta-read))

;; Note: although not exported, this is used by (system repl coop-server)
(define (my-start-repl* lang debug prompting-meta-read)
  ;; ,language at the REPL will update the current-language.  Make
  ;; sure that it does so in a new dynamic scope.
  (parameterize ((current-language lang))
    (my-run-repl* (make-repl lang debug) prompting-meta-read)))

;; (put 'abort-on-error 'scheme-indent-function 1)
(define-syntax-rule (abort-on-error string exp)
  (catch #t
    (lambda () exp)
    (lambda (key . args)
      (format #t "While ~A:~%" string)
      (print-exception (current-output-port) #f key args)
      (abort))))

(define (my-run-repl repl)
  (my-run-repl* repl prompting-meta-read))

(define (my-repl-welcome repl)
;;   (display "-- my-repl --\n")
  #t
  )

(define (my-run-repl* repl prompting-meta-read)
  (define (with-stack-and-prompt thunk)
    (call-with-prompt (default-prompt-tag)
                      (lambda () (start-stack #t (thunk)))
                      (lambda (k proc)
                        (with-stack-and-prompt (lambda () (proc k))))))

  (% (with-fluids ((*repl-stack*
                    (cons repl (or (fluid-ref *repl-stack*) '()))))
       (if (null? (cdr (fluid-ref *repl-stack*)))
           (my-repl-welcome repl))
;;        (display "-- my-repl --\n")
       (let prompt-loop ()
         (let ((exp (prompting-meta-read repl)))
           (cond
            ((eqv? exp *unspecified*))  ; read error or comment, pass
            ((eq? exp meta-command-token)
             (catch #t
               (lambda ()
                 (meta-command repl))
               (lambda (k . args)
                 (if (eq? k 'quit)
                     (abort args)
                     (begin
                       (format #t "While executing meta-command:~%")
                       (print-exception (current-output-port) #f k args))))))
            ((eof-object? exp)
             (newline)
             (abort '()))
            (else
             ;; since the input port is line-buffered, consume up to the
             ;; newline
             (flush-to-newline)
             (call-with-error-handling
              (lambda ()
                (catch 'quit
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (% (let ((thunk
                                    (abort-on-error "compiling expression"
                                      (repl-prepare-eval-thunk
                                       repl
                                       (abort-on-error "parsing expression"
                                         (repl-parse repl exp))))))
                               (run-hook before-eval-hook exp)
                               (call-with-error-handling
                                (lambda ()
                                  (with-stack-and-prompt thunk))
                                #:on-error (repl-option-ref repl 'on-error)))
                             (lambda (k) (values))))
                      (lambda l
                        (for-each (lambda (v)
                                    (repl-print repl v))
                                  l))))
                  (lambda (k . args)
                    (abort args))))
              #:on-error (repl-option-ref repl 'on-error)
              #:trap-handler 'disabled)))
           (flush-to-newline) ;; consume trailing whitespace
           (prompt-loop))))
     (lambda (k status)
       status)))

;; Returns first non-whitespace char.
(define (flush-leading-whitespace)
  (let ((ch (peek-char)))
    (cond ((eof-object? ch) ch)
          ((char-whitespace? ch) (read-char) (flush-leading-whitespace))
          (else ch))))

(define (flush-to-newline)
  (if (char-ready?)
      (let ((ch (peek-char)))
        (if (and (not (eof-object? ch)) (char-whitespace? ch))
            (begin
              (read-char)
              (if (not (char=? ch #\newline))
                  (flush-to-newline)))))))
