;;; module:
;;
;; Issue handling and reporting for applications.
;;
;; Issues are generated with issue reporting functions:
;;
;; * issue-info:    Information only (exception).
;; * issue-warn:    Warning only (exception).
;; * issue-problem: Issue problem and accumulate Issue Count (exception).
;; * issue-errro:   Issue error and exit by default (exception).
;; * fatal-issue:   Immediate fatal error and exit (non-exception).
;;
;; The exceptions are handled with '(issue-handle <body> ...)' macro
;; and exceptions are raise within the '<body> ...'. By default info,
;; warn, and problem exceptions are catched and only the message is
;; displayed for the user. The default behavior can be altered by
;; using '(issue-handle-thunk thunk . ignore-types)', which allows the
;; user to specify which types are catched. The 'thunk' captures the
;; user code that is captured by '<body> ...' with 'issue-handle'
;; macro.
;;
;; Issue Reporter construct the displayed screen lines using a
;; customizable message format. The message is formatted using the
;; format specification. The default is:
;;
;;     @1e:\n@4m
;;
;; @ is a special character in the format and defines a segment, and
;; other characters are passed through as is. Errors are formatter
;; with "e" and the message is formatted with "m". These letter are
;; preceded with an (optional) number of spaces to place before the
;; entry. Space count is 0, if there is no number present. For
;; multiline messages, the specified number of spaces are inserted
;; before each line.
;;
(define-module (tuile issues)
  #:use-module (tuile pr)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 exceptions) #:select (make-exception
                                             make-external-error
                                             make-exception-with-message
                                             make-exception-with-irritants
                                             exception-message
                                             exception-irritants
                                             make-warning
                                             make-error
                                             make-non-continuable-error
                                             warning?
                                             error?
                                             non-continuable-error?))
  #:export
  (
   issue-format
   issue-set-header

   issue-count
   issue-reset

   take-issue
   issue-raise

   issue-info
   issue-warning
   issue-problem
   issue-error

   fatal-issue

   issue-handle
   issue-handle-thunk
   ))


;; Return the current format.
(define issue-format (make-parameter "@1e:\n@4m"))

;; Set issue format.
(define (issue-set-header header)
  (issue-format header))

(define *issue-count* 0)

;; Return Issue Count.
(define (issue-count) *issue-count*)

;; Reset Issue Count to 0.
(define (issue-reset)
  (set! *issue-count* 0))

;; Turn non-zero accumulation of issues into reset.
(define (take-issue)
  (when (> *issue-count* 0)
    (let ((issue-count *issue-count*))
      (issue-reset)
      (issue-error (si "Errors found (#{issue-count})...")))))


;; Build complete message from optionally multiple parts.
(define (build-message message with-parts)
  (if (not message)
      #f
      (apply ss (cons message with-parts))))


;; Format user message according to format specification.
(define (format-message message . opt-error)
  (let ((error-message (match opt-error
                         ((error-msg) error-msg)
                         (else #f))))
    (let lp ((chars (string->list (issue-format)))
             (ret '()))
      (if (pair? chars)
          (let ((char (car chars)))
            (case char
              ((#\@) (if (pair? (cdr chars))
                         ;; There is atleast one char after @.
                         (let* ((space-count (lambda (chars)
                                               (if (null? chars)
                                                   0
                                                   (string->number (list->string chars)))))
                                (get-spaces-and-rest (lambda ()
                                                       (let lp ((chars (cdr chars))
                                                                (ret '()))
                                                         (if (pair? chars)
                                                             (let ((char (car chars)))
                                                               (cond
                                                                ((char-numeric? char) (lp (cdr chars)
                                                                                          (cons char ret)))
                                                                (else (cons (space-count (reverse ret)) chars))))
                                                             (cons "" chars)))))
                                (do-error (lambda (error-message spaces rest)
                                            (if error-message
                                                (lp rest
                                                    (append (reverse (string->list error-message))
                                                            (make-list spaces #\ )
                                                            ret))
                                                (lp rest ret))))
                                (do-message (lambda (message spaces rest)
                                              (define (prefix-message spaces message)
                                                (let* ((lines (string-split message #\newline))
                                                       (prefixed (map (lambda (line) (string-append (make-string spaces #\ )
                                                                                                    line))
                                                                      lines)))
                                                  (string-join prefixed "\n")))
                                              (lp rest
                                                  (append (reverse (string->list (prefix-message spaces message)))
                                                          ret))))
                                (next (cadr chars)))
                           (cond
                            ((char=? next #\@) (lp (cddr chars)
                                                   (cons #\@ ret)))
                            ((char=? next #\e) (do-error error-message 0 (cddr chars)))
                            ((char=? next #\m) (do-message message 0 (cddr chars)))
                            ((char-numeric? next) (let ((spaces-and-rest (get-spaces-and-rest)))
                                                    (case (car (cdr spaces-and-rest))
                                                      ((#\e) (do-error error-message (car spaces-and-rest) (cddr spaces-and-rest)))
                                                      ((#\m) (do-message message (car spaces-and-rest) (cddr spaces-and-rest)))
                                                      (else (lp (cdr chars) ; Failure, output as is.
                                                                (cons #\@ ret))))))
                            (else (lp (cdr chars)
                                      (cons #\@ ret)))))
                         (lp (cdr chars)
                             (cons #\@ ret))))
              (else (lp (cdr chars)
                        (cons char ret)))))
          (list->string (reverse (cons #\newline ret)))))))


;; Raise Issue Exception with message.
;;
;; Type is: info, warn, problem, or error.
;;
(define (issue-raise type message)
  (raise-exception (make-exception (make-exception-with-irritants type)
                                   (make-exception-with-message message))
                   #:continuable? #t))

;; Issue info exception with message.
(define (issue-info message . with-parts)
  (issue-raise 'info (build-message message with-parts)))

;; Issue warning exception with message.
(define (issue-warning message . with-parts)
  (issue-raise 'warning (build-message message with-parts)))

;; Issue problem exception with message.
;;
;; Increase Issue Count.
;;
(define (issue-problem message . with-parts)
  (set! *issue-count* (1+ *issue-count*))
  (issue-raise 'problem (build-message message with-parts)))

;; Issue error exception with message.
;;
;; By default exists the program, but can be also catched with
;; 'issue-handle-thunk'.
;;
(define (issue-error message . with-parts)
  (issue-raise 'error (build-message message with-parts)))

;; Report issue.
(define (issue-report message)
  (when message
    (display (format-message message #f) (current-error-port))))

;; Report issue and exit.
(define (issue-report-error message)
  (when message
    (display (format-message message "ERROR") (current-error-port)))
  (primitive-exit 1))

;; Report fatal issue and exit.
(define (fatal-issue message . with-parts)
  (when message
    (display (format-message (build-message message with-parts) "FATAL ERROR") (current-error-port)))
  (primitive-exit 1))

;; Handle exceptions raised from thunk.
;;
;; User can specify the list of exception types to ignore in
;; 'ignore-types'. The default list is: info, warning, problem.
;;
(define (issue-handle-thunk thunk . ignore-types)
  (with-exception-handler
      (lambda (exn)
        (let ((ignore-types (match ignore-types
                              ((types) types)
                              (else (list 'info 'warning 'problem)))))
          (if (member (exception-irritants exn) ignore-types)
              (issue-report (exception-message exn))
              (if (list? (exception-irritants exn))
                  ;; Handle "exit" etc. like this...
                  (raise-exception exn)
                  (issue-report-error (exception-message exn))))))
    thunk))

;; Handle statements with possible exceptions.
;;
;;     (issue-handle (display "hello world!\n"))
;;
(define-syntax issue-handle
  (lambda (x)
    (syntax-case x ()
      ((_ body ...)
       #`(issue-handle-thunk (lambda () body ...))))))


;; (define header "my prog@1e:\n@4m")
;; (issue-set-header header)
;; (issue-handle
;;  (pr "hello")
;;  (issue-problem "Hello world!\nAnd others!!!")
;;  (take-issue))
