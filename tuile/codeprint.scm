(define-module (tuile codeprint)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:export
  (
   codeprint-open
   codeprint-do
   codeprint-close
   ))

;; For debugging:
;;(use-modules (tuile pr))

;; ------------------------------------------------------------
;; Code printing library:
;;
;; Codeprinter takes commands and prints out text.
;;
;; Codeprinter state is returned if "codeprinter" is called without
;; arguments.
;;
;; API:
;;     codeprint-open <filename>
;;         Return "codeprinter".
;;
;;     codeprint-do   <filename> <commands>
;;         Perform printing for given file with commands.
;;
;;     codeprint-close <codeprinter>
;;         Close "codeprinter".
;;
;; Commands:
;;     p Print (line or lines or line-list)
;;     i Increment indent
;;     d Decrement indent
;;     r Reset indent
;;     m Modify indent step
;;     s Return indent string
;;     c Close output port
;;
;; Example:
;;
;;     (define c (codeprint-open "foobar.v"))
;;     (c 'i 'p "line of text")
;;     (c 'p "line of text" 'i)
;;     (c 'p "line of text")
;;     (codeprint-close c)


;; Codeprinter state.
(define-mu-record codeprint port indent-step indent)


(define (close cp)
  (unless (equal? (current-output-port)
                  (codeprint-port cp))
    (close-port (codeprint-port cp))))


(define (indent-spaces cp)
  (make-string (codeprint-indent cp) #\ ))


(define (update-line cp line)
  (if (or (not line) (string=? line ""))
      ""
      (string-append (indent-spaces cp) line)))


(define (print cp line-or-lines)
  (let ((lines (if (list? line-or-lines)
                   line-or-lines
                   (list line-or-lines))))
    (let loop ((lines lines))
      (when (pair? lines)
        (display (update-line cp (car lines)) (codeprint-port cp))
        (newline (codeprint-port cp))
        (loop (cdr lines))))))


(define (inc-indent cp count)
  (set-codeprint-indent! cp (+ (codeprint-indent cp) (* count (codeprint-indent-step cp)))))


(define (dec-indent cp count)
  (let ((new-indent (- (codeprint-indent cp) (* count (codeprint-indent-step cp)))))
    (if (< new-indent 0)
        (set-codeprint-indent! cp 0)
        (set-codeprint-indent! cp new-indent))))


;; Codeprinter function (interpreter).
(define (codeprint-fn cp args)
  (define (arg? args) (and (pair? args) (not (symbol? (car args)))))
  (if (pair? args)
      (let loop ((args args))
        (when (pair? args)
          (let ((arg (car args)))
            (case arg
              ((close)
               (close cp)
               (loop (cdr args)))
              ((p)
               ;; Normal indented line print.
               (if (not (arg? (cdr args)))
                   (begin
                     (newline (codeprint-port cp))
                     (loop (cdr args)))
                   (let p-loop ((args (cdr args)))
                     (if (arg? args)
                         (begin
                           (print cp (car args))
                           (p-loop (cdr args)))
                         (loop args)))))
              ((s)
               ;; Line list printing with separator: 's <lst> <sep>.
               (let ((args (cadr args))
                     (sep (caddr args)))
                 (cond
                  ((null? args) #f)
                  ((null? (cdr args))
                   (print cp (car args)))
                  (else
                   (display (update-line cp (car args)) (codeprint-port cp))
                   (let ((sep (string-append sep "\n" (indent-spaces cp))))
                     (let s-loop ((args (cdr args)))
                       (if (pair? args)
                           (begin
                             (display (ss sep (car args)) (codeprint-port cp))
                             (s-loop (cdr args)))
                           (newline (codeprint-port cp)))))))))
              ((i)
               (let ((args (cdr args)))
                 (if (arg? args)
                     (begin
                       (inc-indent cp (car args))
                       (loop (cdr args)))
                     (begin
                       (inc-indent cp 1)
                       (loop args)))))
              ((d)
               (let ((args (cdr args)))
                 (if (arg? args)
                     (begin
                       (dec-indent cp (car args))
                       (loop (cdr args)))
                     (begin
                       (dec-indent cp 1)
                       (loop args)))))
              ((r)
               (set-codeprint-indent! cp 0)
               (loop (cdr args)))
              ((m)
               (set-codeprint-indent-step! cp (car args))
               (loop (cdr args)))))))
      cp))


(define (create-codeprint filename indent-step)
  (make-codeprint (if (string=? "<stdout>" filename)
                                (current-output-port)
                                (open-output-file filename))
                            indent-step
                            0))


(define* (codeprint-open filename #:key (indent-step 3))
  (let ((cp (create-codeprint filename indent-step)))
    (lambda args
      (codeprint-fn cp args))))


(define* (codeprint-do filename commands #:key (indent-step 3))
  (let ((cp (make-codeprint (if (string=? "<stdout>" filename)
                                (current-output-port)
                                (open-output-file filename))
                            indent-step
                            0)))
    (codeprint-fn cp commands)))


(define (codeprint-close cp-fn)
  (close (cp-fn)))


;;(define c (codeprint-open "<stdout>"))
;;(c 'i 'p "line of text 1")
;;(c 'p "line of text 2" "line of text 3" 'i)
;;(c 'p "line of text 4")
;;(c 'd 'p "line of text 5")
;;(c 'r 'p "line of text 6")
;;(codeprint-close c)

;;(codeprint-do "<stdout>"
;;              'i 'p "line of text 1"
;;              'p "line of text 2" "line of text 3" 'i
;;              'p "line of text 4"
;;              'd 'p "line of text 5"
;;              'r 'p "line of text 6")
