(define-module (tuile pr)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module (rnrs records inspection)
  #:use-module (rnrs records procedural)
  #:export
  (pr
   pre
   pl
   prp
   pro
   prpo
   prd
   prdl
   ss
   sj
   sp
   si
   pri
   ds
   pd
   pde
   pp
   ppe
   ppr
   ppre
   ippr
   :lj
   :rj
   :cj
   :ls
   :rs
   :lc
   :rc
   :jn
   :sp
   :ms
   :in
   :nl
   :ow

   mp

   expand-string-interpolation
   ))


;; ------------------------------------------------------------
;; Internal functions:

;; Flatten argument list to arbitraty depth.
(define (flat-args . args)
  (if (pair? args)

      (cond
       ((list? (car args))
        ;; (format #t "list?\n")
        (append (apply flat-args (car args)) (apply flat-args (cdr args))))
       ((null? (car args))
        ;; (format #t "null?\n")
        '())
       ((unspecified? (car args))
        (flat-args (cdr args)))
       (else
        ;; (format #t "else\n")
        (cons (car args) (flat-args (cdr args)))))

      '()))


;; Convert obj to string type.
(define (to-string obj)
  (cond
   ((string? obj) obj)
   ((char? obj) (string obj))
   (else (object->string obj))))

;; Macro to flatten args and convert all arguments to strings.
(define-syntax fa
  (syntax-rules ()
    ((_ args)
     (map to-string (apply flat-args args)))))


;; Char or string argument as char.
(define (ch-or-str-as-ch arg)
  (if (char? arg)
      arg
      (car (string->list arg))))


;; Char or string argument as string.
(define (ch-or-str-as-str arg)
  (if (string? arg)
      arg
      (make-string 1 arg)))



;; ------------------------------------------------------------
;; External functions:


;; String from args.
(define (ss . args)
  (apply string-append (fa args)))

;; String from args.
(define (sj . args)
  (string-join (fa args) " "))

(define (sp datum)
  (with-output-to-string (lambda ()
                           (pretty-print datum))))


;; Expand interpolation for a string.
;;
;; "Hello #{my-friend}!" -> ("Hello " my-friend "!")
;;
(define (expand-string-interpolation str)

  (define (update-words words word)
    (if (pair? word)
        (cons (list->string (reverse word)) words)
        words))

  (let loop ((i 0)
             (words '())
             (word '())
             (state 'in-string))

    (if (>= i (string-length str))

        ;; Done.
        (reverse (update-words words word))

        ;; Continue.
        (let ((ch (string-ref str i)))

          (case state

            ((in-string)
             (cond

              ;; Escape.
              ((char=? ch #\\)
               (loop (+ i 2)
                     words
                     (cons (string-ref str (+ i 1)) word)
                     state))

              ;; Interpolation (potentially).
              ((char=? ch #\#)
               (if (char=? (string-ref str (+ i 1))
                           #\{)
                   ;; Interpolation.
                   (loop (+ i 2)
                         (update-words words word)
                         '()
                         'in-interpolation)
                   (loop (+ i 1)
                         words
                         (cons ch word)
                         state)))

              (else
               (loop (+ i 1)
                     words
                     (cons ch word)
                     state))))

            ((in-interpolation)

             (cond

              ;; Escape.
              ((char=? ch #\\)
               (loop (+ i 2)
                     words
                     (cons (string-ref str (+ i 1)) word)
                     state))

              ;; Terminate interpolation.
              ((char=? ch #\})
               (let ((expr (read (open-input-string
                                  (list->string (reverse word))))))
                 (loop (+ i 1)
                       (cons expr words)
                       '()
                       'in-string)))

              (else
               (loop (+ i 1)
                     words
                     (cons ch word)
                     state)))))))))


;; String interpolation (ruby style).
;;
;;     (si "Hello #{my-friend}!")
;;
(define-syntax si
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(ss #,@(datum->syntax x (expand-string-interpolation (cadr stx)))))))


(define-syntax pri
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(pr #,@(datum->syntax x (expand-string-interpolation (cadr stx)))))))

;; (define-syntax-rule (pri s ...)
;;   (pr (si s ...)))


;; Datum to string.
(define (ds datum)
  (with-output-to-string (lambda ()
                           (write datum))))

(define (pd datum)
  (pr (ds datum))
  datum)

(define (pde . datums)
  (for-each pd datums)
  (exit 0))

(define (pretty-print-including-records rec . initial-indent)
  (define indent-step 2)
  (define (space indent) (make-string indent #\ ))
  (define (print-with-indent rec indent)
    (define-syntax-rule (next-indent) (+ indent indent-step))
    (cond
     ((list? rec)
      (pr (space indent) "(")
      (for-each (lambda (datum) (print-with-indent datum (next-indent))) rec)
      (pr (space (next-indent)) ")"))
     ((vector? rec)
      (pr (space indent) "[")
      (for-each (lambda (datum) (print-with-indent datum (next-indent))) (vector->list rec))
      (pr (space (next-indent)) "]"))
     ((record? rec)
      (let ((rtd (record-rtd rec)))
        ;; (pr "IS REC")
        (pr (space indent) (record-type-name rtd) " {")
        (for-each (lambda (name-value)
                    (pr (space (next-indent)) (car name-value) ":")
                    (print-with-indent (cdr name-value) (+ indent (* indent-step 2))))
                  (let loop ((fields (vector->list (record-type-field-names rtd)))
                             (index 0)
                             (ret '()))
                    (if (pair? fields)
                        (loop (cdr fields)
                              (1+ index)
                              (cons (cons (car fields)
                                          ((record-accessor rtd index) rec))
                                    ret))
                        (reverse ret))))
        (pr (space (next-indent)) "}")))
     (else
      ;; (pr "NOT REC")
      (pretty-print rec #:per-line-prefix (space indent)))))
  (print-with-indent rec (if (pair? initial-indent) (car initial-indent) 0)))

(define (pp . datums)
  (for-each pretty-print-including-records datums))

(define (ppe . datums)
  (apply pp datums)
  (exit 0))

(define (ppr . datums)
  (for-each pretty-print datums)
  (car datums))

(define (ppre . datums)
  (apply ppr datums)
  (exit 0))

(define (ippr datum)
  (pretty-print datum)
  datum)

;; Line print from args.
(define (pr . args)
  (display (apply ss args))
  (newline)
;;  (display "flush")
;;  (newline)
;;  (force-output)
  )

;; Line print from args to stderr.
(define (pre . args)
  (display (apply ss args) (current-error-port))
  (newline (current-error-port)))

;; Print lines from args.
(define (pl args)
  (for-each pr args))


;; Print from args.
(define (prp . args)
  (display (apply ss args)))


;; Line print from args to port.
(define (pro port . args)
  (display (apply ss args) port)
  (newline port))


;; Print from args.
(define (prpo port . args)
  (display (apply ss args) port))


;; Printer for debugging. Alias to pr.
(define prd pr)


;; Printer for debugging a list.
(define (prdl lst)
  (for-each (lambda (item)
              (pr item))
            lst))


;; Left justify with padding.
;;
;; left-just <width> <pad-str-or-ch> <strings>
(define (:lj width pad . rest)
  (let* ((pad-ch (ch-or-str-as-ch pad))
         (str (string-concatenate (fa rest)))
         (pad-cnt (if (< (string-length str) width)
                      (- width (string-length str))
                      0)))
    (string-append str (make-string pad-cnt pad-ch))))


;; Right justify with padding.
;;
;; right-just <width> <pad-str-or-ch> <strings>
(define (:rj width pad . rest)
  (let* ((pad-ch (ch-or-str-as-ch pad))
         (str (string-concatenate (fa rest)))
         (pad-cnt (if (< (string-length str) width)
                      (- width (string-length str))
                      0)))
    (string-append (make-string pad-cnt pad-ch) str)))


;; Center justify with padding.
;;
;; center-just <width> <pad-str-or-ch> <strings>
(define (:cj width pad . rest)
  (let* ((str (string-concatenate (fa rest)))
         (len (string-length str)))
    (if (>= len width)
        str
        (let* ((pad-ch (ch-or-str-as-ch pad))
               (lpad-cnt (quotient (- width len) 2))
               (rpad-cnt (- width len lpad-cnt)))
          (string-append (make-string lpad-cnt pad-ch)
                         str
                         (make-string rpad-cnt pad-ch))))))


;; Left-justify with space.
(define (:ls width . rest)
  (:lj width #\  rest))


;; Right-justify with space.
(define (:rs width . rest)
  (:rj width #\  rest))


(define (align-or-clip-with fn width pad rest)
  (let ((str (string-concatenate (fa rest))))
    (if (> (string-length str)
           width)
        (substring str 0 width)
        (apply fn (append (list width pad)
                          rest)))))

;; Left-just-clip with pad.
(define (:lc width pad . rest)
  (align-or-clip-with :lj width pad rest))

;; Right-just-clip with pad.
(define (:rc width pad . rest)
  (align-or-clip-with :rj width pad rest))


;; Right-just-clip with pad.
#;
(define (:rc width pad . rest)
  (let ((str (string-concatenate (fa rest))))
    (if (> (string-length str)
           width)
        (let ((off (- (string-length str) width)))
          (substring str off (+ width off)))
        (apply :rj (append (list width pad)
                           rest)))))


;; Join with given string (or char).
(define (:jn join-str-or-char . rest)
  (string-join (fa rest) (ch-or-str-as-str join-str-or-char)))


;; Join with space.
(define (:sp . args)
  (string-join (fa args) " "))


;; Make string from template (string or char).
(define (:ms count char-or-str)
  (if (char? char-or-str)
      (make-string count char-or-str)
      (string-concatenate (make-list count char-or-str))))


;; Space indentation by count.
(define (:in count)
  (make-string count #\ ))

;; Newline string.
(define :nl "\n")


;; String overwrite with list of cons arguments. Each cons is a
;; (<index> . <overwrite>) pair.
(define (:ow str . rest)
  (if (or (null? rest)
          (not (car rest)))
      str
      (let loop ((mod rest)
                 (str str))
        (if (pair? mod)
            (loop (cdr mod)
                  (string-append (substring str
                                            0
                                            (caar mod))
                                 (cdar mod)
                                 (substring str
                                            (+ (caar mod) (string-length (cdar mod))))))
            str))))

;; Macro print, i.e. print syntax as datum.
(define (mp m)
  (ppre (syntax->datum m)))
