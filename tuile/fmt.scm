(define-module (tuile fmt)
  #:use-module (common base)
  #:use-module ((srfi srfi-1)  #:select (first second third fold drop))
;;  #:use-module ((srfi srfi-9)  #:select (define-record-type))
;;   #:use-module (tuile compatible)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((tuile utils) #:select (delete-nth))
  #:export
  (
   fmt
   fmt-group

   fmt-info-rec
   fmt-info
   ))


;; 0123456789012301234567890123012345678901230
;; |      :      |      :      |      :      |
;; foo                 bar                 dii  ; left-align, center-align, right-align
;; foo       bar       dii                      ; gap with 7

;; Formatters (with space as default pad):
;;
;; ind - indent
;; lal - left-align
;; ral - right-align
;; cal - center-align
;; laf - left-align-fill
;; raf - right-align-fill
;; caf - center-align-fill
;; gap - gap
;; cat - concatenate

;; Converters (with zero as default pad):
;;
;; bin - binary code (default pad: 0)
;; oct - octal  code (default pad: 0)
;; hex - hex    code (default pad: 0)
;; dec - decimal     (default pad: #\ )



;; ------------------------------------------------------------
;; Internal functions:

;; Convert arg to string.
(define (->str arg)
  (cond
   ((string? arg)
    arg)
   ((number? arg)
    (object->string arg))
   ((symbol? arg)
    (object->string arg))
   ((list? arg)
    (string-concatenate (map ->str arg)))
   ((boolean? arg)
    (if arg "true" "false"))))

;; Tail recursive full flatten.
(define (flatten . rest)
  (let lp ((stack rest)
           (res '()))
    (cond
     ((null? stack)
      ;; We are done, reverse the result.
      (reverse res))
     ((null? (car stack))
      ;; Eat out empty tails.
      (lp (cdr stack)
          res))
     ((pair? (car stack))
      ;; Convert stack into: (head tail rest-of-stack).
      (lp (cons (caar stack)
                (cons (cdar stack)
                      (cdr stack)))
          res))
     (else
      ;; Add head to result.
      (lp (cdr stack)
          (cons (car stack) res))))))


;; ------------------------------------------------------------
;; User API:

(define (fmt . args)

  (define format-commands '(ind
                            lal
                            ral
                            cal
                            gap
                            cat
                            bin
                            oct
                            hex
                            dec))

  (define (format-atom atom)

    (define (string-repeat n str)
      (fold string-append "" (make-list n str)))

    (define (ind atom)
      (match atom
        ((num char) (make-string num (car (string->list char))))
        (num (make-string num #\ ))))

    (define (left-align-or-clip str width pad-elem)
      (if (< (string-length str) width)
          (string-append str
                         (string-repeat (- width
                                           (string-length str))
                                        pad-elem))
          (substring str 0 width)))

    (define (right-align-or-clip str width pad-elem)
      (if (< (string-length str) width)
          (string-append (string-repeat (- width
                                           (string-length str))
                                        pad-elem)
                         str)
          (let ((excess (- (string-length str) width)))
            (substring str
                       excess
                       (+ width excess)))))

    (define (center-align-or-clip str width pad-elem)
      (if (< (string-length str) width)
          (let* ((len (string-length str))
                 (left-pad (quotient (- width len) 2)))
            (string-append (string-repeat left-pad pad-elem)
                           str
                           (string-repeat (- width left-pad len) pad-elem)))
          (substring str 0 width)))

    (define (num-to-string radix pad rest)
      (let-values (((width pad num) (match rest
                                      (((width pad) num) (values width pad num))
                                      ((width num) (values width pad num))
                                      ((num) (values (string-length (number->string num radix))
                                                     pad
                                                     num)))))
        (right-align-or-clip (number->string num radix)
                             width
                             pad)))


    ;; Convert number to binary.
    ;;
    ;;     '(bin 123)
    ;;     '(bin 10 123)
    ;;     '(bin (10 " ") 123)
    ;;
    (define (bin rest)
      (num-to-string 2 "0" rest))

    (define (hex rest)
      (num-to-string 16 "0" rest))

    (define (oct rest)
      (num-to-string 8 "0" rest))

    (define (dec rest)
      (num-to-string 10 " " rest))

    ;; Return values: str/str-list, width, pad.
    (define (handle-align-or-clip-args atom fill)
      (apply values (cons (if (= (length atom) 3)
                              (if (list? (third atom))
                                  ;; List argument.
                                  (string-concatenate (format-atom (third atom)))
                                  ;; Single argument.
                                  (third atom))
                              ;; Many arguments.
                              (map format-atom (drop atom 2)))
                          (if (pair? (second atom))
                              (second atom)
                              (list (second atom) (string fill))))))
    
    (define (call-align fn atom fill)
      (let-values (((str-def width pad) (handle-align-or-clip-args atom fill)))
        (if (list? str-def)
            (map (lambda (str)
                   (fn (->str str) width pad))
                 str-def)
            (fn (->str str-def) width pad))))

    ;; Separate fields with sized gap.
    ;;
    ;;     (gap 4 "foo" "bar")
    ;;     (gap (4 "-") "foo "bar")
    ;;
    (define (gap rest)
      (let-values (((size char fields) (match rest
                                         (((size char) field ...) (values size char field))
                                         ((size field ...) (values size " " field)))))
        (string-join (flatten (map format-atom (cdr rest)))
                     (string-repeat size char))))

    (cond

     ((list? atom)

      (case (first atom)

        ((ind)
         (ind (second atom)))

        ((lal)
         (call-align left-align-or-clip atom #\ ))

        ((ral)
         (call-align right-align-or-clip atom #\ ))

        ((cal)
         (call-align center-align-or-clip atom #\ ))

        ((laf)
         (call-align left-align-or-clip (delete-nth atom 2) (third atom)))

        ((raf)
         (call-align right-align-or-clip (delete-nth atom 2) (third atom)))

        ((caf)
         (call-align center-align-or-clip (delete-nth atom 2) (third atom)))

        ((gap)
         (gap (cdr atom)))

        ((bin)
         (bin (cdr atom)))

        ((hex)
         (hex (cdr atom)))

        ((oct)
         (oct (cdr atom)))

        ((dec)
         (dec (cdr atom)))

        ((cat)
         (map fmt (cdr atom)))

        (else
         (map fmt atom))))

     (else
      (->str atom))))

  (define (format-atoms atoms)
    (if (pair? atoms)
        (cons (format-atom (car atoms))
              (format-atoms (cdr atoms)))
        '()))

  (define (format-shorthand atoms)

    (define (group-atoms atoms)
      (let lp ((items '())
               (groups '())
               (atoms atoms))
        (cond

         ((null? atoms)
          (drop (reverse (cons (reverse items)
                               groups))
                1))

         ((member (car atoms)
                  format-commands)
          (lp (list (car atoms))
              (cons (reverse items)
                    groups)
              (cdr atoms)))

         (else
          (lp (cons (car atoms)
                    items)
              groups
              (cdr atoms))))))

    (if (and (symbol? (first atoms))
             (member (first atoms)
                     format-commands))
        (group-atoms atoms)
        atoms))

  (string-concatenate (flatten (format-atoms (format-shorthand args)))))


(define-record-type fmt-info-rec
  (fields count
          max
          min
          total))


(define (fmt-info . args)

  (define (get-fmt-info args)

    (define (find-length-prop cmp args)
      (let find ((prop (string-length (car args)))
                 (tail (cdr args)))
        (if (pair? tail)
            (let ((len (string-length (car tail))))
              (if (cmp len prop)
                  (find len  (cdr tail))
                  (find prop (cdr tail))))
            prop)))

    (let ((str-args (map ->str args)))
      (make-fmt-info-rec (length args)
                         (find-length-prop > str-args)
                         (find-length-prop < str-args)
                         (fold (lambda (i s)
                                 (+ s (string-length i)))
                               0
                               str-args))))

  (if (list? (car args))
      ;; List argument.
      (get-fmt-info (car args))
      (get-fmt-info args)))


;; Format multiple lines with the given "format".
;;
;; "format" includes a formatting rule for each column, and the
;; remaining line columns are output as is.
;;
;; Example:
;;
;;     (fmt-group '((lal 6) (lal 12))
;;                 '(("#" "foo" "First dummy name.")
;;                   ("#" "bar" "Second dummy name.")))
;;
(define (fmt-group format lines)
  (let lp ((lines lines)
           (ret '()))
    (if (pair? lines)
        ;; Example:
        ;;     (fmt '(lal 6 "#") '(lal 12 "foo") "First dummy name.")
        (lp (cdr lines)
            (cons (apply fmt (let lp2 ((rest format)
                                       (cols (car lines))
                                       (ret '()))
                               (if (pair? rest)
                                   (if (eq? (caar rest) 'ind)
                                       (lp2 (cdr rest)
                                            cols
                                            (append ret
                                                    (list (append (car rest)))))
                                       (lp2 (cdr rest)
                                            (cdr cols)
                                            (append ret
                                                    (list (append (car rest) (list (->str (car cols))))))))
                                   (append ret cols))))
                  ret))
        (reverse ret))))


;; (use-modules (tuile pr))
#;
(when #t
  (pr (fmt `(ind 10) `(cal 12 "foobar") `(put "--")))
  (pr (fmt `(ind 10) `(caf 12 #\* "foobar") `(put "--")))
  (pr (fmt '(bin 12 "-" 6)))
  (pr (fmt '(gap 4 "foo" "bar")))
  (pr (fmt `(raf 10 #\- "x"))))

;; (pr (fmt `(ral (5 "-") "foo")))
;; (pr (fmt `(ral (5 "-") 123)))
