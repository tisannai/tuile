(define-module (tuile fmt)
  #:use-module ((srfi srfi-1)  #:select (first second third fold drop))
  #:use-module ((srfi srfi-9)  #:select (define-record-type))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:export
  (
   fmt

   fmt-info-rec
   fmt-info
   ))


;; left-align (with-indent)
;; right-align (with-indent)
;; center-align
;; distribute (with-indent)
;; gap (with-indent)
;;
;; 0123456789012301234567890123012345678901230
;; |      :      |      :      |      :      |
;; foo                 bar                 dii  ; left-align, center-align, right-align
;; foo          bar          dii                ; distribute to 28
;; foo       bar       dii                      ; gap with 7

;; Formatters (with space as default pad):
;;
;; lal - left-align
;; ral - rigth-align
;; lai - left-align-with-indent
;; rai - rigth-align-with-indent
;; dis - distribute
;; gap - gap

;; Converters (with zero as default pad):
;;
;; bin - binary code (default pad: 0)
;; oct - octal  code (default pad: 0)
;; hex - hex    code (default pad: 0)
;; dec - decimal     (default pad: #\ )

;;(use-modules (tuile pr))


;; ------------------------------------------------------------
;; Internal functions:

(define (fmt-to-str arg)
  (cond
   ((string? arg)
    arg)
   ((number? arg)
    (object->string arg))
   ((symbol? arg)
    (object->string arg))
   ((list? arg)
    (string-concatenate (map fmt-to-str arg)))))


;; ------------------------------------------------------------=
;; User API:

(define (fmt . args)

  (define format-commands '(lal
                            ral
                            dis
                            gap
                            bin
                            oct
                            hex
                            dec))

  (define (format-atom atom)

    (define (string-repeat n str)
      (fold string-append "" (make-list n str)))

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

    (define (num-to-string radix pad rest)
      (if (= 1 (length rest))
          (let ((str (number->string (first rest) radix)))
            (right-align-or-clip str
                                 (string-length str)
                                 pad))
          (let-values (((width pad num) (if (pair? (first rest))
                                            (values (caar rest)
                                                    (cadar rest)
                                                    (second rest))
                                            (values (first rest)
                                                    pad
                                                    (second rest)))))
            (right-align-or-clip (number->string num radix)
                                 width
                                 pad))))


    ;; Convert number to binary.
    ;;
    ;; '(bin 123)
    ;; '(bin 10 123)
    ;; '(bin (10 " ") 123)
    (define (bin rest)
      (num-to-string 2 "0" rest))

    (define (hex rest)
      (num-to-string 16 "0" rest))

    (define (oct rest)
      (num-to-string 16 "0" rest))

    (define (dec rest)
      (num-to-string 10 " " rest))

    ;; Return values: str/str-list, width, pad.
    (define (handle-align-or-clip-args atom)
      (apply values (append (list (if (= (length atom) 3)
                                      (if (list? (third atom))
                                          ;; List argument.
                                          (map format-atom (third atom))
                                          ;; Single argument.
                                          (format-atom (third atom)))
                                      ;; Many arguments.
                                      (map format-atom (drop atom 2))))
                            (if (pair? (second atom))
                                (second atom)
                                (list (second atom) " ")))))

    (define (call-align fn atom)
      (let-values (((str-def width pad) (handle-align-or-clip-args atom)))
        (if (list? str-def)
            (string-concatenate (map (lambda (str)
                                       (fn str width pad))
                                     str-def))
            (fn str-def width pad))))

    (cond

     ((list? atom)

      (case (first atom)

        ;; Left-align.
        ((lal)
         (call-align left-align-or-clip atom))

        ((ral)
         (call-align right-align-or-clip atom))

        ((bin)
         (bin (cdr atom)))

        (else
         (apply fmt atom))))

     (else
      (fmt-to-str atom))))

  (define (format-atoms atoms)
    (if (pair? atoms)
        (cons (format-atom (car atoms))
              (format-atoms (cdr atoms)))
        '()))

  (define (format-shorthand atoms)

    (define (group-atoms atoms)
      (let loop ((items '())
                 (groups '())
                 (atoms atoms))
        (cond

         ((null? atoms)
          (drop (reverse (cons (reverse items)
                               groups))
                1))

         ((member (car atoms)
                  format-commands)
          (loop (list (car atoms))
                (cons (reverse items)
                      groups)
                (cdr atoms)))

         (else
          (loop (cons (car atoms)
                      items)
                groups
                (cdr atoms))))))

    (if (and (symbol? (first atoms))
             (member (first atoms)
                     format-commands))
        (group-atoms atoms)
        atoms))

  (string-concatenate (format-atoms (format-shorthand args))))


(define-record-type fmt-info-rec
  (make-fmt-info count max min total)
  fmt-info?
  (count fmt-info-count)
  (max   fmt-info-max)
  (min   fmt-info-min)
  (total fmt-info-total))


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

    (let ((str-args (map fmt-to-str args)))
      (make-fmt-info (length args)
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
