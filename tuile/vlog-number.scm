;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2025
;;;; All rights reserved.


;;; module:
;;
;; 'vlog-number' provides a container and the related conversion
;; procedures for numbers used in RTL.
;;
;; For example, this is the textual representation of signed integer
;; with value '-123' and width of '14' bits (including sign):
;;
;;     14sd-123
;;
;; and the corresponding 'vlog-number' is:
;;
;;    (define-record-type vlog-number
;;      (fields value                         ; Number value.
;;              sign                          ; Signess (unsigned, signed).
;;              width                         ; Bit width.
;;              base                          ; Base.
;;              ))
;;
;; Conversion to 'vlog-number' can be performed from:
;;
;; * int: a plain integer number.
;;
;; * him: hdl-in-memory formatted number.
;;
;; * vlog: vlog formatted number string.
;;
(define-module (tuile vlog-number)
;;   #:use-module (rllib utils)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile basic)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile fmt)
  #:use-module (ice-9 match)
  #:export (
            vlog-number
            make-vlog-number

            vlog-number-value
            vlog-number-sign
            vlog-number-width
            vlog-number-base

            vlog-number-uint?
            vlog-number-length

            vlog-number-from-numstr
            vlog-number-from-him
            vlog-number-from-int
            vlog-number-to-numstr
            vlog-number-to-quoted-numstr
            vlog-number-to-him

            vlog-number-signify
            ))


;;
;; vlog-number: record
;;
;; alias: vnum
;;
;;     '((value  . 0)
;;       (sign   . unsigned)
;;       (width  . #f)
;;       (base   . 10))
;;
(define-record-type vlog-number
  (fields value                         ; Number value.
          sign                          ; Signess (unsigned, signed).
          width                         ; Bit width.
          base                          ; Base.
          ))


(define (vlog-number-uint? number)
  (and (vlog-number-value number)
       (not (vlog-number-sign number))
       (not (vlog-number-width number))
       (vlog-number-base number)))


;; Return width if present, otherwise calculate the number of bits
;; needed to represent the value.
(define (vlog-number-length number)
  (aif (vlog-number-width number)
       it
       (let ((value (vlog-number-value number)))
         (case (vlog-number-sign number)
           ((signed) (if (< value 0)
                         (let* ((abs (- value))
                                (len (integer-length abs)))
                           (if (= abs (expt 2 (1- len)))
                               len
                               (1+ len)))
                         (1+ (integer-length value))))
           ((unsigned) (integer-length value))))))

;;;
;;
;; Return vlog-number from Number String (numstr).
;;
;;     14h1AB7
;;     14sd-123
;;     4b1010
;;     4'sb1010
;;     -123
;;
;; params:
;;
;;     numstr      Number description as 'string'.
;;
;;
;; TODO:
;; * Check for conflicts in width.
;; * Check for negation.
;;
(define (vlog-number-from-numstr numstr)

  ;; Convert numstr to list of characters with removal of all
  ;; underscores ('_') and the optional leading ('#') used for numbers
  ;; without width.
  (define (import-numstr numstr)
    (filter (lambda (ch)
              (not (char=? ch #\_)))
            (string->list numstr)))

  ;; Convert value-string to number value with the help of other
  ;; number info. Return vlog-number or #f (for failure).
  (define (create-number value-string sign width base)

    (define (to-signed value width)
      (cond
       ((not width) value)
       ((> value (1- (expt 2 (1- width)))) (- (- (expt 2 width) value)))
       (else value)))

    (case base
      ((10) (let ((value (string->number value-string base)))
              (if (< value 0)
                  (make-vlog-number value
                                    'signed
                                    width
                                    base)
                  (make-vlog-number value
                                    sign
                                    width
                                    base))))
      (else (case sign
              ((signed) (make-vlog-number (to-signed (string->number value-string base)
                                                     width)
                                          sign
                                          width
                                          base))
              ((unsigned) (make-vlog-number (string->number value-string base)
                                            sign
                                            width
                                            base))
              (else #f)))))


  ;; (ppr numstr)

  (let lp ((chars (import-numstr numstr))
           (state 'width)
           (part '())
           (value #f)
           (sign #f)
           (width #f)
           (base #f))

    (if (pair? chars)

        (let ((ch (car chars)))

          (case state

            ;;     v
            ;;     14'sd-123
            ;; OR
            ;;     -100
            ;;
            ((width)
             (cond
              ((char-numeric? ch)
               (lp (cdr chars) state (cons ch part) value sign width base))
              ((char=? ch #\-)
               (lp (cdr chars) state (cons ch part) value sign width base))
              ((char=? ch #\#)
               (if (null? part)
                   (lp (cdr chars) 'opt-sign part value sign width base)
                   (lp chars 'opt-sign '() value sign (string->number (list->string (reverse part)) 10) base)))
              (else
               (lp chars 'opt-quote '() value sign (string->number (list->string (reverse part)) 10) base))))

            ;;       v
            ;;     14'sd-123
            ((opt-quote)
             (lp (if (char=? ch #\') (cdr chars) chars) 'opt-sign part value sign width base))

            ;;        v
            ;;     14'sd-123
            ((opt-sign)
             (case ch
               ((#\s) (lp (cdr chars) 'base part value 'signed width base))
               ((#\u) (lp (cdr chars) 'base part value 'unsigned width base))
               (else (lp chars 'base part value sign width base))))

            ;;         v
            ;;     14'sd-123
            ((base)
             (case ch
               ((#\d) (lp (cdr chars) 'value part value sign width 10))
               ((#\h) (lp (cdr chars) 'value part value sign width 16))
               ((#\o) (lp (cdr chars) 'value part value sign width 8))
               ((#\b) (lp (cdr chars) 'value part value sign width 2))
               ((#\#) (lp chars 'value part value sign width base))
               (else #f)))

            ;;          v
            ;;     14'sd-123
            ((value)
             (if (char=? ch #\#)
                 (lp (cdr chars) state part value sign width base)
                 (case base
                   ((2) (case ch
                          ((#\0 #\1)
                           (lp (cdr chars) state (cons ch part) value sign width base))
                          (else #f)))
                   ((16) (if (char-set-contains? char-set:hex-digit ch)
                             (lp (cdr chars) state (cons ch part) value sign width base)
                             #f))
                   ((8) (if (char-set-contains? char-set:digit ch)
                            (lp (cdr chars) state (cons ch part) value sign width base)
                            #f))
                   ((10) (if (or (char=? ch #\-)
                                 (char-set-contains? char-set:digit ch))
                             (lp (cdr chars) state (cons ch part) value sign width base)
                             #f))
                   (else #f))))))

        (let ((use-sign (if sign sign 'unsigned)))

          (cond

           ((and (not base)
                 (not width)
                 (not sign))
            ;; Pure integer (e.g. -123).
            (make-vlog-number (string->number (list->string (reverse part)) 10) #f #f 10))

           ((null? part)
            ;; Width and optional sign, only (e.g. 12s).
            (make-vlog-number #f
                              use-sign
                              width
                              base))

           ((not width)
            ;; Width and optional sign, only (e.g. sd10).
            (create-number (list->string (reverse part))
                           use-sign
                           #f
                           base))

           (else
            ;; Full number (e.g. 14'd123).
            (create-number (list->string (reverse part))
                           use-sign
                           width
                           base)))))))


;;;
;;
;; Create 'vlog-number' from him.
;;
;;     '(12 signed 8 10)
;;
;; params:
;;
;;     him         'him' number description.
;;
(define (vlog-number-from-him him)
  (apply make-vlog-number him))


;;;
;;
;; Create 'vlog-number' from plain integer.
;;
;; The 'vlog-number' has the given value and no other information except
;; base.
;;
;; params:
;;
;;     int         Value of 'vlog-number'.
;;
(define (vlog-number-from-int int)
  (make-vlog-number int #f #f 10))


;; Return Number String (numstr) from vlog-number.
(define (vlog-number-to-numstr-common number quote? pad?)

  (define (from-signed value width)
    (if (< value 0)
        (+ (expt 2 width) value)
        value))

  (define (vlog-number->string number base width pad?)
    (if (and pad? width)
        (case base
          ((2) (fmt (list 'bin width number)))
          ((16) (fmt (list 'hex (->hex-width width) number)))
          ((8) (fmt (list 'oct (->oct-width width) number))))
        (number->string number base)))

  (let* ((sign (vlog-number-sign number))
         (is-signed? (eq? sign 'signed))
         (width (vlog-number-width number))
         (base (vlog-number-base number))
         (value (vlog-number-value number)))
    (if (not sign)
        (number->string value base)
        (ss (list-specified (list (if (and quote? value (< value 0) (= base 10))
                                      "-"
                                      *unspecified*)
                                  (awhen width it)
                                  (if (and quote? value) "'")
                                  (if (and is-signed? value) "s")
                                  (case base
                                    ((2) "b")
                                    ((10) "d")
                                    ((16) "h")
                                    ((8) "o"))
                                  (case base
                                    ((2) (vlog-number->string (if is-signed?
                                                                  (from-signed value width)
                                                                  value)
                                                              base
                                                              width
                                                              pad?))
                                    ((10) (if (and quote? value)
                                              (number->string (abs value) base)
                                              (number->string value base)))
                                    ((16) (vlog-number->string (if is-signed?
                                                                   (from-signed value width)
                                                                   value)
                                                               base
                                                               width
                                                               pad?))
                                    ((8) (vlog-number->string (if is-signed?
                                                                  (from-signed value width)
                                                                  value)
                                                              base
                                                              width
                                                              pad?)))))))))


;;;
;;
;; Convert 'vlog-number' to numstr (number string).
;;
;;     "12sd10"
;;
;; params:
;;
;;     vlog-number 'vlog-number' to convert.
;;
(define (vlog-number-to-numstr number . opt-pad)
  (vlog-number-to-numstr-common number #f (match opt-pad ((pad) pad) (else #f))))


;;;
;;
;; Convert 'vlog-number' to quoted numstr (Verilog number string).
;;
;;     "12'sd10"
;;
;; params:
;;
;;     vlog-number 'vlog-number' to convert.
;;
(define (vlog-number-to-quoted-numstr number . opt-pad)
  (vlog-number-to-numstr-common number #t (match opt-pad ((pad) pad) (else #f))))


;;;
;;
;; Convert 'vlog-number' to him.
;;
;;     '(12 signed 8 10)
;;
;;
;; params:
;;
;;     number      'vlog-number' to convert.
;;
(define (vlog-number-to-him number)
  (list (vlog-number-value number)
        (vlog-number-sign  number)
        (vlog-number-width number)
        (vlog-number-base  number)))


(define (vlog-number-signify number)
  (if (symbol? (vlog-number-sign number))
      number
      (make-vlog-number
       (vlog-number-value number)
       (if (vlog-number-sign number)
           'signed
           'unsigned)
       (vlog-number-width number)
       (vlog-number-base number))))


;; (pp (vlog-number-from-numstr "#sb0"))
;; (pp (vlog-number-from-numstr "sb0"))
;; (pp (vlog-number-from-numstr "12s"))
;; (pp (vlog-number-from-numstr "-2"))
;; (pp (vlog-number-from-numstr "sd-10"))
;; (pp (vlog-number-from-numstr "d-10"))
;; (pp (vlog-number-from-numstr "8s"))
;; (pp (vlog-number-from-numstr "sd-10"))
;; (pp (vlog-number-from-numstr "12sd10"))
;; (pp (vlog-number-to-numstr (vlog-number-from-numstr "12h01AF")))
;; (pp (vlog-number-from-numstr "12sh01AF"))
;; (pp (vlog-number-to-numstr (vlog-number-from-numstr "12sh01AF")))
;; (pp (vlog-number-to-numstr (vlog-number-from-numstr "10b1010101100")))

;; (pp (vlog-number-to-quoted-numstr (vlog-number-from-him '(-1 signed 2 2))))

;; (pr (vlog-number-length (make-vlog-number -8 'signed #f 2)))

;; (ppr (vlog-number-from-numstr "12s#"))
;; (ppr (vlog-number-from-numstr "12sb#"))
;; (ppr (vlog-number-from-numstr "12#"))

;; (ppr (vlog-number-from-numstr "##"))
