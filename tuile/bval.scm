(define-module (tuile bval)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (first second fold))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile re)
  #:export
  (
   bval-new
   bv
   bv=
   bv->fix

   bv-eq?
   bv-eq-type?
   bv-eq-topology?

   bv-sext
   bv-sext+
   bv-ext

   bv*
   bv+
   bv-

   bv->str
   ))


(when #f
  (use-modules ((rnrs records syntactic) #:select (define-record-type)))
  (use-modules (ice-9 match))
  (use-modules ((srfi srfi-1) #:select (first second fold)))
  (use-modules ((srfi srfi-11) #:select (let-values)))
  (use-modules (tuile utils))
  (use-modules (tuile pr))
  (use-modules (tuile re))
  )

;; ------------------------------------------------------------
;; Creation and modification:

(define-record-type bval
  (fields value                        ; Integer or fixed.
          size                         ; Number of bits, pair for fix.
          signed                       ; Signed/unsigned.
          format                       ; bin, hex, dec.
          ))

(define (bval-error msg)
  (display (ss "bval error: " msg "\n") (current-error-port))
  (raise-exception &error))

(define (bval-type bval)
  (if (pair? (bval-size bval)) 'fix 'int))

(define bval-size-width car)
(define bval-size-int cdr)

(define (bval-width bval)
  (case (bval-type bval)
    ((fix) (bval-size-width (bval-size bval)))
    (else (bval-size bval))))

(define (bval-width-int bval)
  (case (bval-type bval)
    ((fix) (bval-size-int (bval-size bval)))
    (else #f)))

(define (bval-width-frac bval)
  (case (bval-type bval)
    ((fix) (- (bval-size-width (bval-size bval)) (bval-size-int (bval-size bval))))
    (else #f)))

(define (bval-fixed? bval)
  (case (bval-type bval)
    ((fix) #t)
    (else #f)))

(define (bval-int-value bval)
  (case (bval-type bval)
    ((fix) (let ((frac (bval-width-frac bval)))
             (inexact->exact (* (bval-value bval) (expt 2 frac)))))
    (else (bval-value bval))))

(define (bval-range bval)
  (case (bval-type bval)
    ((fix) (let* ((width (bval-width bval))
                  (frac (bval-width-frac bval)))
             ;; fix
             (if (bval-signed bval)
                 (cons (/ (- (expt 2.0 (1- width)))
                          (expt 2 frac))
                       (/ (- (expt 2.0 (1- width)) 1)
                          (expt 2 frac)))
                 (cons 0.0
                       (/ (- (expt 2.0 width) 1)
                          (expt 2 frac))))))
    (else
     ;; non-fix
     (let ((width (bval-width bval)))
       (if (bval-signed bval)
           (cons (- (expt 2 (1- width)))
                 (- (expt 2 (1- width)) 1))
           (cons 0 (- (expt 2 width) 1)))))))

(define (bval-value-fits? bval value)
  (let ((range (bval-range bval)))
    (and (>= value (car range))
         (<= value (cdr range)))))

(define (bval-new value size . rest)
  (let* ((fixed? (pair? size))
         (default-format 'dec)
         (import-fixed-value (lambda (value size)
                               (let ((frac (expt 2 (- (bval-size-width size) (bval-size-int size)))))
                                 (if (exact? value)
                                     (exact->inexact value)
                                     (/ (floor (+ (* value frac) 0.5))
                                        frac))))))
    (let-values (((signed format) (match rest
                                    ((signed format) (values signed format))
                                    ((signed) (values signed default-format))
                                    (else (values #t default-format)))))
      (when (and (not signed) (< value 0))
        (bval-error "Unsigned value must be 0 or more."))
      (cond
       (fixed?
        (when (> (bval-size-int size) (bval-size-width size))
          (bval-error (ss "Integer portion of fix must be smaller than width")))
        (when (and signed
                   (< (bval-size-width size) 2))
          (bval-error (ss "Width of signed fix must be at least two"))))
       ((not (exact? value))
        (bval-error (ss "Value on non-fix number must be an integer")))
       ((<= size 0)
        (bval-error "Size must be greater than 0.")))
      (case format
        ((bin hex dec)
         (let ((temp (make-bval 0 size signed format)))
           (if (bval-value-fits? temp value)
               (if fixed?
                   (make-bval (import-fixed-value value size) size signed format)
                   (make-bval value size signed format))
               (bval-error (ss "Value overflow for " (bval-width temp) " bits, for value: \"" value "\"")))))
        (else (bval-error (ss "Invalid bval format: \"" format "\"")))))))


(define (bval->string bval . rest)

  (define (pad str width)
    (let ((len (string-length str)))
      (if (< len width)
          (ss (make-string (- width len) #\0)
              str)
          str)))

  (define (hex-nibbles width)
    (1+ (quotient (1- width) 4)))

  (define (bin-format bval)
    (number->string (if (< (bval-int-value bval) 0)
                        (+ (expt 2 (bval-width bval)) (bval-int-value bval))
                        (bval-int-value bval))
                    2))

  (let ((format (match rest
                  ((format) format)
                  (else (bval-format bval)))))

    (case format
      ((bin) (if (bval-fixed? bval)
                 (ss (bval-width bval)
                     (if (bval-signed bval) "s" "u")
                     (bval-width-int bval)
                     "b"
                     (let ((int-digit-limit (if (bval-signed bval) 1 0))
                           (explicit-sign (if (bval-signed bval)
                                              (if (< (bval-value bval) 0) #\- #\+)
                                              #\*))
                           (body (pad (bin-format bval) (bval-width bval)))
                           (int (bval-width-int bval)))
                       (if (< (bval-width-int bval) int-digit-limit)
                           (ss (string explicit-sign)
                               "."
                               (make-string (1- (- int)) explicit-sign)
                               body)
                           (ss (substring body 0 int)
                               "."
                               (substring body int)))))
                 (ss (bval-width bval)
                     (if (bval-signed bval) "sb" "ub")
                     (pad (bin-format bval) (bval-width bval)))))
      ((hex) (if (bval-fixed? bval)
                 (bval-error "Can't format fix-point number into hex string.")
                 (ss (bval-width bval)
                     (if (bval-signed bval) "sh" "uh")
                     (pad (number->string (bval-value bval) 16)
                          (hex-nibbles (bval-width bval))))))
      ((dec) (if (bval-fixed? bval)
                 (ss (bval-width bval)
                     (if (bval-signed bval) "s" "u")
                     (bval-width-int bval)
                     "d"
                     (number->string (bval-value bval) 10))
                 (number->string (bval-value bval)))))))

;; 12sb1010            - 12-bit wide signed binary, value = d10
;; b1010               - 4-bit wide (implicit) unsigned binary, value = d10
;; 16shabcd            - 16-bit wide unsigned hexadecimal, value = d-21555
;; habcd               - 16-bit wide (implicit) unsigned hexadecimal, value = d43981
;; d100                - 8-bit wide (implicit) signed decimal, value = d100
;; sd100               - 8-bit wide (implicit) signed decimal, value = d100
;; ud100               - 7-bit wide (implicit) unsigned decimal, value = d100
;; sd-100              - 8-bit wide (implicit) signed decimal, value = d-100
;; 10d100              - 10-bit wide signed decimal, value = d100
;; 10sd-100            - 10-bit wide signed decimal, value = d-100
;; 10d-100             - 10-bit wide signed decimal, value = d-100
;; 6s2d1.5625          - 6-bit wide signed fixed (Q2.4), value = d1.5625
;; 6s2b01.1001         - 6-bit wide signed fixed (Q2.4), value = d1.5625
(define (bv spec)

  (define re-bin "^([0-9]+)?(s|u)?b([01][01_]*)$")
  (define re-hex "^([0-9]+)?(s|u)?h([0-9a-fA-F][0-9a-fA-F_]*)$")
  (define re-dec "^([0-9]+)?(s|u)?d([-]?[0-9][0-9_]*)$")
  (define re-fix-dec "^([0-9]+)(s|u)([0-9]+)d([-]?[0-9][0-9_]*\\.[0-9][0-9_]*)$")
  (define re-fix-bin "^([0-9]+)(s|u)([0-9]+)b([01][01_]*\\.[01_]*)$")

  (define (cleanup str) (string-filter (lambda (c) (not (char=? c #\_))) str))
  (define (cleanup-fix str) (string-filter (lambda (c) (not (or (char=? c #\_) (char=? c #\.)))) str))

  ;; Perform two's complement conversion and overflow check.
  (define (import-binary-value value width signed)
    (if signed
        (let ((raw-value (if (>= value (expt 2 (- width 1)))
                             (if (<= value (expt 2 width))
                                 (- (- (expt 2 width) value))
                                 (bval-error (ss "Value range overflow")))
                             value))
              (range (bval-new 0 width signed 'bin)))
          (if (bval-value-fits? range raw-value)
              raw-value
              (bval-error (ss "Value range overflow"))))
        (let ((range (bval-new 0 width signed 'bin)))
          (if (bval-value-fits? range value)
              value
              (bval-error (ss "Value range overflow"))))))

  (define (min-width value signed)
    (define (bit-cnt value) (if (<= (abs value) 2) 1 (inexact->exact (ceiling (/ (log (abs value)) (log 2))))))
    (if signed
        (1+ (bit-cnt value))
        (bit-cnt value)))

  (cond

   ((re-matches re-bin spec)
    (let-values (((full width sign body) (apply values (re-matches re-bin spec))))
      (let ((body (cleanup body)))
        (let* ((width (if width (string->number width) (string-length body)))
               (signed (if sign (string=? sign "s") #f))
               (value (import-binary-value (string->number body 2) width signed)))
          (bval-new value width signed 'bin)))))

   ((re-matches re-hex spec)
    (let-values (((full width sign body) (apply values (re-matches re-hex spec))))
      (let ((body (cleanup body)))
        (let* ((width (if width (string->number width) (* 4 (string-length body))))
               (signed (if sign (string=? sign "s") #f))
               (value (import-binary-value (string->number body 16) width signed)))
          (bval-new value width signed 'hex)))))

   ((re-matches re-dec spec)
    (let-values (((full width sign body) (apply values (re-matches re-dec spec))))
      (let ((body (cleanup body)))
        (let* ((value (string->number body 10))
               (signed (if sign (string=? sign "s") #t))
               (width (if width (string->number width 10) (min-width value signed))))
          (bval-new value width signed 'dec)))))

   ((re-matches re-fix-dec spec)
    (let-values (((full width sign int-width body) (apply values (re-matches re-fix-dec spec))))
      (let ((body (cleanup body)))
        (let* ((value (string->number body 10))
               (signed (string=? sign "s"))
               (width (cons (string->number width) (string->number int-width))))
          (bval-new value width signed 'dec)))))

   ((re-matches re-fix-bin spec)
    (let-values (((full width sign int-width body) (apply values (re-matches re-fix-bin spec))))
      (let ((body (cleanup-fix body)))
        (let* ((width (if (= (string->number width) (string-length body))
                          (string->number width)
                          (bval-error (ss "Width and number literal mismatch"))))
               (int-width (string->number int-width))
               (signed (string=? sign "s"))
               (value (import-binary-value (string->number body 2) width signed))
               (scaled-value (/ (exact->inexact value) (expt 2 (- width int-width)))))
          (bval-new scaled-value (cons width int-width) signed 'bin)))))

   (else (bval-error (ss "Invalid bval format: \"" spec "\"")))))


(define (bv= bval)
  (bval-value bval))

(define (bv->fix bval int-width)
  (bval-new (/ (exact->inexact (bval-value bval)) (expt 2 (- (bval-width bval) int-width)))
            (cons (bval-width bval) int-width)
            (bval-signed bval)
            (bval-format bval)))

(define (bv-eq? a b)
  (equal? a b))

(define (bv-eq-type? a b)
  (eq? (bval-type a) (bval-type b)))

(define (bv-eq-topology? a b)
  (and (equal? (bval-size a)
               (bval-size b))
       (eq? (bval-signed a)
            (bval-signed b))))

(define (bv-sext a new-width)
  (if (>= new-width (bval-width a))
      (case (bval-type a)
        ((fix) (bval-new (bval-value a)
                         (cons new-width
                               (+ (bval-width-int a) (- new-width (bval-width a))))
                         (bval-signed a)
                         (bval-format a)))
        (else (bval-new (bval-value a)
                        new-width
                        (bval-signed a)
                        (bval-format a))))
      (bval-error (ss "Updated width must be greater or equal to original width"))))

(define (bv-sext+ a width-inc)
  (bv-sext a (+ (bval-width a) width-inc)))

;; Extend fix-type to new size. NA for int-type.
(define (bv-ext a size)
  (if (eq? (bval-type a) 'fix)
      (if (and (pair? size)
               (>= (bval-size-width size) (bval-width a))
               (>= (bval-size-int size) (bval-width-int a)))
          (bval-new (bval-value a)
                    size
                    (bval-signed a)
                    (bval-format a))
          (bval-error (ss "Invalid size specification for \"bv-ext\"")))
      (bval-error (ss "Operation (\"bv-ext\") can only performed for fixed value"))))

(define (bv* a b)
  (if (bv-eq-type? a b)
      (case (bval-type a)
        ((fix) (bval-new (* (bval-value a)
                            (bval-value b))
                         (cons (+ (bval-width a) (bval-width b))
                               (+ (bval-width-int a) (bval-width-int b)))
                         (bval-signed a)
                         (bval-format a)))
        (else (bval-new (* (bval-value a)
                           (bval-value b))
                        (+ (bval-width a) (bval-width b))
                        (bval-signed a)
                        (bval-format a))))
      (bval-error (ss "Operand type mismatch for \"bv*\""))))

(define (bv+ a b)
  (if (bv-eq-topology? a b)
      (case (bval-type a)
        ((fix) (bval-new (+ (bval-value a)
                            (bval-value b))
                         (cons (1+ (bval-width a))
                               (1+ (bval-width-int a)))
                         (bval-signed a)
                         (bval-format a)))
        (else (bval-new (+ (bval-value a)
                           (bval-value b))
                        (1+ (bval-width a))
                        (bval-signed a)
                        (bval-format a))))
      (bval-error (ss "Operand type mismatch for \"bv+\""))))

(define (bv- a b)
  (if (bv-eq-topology? a b)
      (case (bval-type a)
        ((fix) (bval-new (- (bval-value a)
                            (bval-value b))
                         (cons (1+ (bval-width a))
                               (1+ (bval-width-int a)))
                         (bval-signed a)
                         (bval-format a)))
        (else (bval-new (- (bval-value a)
                           (bval-value b))
                        (1+ (bval-width a))
                        (bval-signed a)
                        (bval-format a))))
      (bval-error (ss "Operand type mismatch for \"bv-\""))))


(define bv->str bval->string)

;;(define a (bval-new 0.00001 (cons 20 -3) #t 'bin))
;;(pr (bval->string a))
