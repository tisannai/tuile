(define-module (tuile bval)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (first second fold))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:export
  (
   ))


;;(use-modules ((rnrs records syntactic) #:select (define-record-type)))
;;(use-modules (ice-9 match))
;;(use-modules ((srfi srfi-1) #:select (first second fold)))
;;(use-modules ((srfi srfi-11) #:select (let-values)))
;;(use-modules (tuile utils))
;;(use-modules (tuile pr))

;; ------------------------------------------------------------
;; Creation and modification:

(define-record-type bval
  (fields value                        ; Integer or fixed.
          size                         ; Number of bits, pair for fix.
          signed                       ; Signed/unsigned.
          format                       ; fix, bin, hex, dec.
          ))

(define (bval-error msg)
  (display (ss "bval error: " msg "\n") (current-error-port))
  (raise-exception &error))

(define (bval-type bval)
  (if (pair? (bval-size bval)) 'fix 'int))

(define (bval-width bval)
  (case (bval-type bval)
    ((fix) (car (bval-size bval)))
    (else (bval-size bval))))

(define (bval-fixed? bval)
  (inexact? (bval-value bval)))

(define (bval-int-value bval)
  (case (bval-type bval)
    ((fix) (let ((frac (- (car (bval-size bval))
                          (cdr (bval-size bval)))))
             (inexact->exact (* (bval-value bval) (expt 2 frac)))))
    (else (bval-value bval))))

(define (bval-range bval)
  (case (bval-type bval)
    ((fix) (let* ((size (bval-size bval))
                  (width (car size))
                  (frac (- width (cdr size))))
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
  (let* ((size-is-fix (pair? size))
         (default-format (if size-is-fix 'fix 'dec))
         (import-fixed-value (lambda (value size)
                               (let ((frac (expt 2 (- (car size) (cdr size)))))
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
       ((eq? format 'fix)
        (when (not (pair? size))
          (bval-error (ss "Size of fix must be a pair")))
        (when (> (cdr size) (car size))
          (bval-error (ss "Integer portion of fix must be smaller than width")))
        (when (and signed
                   (< (car size) 2))
          (bval-error (ss "Width of signed fix must be at least two"))))
       ((not (exact? value))
        (bval-error (ss "Value on non-fix number must be an integer")))
       ((<= size 0)
        (bval-error "Size must be greater than 0.")))
      (case format
        ((fix bin hex dec)
         (let ((temp (make-bval 0 size signed format)))
           (if (bval-value-fits? temp value)
               (if (eq? format 'fix)
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
      ((fix) (ss (bval-width bval)
                 (if (bval-signed bval) "s" "u")
                 (cdr (bval-size bval))
                 "q"
                 (number->string (bval-value bval))))
      ((bin) (ss (bval-width bval)
                 (if (bval-signed bval) "sb" "ub")
                 (let ((body (pad (bin-format bval) (bval-width bval))))
                   (if (bval-fixed? bval)
                       (let ((int (cdr (bval-size bval))))
                         (ss (substring body 0 int) "." (substring body int)))
                       body))))
      ((hex) (if (bval-fixed? bval)
                 (bval-error "Can't format fixed into hex string.")
                 (ss (bval-width bval)
                     (if (bval-signed bval) "sh" "uh")
                     (pad (number->string (bval-value bval) 16)
                          (hex-nibbles (bval-width bval))))))
      ((dec) (number->string (bval-value bval))))))

;; 12sb1010 or b1010 (for 4-bit wide)
(define (bv spec)

  (define re-bin "^([0-9]+)?(s|u)?b([01][01_]*)$")
  (define re-hex "^([0-9]+)?(s|u)?h([0-9a-fA-F][0-9a-fA-F_]*)$")
  (define re-dec "^([0-9]+)?(s|u)?d([-]?[0-9][0-9_]*)$")

  (define (cleanup str) (string-filter (lambda (c) (not (char=? c #\_))) str))

  (define (import-value value width signed)
    (if (and signed
             (>= value (expt 2 (- width 1))))
        ;; Twos complement.
        (- (- (expt 2 width) value))
        ;; Direct.
        value))

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
               (value (import-value (string->number body 2) width signed)))
          (bval-new value width signed 'bin)))))

   ((re-matches re-hex spec)
    (let-values (((full width sign body) (apply values (re-matches re-hex spec))))
      (let ((body (cleanup body)))
        (let* ((width (if width (string->number width) (string-length body)))
               (signed (if sign (string=? sign "s") #f))
               (value (import-value (string->number body 16) width signed)))
          (bval-new value width signed 'hex)))))

   ((re-matches re-dec spec)
    (let-values (((full width sign body) (apply values (re-matches re-dec spec))))
      (let ((body (cleanup body)))
        (let* ((value (string->number body 10))
               (signed (if sign (string=? sign "s") (< value 0)))
               (width (if width (string->number width) (min-width value signed))))
          (bval-new value width signed 'dec)))))

   ))
