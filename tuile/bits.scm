(define-module (tuile bits)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:export
  (
   bits-value
   bits-width
   bits-new
   bits-new-tc

   bits-error
   bits-required-width?
   bits-value-wrap
   bits-dup
   bits-value-tc
   bits-resize
   bits^
   bits+
   bits-
   bits*
   bits-sel
   bits->bin-string

   bn
   bt
   bd
   bv
   bs
   bw
   ))


;; ------------------------------------------------------------
;; Creation and modification:

(define-record-type bits
  (fields (mutable value)
          (mutable width)))

(define (bits-error msg)
  (display (ss "bits error: " msg "\n") (current-error-port))
  #f)

(define (bits-required-width? value)
  (case value
    ((0 1) 1)
    (else
     (inexact->exact (ceiling (/ (log (1+ value)) (log 2)))))))

(define (bits-value-wrap value width)
  (remainder value (expt 2 width)))

(define* (bits-new value #:optional (width #f))
  (if width
      (if (> (bits-required-width? value) width)
          (make-bits (bits-value-wrap value width) width)
          (make-bits value width))
      (make-bits value (bits-required-width? value))))

(define (bits-new-tc value width)
  (if (< value 0)
      (if (< value (- (expt 2 (1- width))))
          (bits-error (ss "TC-underflow of value for " width " bits with: " value))
          (bits-new (+ (expt 2 width) value) width))
      (if (> value (1- (expt 2 (1- width))))
          (bits-error (ss "TC-overflow of value for " width " bits with: " value))
          (bits-new value width))))

(define (bits-dup b)
  (bits-new (bits-value b)
            (bits-width b)))

(define (bits-value-tc b)
  (- (- (expt 2 (bits-width b))
        (bits-value b))))

(define (bits-resize bits count)
  (let ((width (+ (bits-width bits) count)))
    (when (> width 1)
      (bits-width-set! bits width)
      (bits-value-set! bits (bits-value-wrap (bits-value bits) width)))))

(define (bits^ b)
  (bits-new-tc (- (bits-value b)) (bits-width b)))

;; ------------------------------------------------------------
;; Arithmetics:

(define (bits+ a . rest)
  (bits-new (apply + (map bits-value (cons a rest)))
            (apply max (map bits-width (cons a rest)))))

(define (bits- a . rest)
  (if (let loop-check ((rest rest))
        (if (pair? rest)
            (if (not (= (bits-width a)
                        (bits-width (car rest))))
                (bits-error (ss "Non-uniform widths for operands with bits-: " (map datum->string (cons a rest))))
                (loop-check (cdr rest)))
            #t))
      (apply bits+ (cons a (map bits^ rest)))
      #f))


(define (bits* a . rest)
  (if (= (length rest) 1)
      ;; Two operand multiplication.
      (bits-new (* (bits-value a) (bits-value (car rest))) (+ (bits-width a) (bits-width (car rest))))
      (let ((res (apply * (map bits-value (cons a rest)))))
        (bits-new res))))

(define (bits-sel a i)
  (if (logbit? i (bits-value a)) 1 0))

(define (bits->bin-string bits)
  (:rj (bits-width bits) "0" (number->string (bits-value bits) 2)))



;; ------------------------------------------------------------
;; Aliases:


(define bn bits-new)
(define bt bits-new-tc)
(define bd bits-dup)
(define bv bits-value)
(define bs bits-value-tc)
(define bw bits-width)



#|
(use-modules (tuile bits))
(bits->bin-string (bits-new 3 4))
|#


#|


|#
