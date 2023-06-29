;;; module:
;;
;; Range is a library for range arithmetics.
;;
;; Range is a pair with start value and the end value. Empty ranges
;; are not possible. End value must be greater or equal to the start
;; value.
;;
(define-module (tuile range)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:export
  (
   range-overlap?
   range-contained?
   range-union
   range-intersect
   range-difference
   range-complement
   range-combine
   ))


;; ------------------------------------------------------------
;; Internals:

;; Are ranges (as parts) overlapping?
(define (parts-overlap? a0 a1 b0 b1)
  ;; It is easiest to test which ranges do NOT overlap. Hence we
  ;; test for non-overlap and then say that or others overlap.
  (cond
   ;;     a ---
   ;;     b     ----
   ((< a1 b0) #f)
   ;;     a      ---
   ;;     b ----
   ((> a0 b1) #f)

   (else #t)))

;; Macro: Decompose a and b and evaluate op for the decomposed ranges.
;;
;;     (with-decomposed-ranges
;;      a
;;      b
;;      (parts-overlap? a0 a1 b0 b1))
;;
(define-syntax with-decomposed-ranges
  (lambda (x)
    (syntax-case x (a b)
      ((_ op)
       (with-syntax ((a (datum->syntax x 'a))
                     (b (datum->syntax x 'b))
                     (a0 (datum->syntax x 'a0))
                     (a1 (datum->syntax x 'a1))
                     (b0 (datum->syntax x 'b0))
                     (b1 (datum->syntax x 'b1)))
         #`(let ((a0 (car a))
                 (a1 (cdr a))
                 (b0 (car b))
                 (b1 (cdr b)))
             op))))))

;; Internals:
;; ------------------------------------------------------------


;; Does A and B overlap?
(define (range-overlap? a b)
  (with-decomposed-ranges
   (parts-overlap? a0 a1 b0 b1)))


;; Is A contained in B?
(define (range-contained? a b)
  (with-decomposed-ranges
   (and (<= b0 a0)
        (>= b1 a1))))


;; Create union (or) of ranges.
;;
;;     a :  ---       :
;;     b :   ----     :
;;       =>
;;     r :  -----     :
;;
(define (range-union a b)
  (with-decomposed-ranges
   (if (parts-overlap? a0 a1 b0 b1)
       (cons (min a0 b0)
             (max a1 b1))
       #f)))


;; Create intersection (and) of ranges.
;;
;;     a :  ---       :
;;     b :   ----     :
;;       =>
;;     r :   --       :
;;
(define (range-intersect a b)
  (with-decomposed-ranges
   (if (range-overlap? a b)
       (cons (max a0 b0)
             (min a1 b1))
       #f)))


;; Create difference of ranges.
;;
;; Difference: values in A, but not in B.
;;
;;
;;     a :  ---       :
;;     b :   ----     :
;;       =>
;;     r :  -         :
;;
(define (range-difference a b)
  (with-decomposed-ranges
   (if (range-overlap? a b)
       (if (< a0 b0)
           (cons a0 b0)
           (cons b1 a1))
       #f)))


;; Create complement of A in B.
;;
;; Complement exists only if B is superset of A and A is not same as
;; B. Complement result is either one range or a pair of ranges.
;;
;;     a :  ---       :
;;     b : ------     :
;;       =>
;;     r : -   --     :
;;
(define (range-complement a b)
  (with-decomposed-ranges
   (if (range-overlap? a b)
       (if (< a0 b0)
           (cons a0 b0)
           (cons b1 a1))
       #f)))


;; Combine (with union) a list of ranges.
(define (range-combine lst)

  (define (separate new ref)
    (let lp ((ref ref)
             (yes '())
             (no '()))
      (if (pair? ref)
          (if (range-overlap? new (car ref))
              (lp (cdr ref)
                  (cons (car ref) yes)
                  no)
              (lp (cdr ref)
                  yes
                  (cons (car ref) no)))
          (cons yes no))))

  (if (= (length lst) 1)
      lst
      (let lp ((rest (cdr lst))
               (ret (list (car lst))))
        (if (pair? rest)
            (lp (cdr rest)
                (let ((separation (separate (car rest) ret)))
                  (if (pair? (car separation))
                      (cons (fold range-union (car rest) (car separation)) (cdr separation))
                      (cons (car rest) ret))))
            (begin
              ;; (ppre ret)
              (stable-sort ret (lambda (a b) (< (car a) (car b)))))))))


;; (use-modules (tuile pr))
;; (ppre (range-combine (list (cons 1 2) (cons 4 6) (cons 5 9) (cons 10 11) (cons 9 12))))
;; ;; (ppre (range-combine (list (cons 1 2) (cons 4 6) (cons 5 9) (cons 10 11) (cons 9 12))))

;; (use-modules (tuile pr))
;; (ppre (range-overlap? (cons 1 3) (cons 4 6)))
;; (ppre (range-overlap? (cons 1 4) (cons 4 6)))
