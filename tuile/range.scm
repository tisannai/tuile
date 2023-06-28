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
   range-join
   range-combine
   ))


(define (range-overlap? a b)

  (let ((a0 (car a))
        (a1 (cdr a))
        (b0 (car b))
        (b1 (cdr b)))

    ;; It is easiest to test which ranges do NOT overlap. Hence we
    ;; test for non-overlap and then say that or others overlap.
    (cond

     ;;     a ---
     ;;     b     ----
     ((< a1 b0) #f)
     ;;     a      ---
     ;;     b ----
     ((> a0 b1) #f)

     (else #t))))


(define (range-join a b)
  (let ((a0 (car a))
        (a1 (cdr a))
        (b0 (car b))
        (b1 (cdr b)))
    (if (range-overlap? a b)
        (cons (min a0 b0)
              (max a1 b1))
        #f)))


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
                      (cons (fold range-join (car rest) (car separation)) (cdr separation))
                      (cons (car rest) ret))))
            (begin
              ;; (ppre ret)
              (stable-sort ret (lambda (a b) (< (car a) (car b)))))))))


;; (use-modules (tuile pr))
;; (ppre (range-combine (list (cons 1 2) (cons 4 6) (cons 5 9) (cons 10 11) (cons 9 12))))
;; ;; (ppre (range-combine (list (cons 1 2) (cons 4 6) (cons 5 9) (cons 10 11) (cons 9 12))))



