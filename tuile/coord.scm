(define-module (tuile coord)
  #:export
  (
   ->p
   px
   py
   p+
   p-
   px+
   py+
   px-
   py-

   ->pp
   xy->pp
   pp0
   pp1
   pp->xy
   pp00
   pp10
   pp01
   pp11

   r->corners
   r-width
   r-height
   r-corner

   ->dir

   p-dist
   ))

;; Create point (from x, y).
(define ->p cons)
(define px car)
(define py cdr)
(define (p+ a b) (->p (+ (px a) (px b)) (+ (py a) (py b))))
(define (p- a b) (->p (- (px a) (px b)) (- (py a) (py b))))
(define (px+ a b) (->p (+ (px a) (px b)) (py a)))
(define (py+ a b) (->p (px a) (+ (py a) (py b))))
(define (px- a b) (->p (- (px a) (px b)) (py a)))
(define (py- a b) (->p (px a) (- (py a) (py b))))


;; Create point-pair.
(define ->pp cons)
(define (xy->pp x0 y0 x1 y1) (->pp (->p x0 y0) (->p x1 y1)))
(define pp0 car)
(define pp1 cdr)
(define (pp->xy pp) (list (px (pp0 pp))
                          (py (pp0 pp))
                          (px (pp1 pp))
                          (py (pp1 pp))))

(define pp00 pp0)                       ; top-left
(define (pp10 pp) (->p (px (pp1 pp)) (py (pp0 pp)))) ; top-right
(define (pp01 pp) (->p (px (pp0 pp)) (py (pp1 pp)))) ; bottom-left
(define pp11 pp1)                       ; bottom-right

;; Convert two points to 4 corner points.
(define (r->corners a b)
  (let ((x0 (px a))
        (y0 (py a))
        (x1 (px b))
        (y1 (py b)))
    (list (->p x0 y0)
          (->p x1 y0)
          (->p x1 y1)
          (->p x0 y1))))

;; Return rectangle width.
(define (r-width a b)
  (1+ (- (px b) (px a))))

;; Return rectangle height.
(define (r-height a b)
  (1+ (- (py b) (py a))))

;; Rectangle corner (0..3).
(define (r-corner pp n)
  (case n
    ((0) (pp00 pp))
    ((1) (pp10 pp))
    ((2) (pp11 pp))
    ((3) (pp01 pp))))

(define (->dir val)
  (if (symbol? val)
      val
      (list-ref '(right down left up) val)))

;; Distance for horizontally or vertically aligned points.
(define (p-dist a b)
  (1+ (abs (if (= (px a) (px b))
               (- (py b) (py a))
               (- (px b) (px a))))))
