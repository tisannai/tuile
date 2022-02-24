(define-module (tuile coord)
  #:export
  (
   pos
   px
   py
   p+
   p-
   px+
   py+
   px-
   py-
   ))

(define pos cons)
(define px car)
(define py cdr)
(define (p+ p1 p2) (pos (+ (px p1) (px p2)) (+ (py p1) (py p2))))
(define (p- p1 p2) (pos (- (px p1) (px p2)) (- (py p1) (py p2))))
(define (px+ p1 p2) (pos (+ (px p1) (px p2)) (py p1)))
(define (py+ p1 p2) (pos (px p1) (+ (py p1) (py p2))))
(define (px- p1 p2) (pos (- (px p1) (px p2)) (py p1)))
(define (py- p1 p2) (pos (px p1) (- (py p1) (py p2))))
