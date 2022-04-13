(define-module (tuile coord)
  #:use-module ((tuile utils) #:select (span))
  #:export
  (
   ->p
   xy
   yx
   px
   py
   p+
   p-
   px+
   py+
   px-
   py-
   p=
   p+x
   p+y
   p-x
   p-y
   p+dir
   p-dir
   p->dir
   p->h-dir
   p->orientation
   p->angle
   xy->points
   points->xy
   p->points
   p-inside?

   ->pp
   xy->pp
   pp0
   pp1
   pp->xy
   pp00
   pp10
   pp01
   pp11
   pp->points

   ->mp

   r->corners
   r-width
   r-height
   r-corner

   ->dir

   p-dist
   p-manhattan-distance
   ))


(define pi (* 4 (atan 1)))

;; Create point (from x, y).
(define ->p cons)
(define xy cons)
(define (yx y x) (cons x y))
(define px car)
(define py cdr)
(define (p+ a b) (->p (+ (px a) (px b)) (+ (py a) (py b))))
(define (p- a b) (->p (- (px a) (px b)) (- (py a) (py b))))
(define (px+ a b) (->p (+ (px a) (px b)) (py a)))
(define (py+ a b) (->p (px a) (+ (py a) (py b))))
(define (px- a b) (->p (- (px a) (px b)) (py a)))
(define (py- a b) (->p (px a) (- (py a) (py b))))
(define (p= a b) (and (= (px a) (px b)) (= (py a) (py b))))
(define (p+x p x) (->p (+ (px p) x) (py p)))
(define (p+y p y) (->p (px p) (+ (py p) y)))
(define (p-x p x) (->p (- (px p) x) (py p)))
(define (p-y p y) (->p (px p) (- (py p) y)))
(define (p+dir p dir)
  (case dir
    ((left) (p-x p 1))
    ((right) (p+x p 1))
    ((down) (p+y p 1))
    ((up) (p-y p 1))))
(define (p-dir p dir)
  (case dir
    ((left) (p+x p 1))
    ((rigth) (p-x p 1))
    ((down) (p-y p 1))
    ((up) (p+y p 1))))
(define (p->dir a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a))))
      (cond
       ((and (> xd 0) (= yd 0)) 'right)
       ((and (< yd 0) (= xd 0)) 'down)
       ((and (< xd 0) (= yd 0)) 'left)
       ((and (> yd 0) (= xd 0)) 'up)
       (else *unspecified*))))
(define (p->h-dir a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a))))
    (cond
     ((and (> xd 0) (= yd 0)) 'right)        ;; Straight right ->.
     ((and (< xd 0) (= yd 0)) 'left)         ;; Straight left  <-.
     ((and (= xd 0) (> yd 0)) 'right)        ;; Straight up   |^.
     ((and (= xd 0) (< yd 0)) 'left)         ;; Straight down |v.
     ((and (> xd 0) (> yd 0)) 'right)        ;; Right up /^.
     ((and (< xd 0) (> yd 0)) 'left)         ;; Left up  ^\.
     ((and (< xd 0) (< yd 0)) 'left)         ;; Left down /v.
     ((and (> xd 0) (< yd 0)) 'right)        ;; Right down \v.
     (else *unspecified*) ;; +, a and b are same position.
     )))
(define (p->orientation a b)
  (let ((dir (p->dir a b)))
    (case dir
      ((left right) 'horizontal)
      ((down up) 'vertical)
      (else *unspecified*))))
(define (p->angle a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a)))
        (p2 (* pi 2)))
    (cond
     ((and (> xd 0) (= yd 0)) 0.0)           ;; Straight right ->.
     ((and (< xd 0) (= yd 0)) pi)            ;; Straight left  <-.
     ((and (= xd 0) (> yd 0)) (* pi 0.5))    ;; Straight up   |^.
     ((and (= xd 0) (< yd 0)) (* pi 1.5))    ;; Straight down |v.
     ((and (> xd 0) (> yd 0)) (+  0 (atan (/ (* +1.0 yd) (* +1.0 xd))))) ;; Right up /^.
     ((and (< xd 0) (> yd 0)) (- pi (atan (/ (* +1.0 yd) (* -1.0 xd))))) ;; Left up  ^\.
     ((and (< xd 0) (< yd 0)) (+ pi (atan (/ (* -1.0 yd) (* -1.0 xd))))) ;; Left down /v.
     ((and (> xd 0) (< yd 0)) (- p2 (atan (/ (* -1.0 yd) (* +1.0 xd))))) ;; Right down \v.
     (else *unspecified*) ;; +, a and b are same position.
     )))

;; Convert pairs of xy to points.
(define (xy->points . xy-pair-list)
  (if (= (remainder (length xy-pair-list) 2) 0)
      (let loop ((rest xy-pair-list)
                 (points '()))
        (if (pair? rest)
            (loop (cddr rest)
                  (cons (->p (car rest) (cadr rest))
                        points))
            (reverse points)))
      *unspecified*))
(define (points->xy . points)
  (let loop ((rest points)
             (ret '()))
    (if (pair? rest)
        (loop (cdr rest)
              (append (list (cdar rest) (caar rest)) ret))
        (reverse ret))))

(define (p->points p0 p1)
  (let* ((dist (p-dist p0 p1)))
    (case (p->dir p0 p1)
      ((left right) (map ->p (span (px p0) (px p1)) (make-list dist (py p0))))
      ((up down) (map ->p (make-list dist (px p0)) (span (py p0) (py p1)))))))

;; Check that p is inside boundaries defined by p0 and p1.
(define (p-inside? p p0 p1)
  (let ((x (px p))
        (y (py p))
        (x0 (px p0))
        (y0 (py p0))
        (x1 (px p1))
        (y1 (py p1)))
    (and (and (> x x0) (< x x1))
         (and (> y y0) (< y y1)))))

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

;; Return all points within point-pair (pp).
(define (pp->points pp)
  (p->points (pp0 pp) (pp1 pp)))


;; Multipoint.
(define (->mp lst)
  (cond
   ;; Point based multipoint.
   ((pair? (car lst))
    lst)
   ;; Coord based multipoint. Must have even number of coords.
   ((and (even? (length lst))
         (integer? (car lst))
         (integer? (cadr lst)))
    (let loop ((lst lst)
               (pairs '()))
      (if (pair? lst)
          (loop (cddr lst)
                (cons (->p (car lst) (cadr lst))
                      pairs))
          (reverse pairs))))
   (else
    ;; not supported (yet).
    '())))

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

;; Distance for horizontally or vertically aligned points.
(define (p-manhattan-distance a b)
  (let ((ax (px a))
        (ay (py a))
        (bx (px b))
        (by (py b)))
    (+ (abs (- ax bx)) (abs (- ay by)))))
