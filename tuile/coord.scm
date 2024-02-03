(define-module (tuile coord)
  #:use-module ((tuile utils) #:select (number-span list-range div))
  #:use-module ((srfi srfi-1) #:select (fold first second))
  #:export
  (
   p.
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
   p*
   p/
   p+dir
   p-dir
   p-p-dir
   p-p-len
   p-p-distance
   p-p-dir-len
   p-p-manhattan-distance
   p-p-hdir
   p-p-orientation
   p-p-angle
   xy->points
   points->xy
   p-p->trace
   p-p-point
   p-inside?
   p-contained?

   pp.
   xy->pp
   pp0
   pp1
   pp->xy
   pp00
   pp10
   pp01
   pp11
   pp-len
   pp-dir
   pp->trace
   pp-point

   r-corners
   r-width
   r-height
   r-corner

   dir-orientation
   dir-opposite
   diridx->dir

   path->segments
   path->trace
   path-len
   path-point
   path-points
   path-start-dir

   ))


(define pi (* 4 (atan 1)))

;; Create point (from x, y).
(define p. cons)
(define xy cons)
(define (yx y x) (cons x y))
(define px car)
(define py cdr)
(define (p+ a b) (p. (+ (px a) (px b)) (+ (py a) (py b))))
(define (p- a b) (p. (- (px a) (px b)) (- (py a) (py b))))
(define (px+ a b) (p. (+ (px a) (px b)) (py a)))
(define (py+ a b) (p. (px a) (+ (py a) (py b))))
(define (px- a b) (p. (- (px a) (px b)) (py a)))
(define (py- a b) (p. (px a) (- (py a) (py b))))
(define (p= a b) (and (= (px a) (px b)) (= (py a) (py b))))
(define (p+x p x) (p. (+ (px p) x) (py p)))
(define (p+y p y) (p. (px p) (+ (py p) y)))
(define (p-x p x) (p. (- (px p) x) (py p)))
(define (p-y p y) (p. (px p) (- (py p) y)))
(define (p* p s) (p. (* (px p) s) (* (py p) s)))
(define (p/ p s) (p. (div (px p) s) (div (py p) s)))

(define (p+dir p dir . step)
  (let ((count (if (pair? step) (car step) 1)))
    (case dir
      ((left) (p-x p count))
      ((right) (p+x p count))
      ((down) (p+y p count))
      ((up) (p-y p count)))))

(define (p-dir p dir . step)
  (let ((count (if (pair? step) (car step) 1)))
    (case dir
      ((left) (p+x p count))
      ((rigth) (p-x p count))
      ((down) (p-y p count))
      ((up) (p+y p count)))))

(define (p-p-dir a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a))))
      (cond
       ((and (> xd 0) (= yd 0)) 'right)
       ((and (> yd 0) (= xd 0)) 'down)
       ((and (< xd 0) (= yd 0)) 'left)
       ((and (< yd 0) (= xd 0)) 'up)
       (else *unspecified*))))

;; Length (distance) for horizontally or vertically aligned points.
(define (p-p-len a b)
  (1+ (abs (if (= (px a) (px b))
               (- (py b) (py a))
               (- (px b) (px a))))))

(define p-p-distance p-p-len)

(define (p-p-dir-len a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a))))
      (cond
       ((and (> xd 0) (= yd 0)) (cons 'right (+ xd 1)))
       ((and (> yd 0) (= xd 0)) (cons 'down  (+ yd 1)))
       ((and (< xd 0) (= yd 0)) (cons 'left  (+ (- xd) 1)))
       ((and (< yd 0) (= xd 0)) (cons 'up    (+ (- yd) 1)))
       (else (cons *unspecified* *unspecified*)))))

;; Distance for horizontally or vertically aligned points.
(define (p-p-manhattan-distance a b)
  (let ((ax (px a))
        (ay (py a))
        (bx (px b))
        (by (py b)))
    (+ (abs (- ax bx)) (abs (- ay by)))))

(define (p-p-hdir a b)
  (let ((xd (- (px b) (px a)))
        (yd (- (py b) (py a))))
    (cond
     ((and (> xd 0) (= yd 0)) 'right)        ;; Straight right ->.
     ((and (< xd 0) (= yd 0)) 'left)         ;; Straight left  -<.
     ((and (= xd 0) (> yd 0)) 'right)        ;; Straight up   |^.
     ((and (= xd 0) (< yd 0)) 'left)         ;; Straight down |v.
     ((and (> xd 0) (> yd 0)) 'right)        ;; Right up /^.
     ((and (< xd 0) (> yd 0)) 'left)         ;; Left up  ^\.
     ((and (< xd 0) (< yd 0)) 'left)         ;; Left down /v.
     ((and (> xd 0) (< yd 0)) 'right)        ;; Right down \v.
     (else *unspecified*) ;; +, a and b are same position.
     )))
(define (p-p-orientation a b)
  (let ((dir (p-p-dir a b)))
    (case dir
      ((left right) 'horizontal)
      ((down up) 'vertical)
      (else *unspecified*))))
(define (p-p-angle a b)
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
(define (xy->points . xy-pairs)
  (if (= (remainder (length xy-pairs) 2) 0)
      (let loop ((rest xy-pairs)
                 (points '()))
        (if (pair? rest)
            (loop (cddr rest)
                  (cons (p. (car rest) (cadr rest))
                        points))
            (reverse points)))
      *unspecified*))

;; Convert points to pairs of xy.
(define (points->xy . points)
  (let loop ((rest points)
             (ret '()))
    (if (pair? rest)
        (loop (cdr rest)
              (append (list (cdar rest) (caar rest)) ret))
        (reverse ret))))

(define (p-p->trace p0 p1)
  (let* ((dist (p-p-distance p0 p1)))
    (case (p-p-dir p0 p1)
      ((left right) (map p. (number-span (px p0) (px p1)) (make-list dist (py p0))))
      ((up down) (map p. (make-list dist (px p0)) (number-span (py p0) (py p1)))))))

;; Return nth point between endpoints.
(define (p-p-point p0 p1 nth)
  (if (= nth 0)
      p0
      (let ((dir (p-p-dir p0 p1)))
        (p+dir p0 dir nth))))

;; Check that p is inside boundaries (not on boundaries), defined by
;; p0 and p1.
(define (p-inside? p p0 p1)
  (let ((x (px p))
        (y (py p))
        (x0 (px p0))
        (y0 (py p0))
        (x1 (px p1))
        (y1 (py p1)))
    (and (and (> x x0) (< x x1))
         (and (> y y0) (< y y1)))))

;; Check that p is inside boundaries or on boundaries, defined by p0
;; and p1.
(define (p-contained? p p0 p1)
  (let ((x (px p))
        (y (py p))
        (x0 (px p0))
        (y0 (py p0))
        (x1 (px p1))
        (y1 (py p1)))
    (and (and (>= x x0) (<= x x1))
         (and (>= y y0) (<= y y1)))))

;; Create point-pair.
(define pp. cons)
(define (xy->pp x0 y0 x1 y1) (pp. (p. x0 y0) (p. x1 y1)))
(define pp0 car)
(define pp1 cdr)
(define (pp->xy pp) (list (px (pp0 pp))
                          (py (pp0 pp))
                          (px (pp1 pp))
                          (py (pp1 pp))))

(define pp00 pp0)                       ; top-left
(define (pp10 pp) (p. (px (pp1 pp)) (py (pp0 pp)))) ; top-right
(define (pp01 pp) (p. (px (pp0 pp)) (py (pp1 pp)))) ; bottom-left
(define pp11 pp1)                       ; bottom-right

(define (pp-dir pp)
  (p-p-dir (pp0 pp) (pp1 pp)))

(define (pp-len pp)
  (p-p-len (pp0 pp) (pp1 pp)))

;; Return all points within point-pair (pp).
(define (pp->trace pp)
  (p-p->trace (pp0 pp) (pp1 pp)))

;; Return all points within point-pair (pp).
(define (pp-point pp nth)
  (p-p-point (pp0 pp) (pp1 pp) nth))

;; Convert two points to 4 corner points.
(define (r-corners a b)
  (let ((x0 (px a))
        (y0 (py a))
        (x1 (px b))
        (y1 (py b)))
    (list (p. x0 y0)
          (p. x1 y0)
          (p. x1 y1)
          (p. x0 y1))))

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


(define (dir-orientation dir)
  (case dir
    ((left right) 'horizontal)
    ((down up)    'vertical)))


(define (dir-opposite dir)
  (case dir
    ((left)  'right)
    ((right) 'left)
    ((up)    'down)
    ((down)  'up)))


(define (diridx->dir idx)
  (list-ref '(right down left up) idx))


;; Return segments of path.
;;
;;       +---------+        +---------+   +
;;                 |   =>                 |
;;                 |                      |
;;                 +                      +
;;
(define (path->segments path)
  (let loop ((rest (cdr path))
             (prev (car path))
             (ret '()))
    (if (null? rest)
        (reverse ret)
        (loop (cdr rest)
              (car rest)
              (cons (pp. prev (car rest))
                    ret)))))


(define (path->trace path)
  (let loop ((rest (path->segments path))
             (ret (list (car path))))
    (if (null? rest)
        (reverse ret)
        (loop (cdr rest)
              (append (reverse (cdr (pp->trace (car rest))))
                      ret)))))


(define (path-len path)
  (fold (lambda (seg s) (+ s (1- (pp-len seg))))
        1
        (path->segments path)))


(define (path-point path nth)
  (list-ref (path->trace path) nth))


(define (path-points path nth count)
  (list-range (path->trace path) nth count))


(define (path-start-dir path)
  (pp-dir (pp. (first path)
               (second path))))
