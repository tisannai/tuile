(define-module (tuile forms)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile coord)
  #:use-module ((tuile canvas) #:prefix cv.)
  #:use-module ((srfi srfi-1) #:select (last first second))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:export
  (
   line
   rectangle
   text
   poly
   draw
   show
   ))


;; Draw line to canvas.
;;
;;     (line cv ">->" (p. 1 1) (p. 28 1))
;;
(define (line cv spec pa pb)
  (let ((dir-len (p-p-dir-len pa pb)))
    (cv.put-str-in-dir cv
                       (stretch-string spec (cdr dir-len))
                       pa
                       (car dir-len))))



;;
;;     |--------|    |--..        |-.          |-.          |.
;;                        ''--|      '..          '.          '.
;;                                      ''-|        '.          '.
;;                                                    '-|         '.
;;                                                                  '|
(define (freeline cv spec pa pb)
  #t)


;; Draw rectangle to canvas.
;;
;;     (rectangle cv (p. 5 3) (p. 16 7))
;;
(define (rectangle cv pa pb)
  (let ((sides (list "+-" "+|" "+-" "+|")))
    (cond
     ((equal? pa pb) (cv.put-ch cv #\+ pa))
     ((equal? (px pa) (px pb)) (cv.put-str-in-dir cv
                                                  (ss (stretch-string (second sides) (1- (r-height pa pb))) "+")
                                                  pa
                                                  'down))
     ((equal? (py pa) (py pb)) (cv.put-str-in-dir cv
                                                  (ss (stretch-string (first sides) (1- (r-width pa pb))) "+")
                                                  pa
                                                  'right))
     (else (let ((w (1- (r-width pa pb)))
                 (h (1- (r-height pa pb))))
             (let loop ((len (list w h w h))
                        (pos (r-corners pa pb))
                        (str sides)
                        (diridx 0))
               (when (< diridx 4)
                 (cv.put-str-in-dir cv
                                    (stretch-string (car str) (car len))
                                    (car pos)
                                    (diridx->dir diridx))
                 (loop (cdr len)
                       (cdr pos)
                       (cdr str)
                       (1+ diridx))))))))

  )


;; Draw text to position in direction.
;;
;;     (text cv "Hello" (p. 8 5) 'down)
;;
(define (text cv str pos dir)
  (cv.put-str-in-dir cv str pos dir))


;; Draw polyline through points.
;;
;; NOTE: @ is a special character that corresponds to first segment
;;       direction character.
;;
;;     (poly cv ":-|+>" (xy->points 20 10 24 10 24 6 30 6))
;;
;;     (poly cv "@-|+>" (xy->points 20 10 24 10 24 6 30 6))
;;
(define (poly cv spec points)

  (define (draw-segment cv pa pb ch-beg ch-hor ch-ver ch-end)
    (let ((dir-len (p-p-dir-len pa pb)))
      (cv.put-str-in-dir cv
                         (stretch-string (string ch-beg
                                                 (if (eq? (dir-orientation (car dir-len))
                                                          'horizontal)
                                                     ch-hor
                                                     ch-ver)
                                                 ch-end)
                                         (cdr dir-len))
                         pa
                         (car dir-len))))

  (let-values (((ch-beg-sym ch-hor ch-ver ch-cnr ch-end) (apply values (string->list spec))))
    (let* ((start-dir (path-start-dir points))
           (ch-beg (if (char=? ch-beg-sym #\@)
                       (if (or (eq? start-dir 'left)
                               (eq? start-dir 'right))
                           ch-hor
                           ch-ver)
                       ch-beg-sym)))
      (let loop ((state 'first)
                 (prev (car points))
                 (points (cdr points)))
        (when (pair? points)
          (draw-segment cv
                        prev
                        (car points)
                        (if (eq? state 'first) ch-beg ch-cnr)
                        ch-hor
                        ch-ver
                        (if (pair? (cdr points)) ch-cnr ch-end))
          (loop 'middle
                (car points)
                (cdr points)))))))


;; Show drawing from canvas.
;;
;; Args:
;;     cv        Canvas.
;;     [file]    Optional output file (default: stdout).
;;
(define (draw cv)
  (cv.get-lines-list cv))


;; Show drawing from canvas.
;;
;; Args:
;;     cv        Canvas.
;;     [file]    Optional output file (default: stdout).
;;
(define (show cv . rest)
  (let ((lines (draw cv)))
    (if (and (pair? rest) (car rest))
        (with-output-to-file (car rest)
          (lambda ()
            (for-each (lambda (line) (display line) (newline)) lines)))
        (for-each pr lines))))


;; ------------------------------------------------------------
;; Internal:

(define (stretch-string str len)
  (let ((chars (string->list str)))
    (case (length chars)
      ((1) (apply string (make-list len (car chars))))
      ((2) (apply string (cons (first chars) (make-list (1- len) (second chars)))))
      ((3)
       (case len
         ((0) "")
         ((1) (string (car chars)))
         ((2) (string (car chars) (last chars)))
         (else (apply string (append (list (car chars))
                                     (make-list (- len 2) (second chars))
                                     (list (last chars))))))))))


#;
(begin
  (define cv (cv.create))
  (line cv ">->" (p. 1 1) (p. 28 1))
  (line cv "+|+" (p. 1 1) (p. 1 14))
  (rectangle cv (p. 5 3) (p. 16 7))
  (text cv "Hello" (p. 8 5) 'down)
  (poly cv ":-|+>" (xy->points 20 10 24 10 24 6 30 6))
  (show cv)
  )

;; 000000000011111111112222222222333333333344444444445555555555666666666677777777778
;; 012345678901234567890123456789012345678901234567890123456789012345678901234567890
;;0
;;1    +--------+
;;2    | MyBox1 |:-----+
;;3    +--------+      |
;;4                    +------->
;;5                     +------+
;;6                     | Box3 |
;;7                     +------+
