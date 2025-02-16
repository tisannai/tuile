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
   freeline-dots
   freeline
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
;;
;;       ---       ---
;;        :         -
;;         :         '.
;;         :           -
;;         :            '.
;;          :             -
;;         ---           ---
;;
;;       +-----+-----+-----+-----+-----+-----+
;;       |     |     |     |     |     |     |
;;       |     |     |     |     |     |     |
;;       |  +  |     |     |     |     |     |
;;       |     |  +  |     |     |     |     |
;;       |     |     |  +  |     |     |     |
;;       +-----+-----+-----+-----+-----+-----+
;;       |     |     |     |  +  |     |     |
;;       |     |     |     |     |  +  |     |
;;       |     |     |     |     |     |  +  |
;;       |     |     |     |     |     |     |
;;       |     |     |     |     |     |     |
;;       +-----+-----+-----+-----+-----+-----+
;;
;;    +--------------+--------------+--------------+--------------+--------------+
;;    |              |              |              |              |              |
;;    |      --      |              |              |              |              |
;;    |              |      --      |              |              |              |
;;    +--------------+--------------+------oo------+--------------+--------------+
;;    |              |              |              |      --      |              |
;;    |              |              |              |              |      --      |
;;    |              |              |              |              |              |
;;    +--------------+--------------+--------------+--------------+--------------+
;;
;;
(define (freeline-dots pa pb)

  (define (hdots pa pb vs)
    (let* ((xa (px pa))
           (xb (px pb))
           (ya (py pa))
           (yb (py pb))
           ;; Calculate in positive values and adjust when result is applied.
           (ch-mirror (case (car vs)
                        ((1 2 6 7) #f)
                        (else #t)))
           (x-offset (case (car vs)
                       ((1 2 14 15) +)
                       (else -)))
           (y-offset (case (car vs)
                       ((1 2 6 7) +)
                       (else -)))
           (limit (abs (- xb xa)))
           (a-sign (case (car vs)
                     ((1 2 9 10) +1.0)
                     (else -1.0))))
      ;; (ppr (list "hdots: pa/pb/vs" pa pb vs))
      (let lp ((i 0)
               (ret '()))
        (let* ( ;; NOTE: vs has real slope, i.e. it is y-height adjusted.
               ;; Offset y, real
               (oyr (/ (+ (* i (* a-sign (cdr vs)))
                          1.0)
                       2.0))
               ;; Offset y, integer-fraction
               (oyif (->integer-fraction oyr 2))
               ;; Offset y, fraction
               (oyf (cdr oyif))
               ;; Offset y, integer
               (oyi (car oyif))
               ;; Char
               (ch (if ch-mirror
                       (cond
                        ((< oyf 33) #\.)
                        ((> oyf 66) #\')
                        (else #\-))
                       (cond
                        ((< oyf 33) #\')
                        ((> oyf 66) #\.)
                        (else #\-))
                       )))
          (if (<= i limit)
              (lp (1+ i)
                  (cons (cons ch (p. (x-offset xa i)
                                     (y-offset ya oyi)))
                        ret))
              (reverse ret))))))

  (define (vdots pa pb vs)
    (let* ((xa (px pa))
           (xb (px pb))
           (ya (py pa))
           (yb (py pb))
           ;; Calculate in positive values and adjust when result is applied.
           (ch-mirror (case (car vs)
                        ((3 5) #f)
                        (else #t)))
           (x-offset (case (car vs)
                       ((3 13) +)
                       (else -)))
           (y-offset (case (car vs)
                       ((3 5) +)
                       (else -)))
           (limit (abs (- yb ya)))
           (a-sign (case (car vs)
                     ((3 11) +1.0)
                     (else -1.0))))
      ;; (ppr (list "vdots: pa/pb/vs" pa pb vs))
      (let lp ((i 0)
               (ret '()))
        (let* ( ;; NOTE: vs has real slope, i.e. it is y-height adjusted.
               ;; Offset x, real
               (oxr (+ (/ i (* a-sign (cdr vs)) 0.5)
                       0.5))
               ;; Offset x, integer-fraction
               (oxif (->integer-fraction oxr 2))
               ;; Offset x, fraction
               (oxf (cdr oxif))
               ;; Offset x, integer
               (oxi (car oxif))
               ;; Char
               (ch (if ch-mirror
                       (cond
                        ((< oxf 33) #\')
                        ((> oxf 66) #\.)
                        (else #\-))
                       (cond
                        ((< oxf 33) #\.)
                        ((> oxf 66) #\')
                        (else #\-))
                       )))
          (if (<= i limit)
              (lp (1+ i)
                  (cons (cons ch (p. (x-offset xa oxi)
                                     (y-offset ya i)))
                        ret))
              (reverse ret))))))

  (if (p= pa pb)
      ;; Singular.
      (list (cons #\- pa))
      ;; Non-singular.
      (let* ((vv (p. (- (px pb) (px pa))
                     (* 2 (- (py pb) (py pa)))))
             (p0 (p. 0 0))
             ;; "vs" has real slope, i.e. it is y-height adjusted.
             (vs (p-p-type-and-slope p0 vv)))
        (case (car vs)
          ((0 8) (let* ((limit (p-p-len pa pb))
                        (ret (make-vector limit))
                        (x-offset (case (car vs)
                                    ((0) p+x)
                                    (else p-x))))
                   (let lp ((i 0))
                     (if (< i limit)
                         (begin
                           (vector-set! ret i (cons #\- (x-offset pa i)))
                           (lp (1+ i)))
                         (vector->list ret)))))
          ((4 12) (let* ((limit (p-p-len pa pb))
                         (ret (make-vector limit))
                         (y-offset (case (car vs)
                                     ((4) p+y)
                                     (else p-y))))
                    (let lp ((i 0))
                      (if (< i limit)
                          (begin
                            (vector-set! ret i (cons #\: (y-offset pa i)))
                            (lp (1+ i)))
                          (vector->list ret)))))
          ((1 2 6 7 9 10 14 15)
           (hdots pa pb vs))
          ((3 5 11 13)
           (vdots pa pb vs))))))


(define (freeline cv pa pb)
  (for-each (lambda (dot)
              (cv.put-ch cv (car dot) (cdr dot)))
            (freeline-dots pa pb)))


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

#;
(begin
  (define cv (cv.create))

  (define (draw-circle)
    (let ((po (p. 40 20))
        (len 32)
        (ix (lambda (f) (inexact->exact (round f))))
        (iy (lambda (f) (inexact->exact (truncate f)))))
    (let lp ((i 0))
      (when (< i 16)
        (let ((angle (* i (/ (* 2 pi) 16.0))))
          (ppr (p. (ix (* len (cos angle)))
                   (iy (* 0.5 len (sin angle)))))
          (lp (1+ i)))))))

  (let ((po (p. 40 20))
        (endpoints '((32 . 0)           ; 0
                     (30 . 6)
                     (22 . 11)
                     (12 . 14)
                     (0 . 16)           ; 4
                     (-12 . 14)
                     (-22 . 11)
                     (-30 . 6)
                     (-32 . 0)          ; 8
                     (-30 . -6)
                     (-22 . -11)
                     (-12 . -14)
                     (0 . -16)          ; 12
                     (12 . -14)
                     (22 . -11)
                     (30 . -6))))
    ;;     (freeline cv po (p+ po (list-ref endpoints 13)))
    ;;     (show cv)
    ;;     (exit 1)
    (let lp ((i 0))
      (when (< i 16)
        (freeline cv po (p+ po (list-ref endpoints i)))
        (lp (1+ i))))
    )

  (freeline cv (p. 0 40) (p. 12 40))
  (show cv)
  )
