(define-module (tuile canvas)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile massoc)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile coord)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-43)
  #:export
  (
   create
   set-layer
   get-layer-index
   put-ch
   put-str
   put-str-in-dir
   get-lines-vector
   get-lines-list
   ))


;; Multilayer canvas state.
(define-record-type canvas
  (fields (mutable layers)              ; Canvas layers '((0 . '(<chars>))), <char> = #(x y ch).
          (mutable lindex)              ; Layer index (li).
;;          (mutable chars)               ; Canvas characters #(li x y ch).
          (mutable xmax)                ; Maximum x within canvas.
          (mutable ymax)                ; Maximum y within canvas.
          ))


;; Create canvas at layer 0.
(define (create)
  (make-canvas (create-layer 0) 0 0 0))


;; Set active layer.
(define (set-layer cv li)
  (unless (massoc-has-key? (canvas-layers cv) li)
    (massoc-set! (canvas-layers cv) li '()))
  (canvas-lindex-set! cv li))


;; Return layer index.
(define (get-layer-index cv)
  (canvas-lindex cv))


;; Put char to position (on active layer).
(define (put-ch cv pos ch)
  (let ((x (px pos))
        (y (py pos)))
    (when (> x (canvas-xmax cv)) (canvas-xmax-set! cv x))
    (when (> y (canvas-ymax cv)) (canvas-ymax-set! cv y))
    (massoc-update! (canvas-layers cv)
                    (canvas-lindex cv)
                    (lambda (oldval)
                      (cons (vector (px pos) (py pos) ch)
                            oldval)))))


;; Put string of chars starting from position (on active layer).
(define (put-str cv pos str)
  (let loop ((rest (string->list str))
             (x (px pos)))
    (when (pair? rest)
      (put-ch cv
              (->p x (py pos))
              (car rest))
      (loop (cdr rest)
            (1+ x)))))


;; Put string of chars starting from position (on active layer) to
;; selected direction.
;;
;; dirs: 'up, 'down, 'left, 'right
;;
(define (put-str-in-dir cv pos str dir-spec)

  (define (step pos dir)
    (define (dec val) (if (> val 0) (1- val) 0))
    (define inc 1+)
    (case dir
      ((up)    (->p (px pos) (dec (py pos))))
      ((down)  (->p (px pos) (inc (py pos))))
      ((left)  (->p (dec (px pos)) (py pos)))
      ((right) (->p (inc (px pos)) (py pos)))))

  (let (
        (dir (->dir dir-spec))
        ;;(dir dir-spec)
        )
    (let loop ((rest (string->list str))
               (pos pos))
      (when (pair? rest)
        (put-ch cv
                pos
                (car rest))
        (loop (cdr rest)
              (step pos dir))))))


;; Get canvas content (lines) as vector.
(define (get-lines-vector cv)
  (let ((lines (apply vector (repeat (lambda (i)
                                       (make-bytevector (1+ (canvas-xmax cv))
                                                        (char->integer #\ )))
                                     (1+ (canvas-ymax cv))))))
    (let loop-layers ((sorted-li (stable-sort (massoc-keys (canvas-layers cv))
                                              <)))
      (when (pair? sorted-li)
        (let loop ((chars (reverse (canvas-chars cv (car sorted-li)))))
          (when (pair? chars)
            (let ((x (vector-ref (car chars) 0))
                  (y (vector-ref (car chars) 1))
                  (ch (vector-ref (car chars) 2)))
              (bytevector-u8-set! (vector-ref lines y) x (char->integer ch))
              (loop (cdr chars)))))
        (loop-layers (cdr sorted-li))))
    (vector-map (lambda (i line)
                  (utf8->string line))
                lines)))


;; Get canvas content (lines) as list.
(define (get-lines-list cv)
  (vector->list (get-lines-vector cv)))


#;
(begin
  (define cv (create))
  (put-ch cv '(3 . 1) #\a)
  (put-ch cv '(10 . 10) #\b)
  ;;(get-lines cv)
  (for-each pr (get-lines-as-string-list cv)))


;; ------------------------------------------------------------
;; Support:


(define (canvas-chars cv li)
  (massoc-ref (canvas-layers cv) li))


(define (create-layer li)
  (make-massoc (list (cons li '()))))

;;;; x-coord of pos.
;;(define px car)
;;
;;;; y-coord of pos.
;;(define py cdr)
;;
;;;; x and y to pos.
;;(define pos cons)
