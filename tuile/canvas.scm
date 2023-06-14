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
;;    create-proxy
   use-layer
   layer-index
   layer-indeces
;;    set-layer
;;    get-layer-index
   put-ch
   put-str
   put-str-in-dir
   get-lines-vector
   get-lines-list
   dimensions
   ))


;; Layer state.
(define-record-type layer
  (fields (mutable chars)
          (mutable xmax)
          (mutable ymax)
          ))


;; ;; Multilayer canvas state.
;; (define-record-type canvas
;;   (fields (mutable layers)              ; Canvas layers '((0 . '(<chars>))), <char> = #(x y ch).
;;           (mutable lindex)              ; Layer index (li).
;; ;;           (mutable xmax)                ; Maximum x within canvas.
;; ;;           (mutable ymax)                ; Maximum y within canvas.
;;           ))


;; Proxy to specified canvas layer.
(define-record-type proxy
  (fields canvas
          lindex
          ))


;; Create canvas at layer 0.
(define (create)
  ;; (make-canvas (create-layer 0) 0 0 0)
  ;; (make-canvas (make-massoc (list (cons 0 (create-layer 0)))) 0)
  (make-proxy (make-massoc (list (cons 0 (create-layer 0))))
              0))


;; Create proxy for "cv" at layer index "lindex".
(define (use-layer cv li)
  (ensure-layer cv li)
  (make-proxy (proxy-canvas cv) li))


;; ;; Reference canvas directly or indirectly with canvas-proxy.
;; (define (r/ cv)
;;   (case (record-type cv)
;;     ((canvas) cv)
;;     ((proxy) (proxy-canvas cv))))
;;
;;
;; ;; Reference layer directly or indirectly with canvas-proxy.
;; (define (l/ cv li)
;;   (massoc-ref (canvas-layers (r/ cv)) li))


;; ;; Set active layer.
;; (define (set-layer cv li)
;;   (ensure-layer cv li)
;;   (canvas-lindex-set! cv li))


;; ;; Return layer index.
;; (define (get-layer-index cv)
;;   (case (record-type cv)
;;     ((canvas) (canvas-lindex cv))
;;     ((proxy) (proxy-lindex cv))))


(define (layer-index cv)
  (proxy-lindex cv))

(define (layer-indeces cv)
  (massoc-keys (proxy-canvas cv)))


;; Put char to position (on active layer).
#;
(define (put-ch cv ch pos)
  (let ((x (px pos))
        (y (py pos)))
    (when (> x (canvas-xmax (r/ cv))) (canvas-xmax-set! (r/ cv) x))
    (when (> y (canvas-ymax (r/ cv))) (canvas-ymax-set! (r/ cv) y))
    (massoc-update! (canvas-layers (r/ cv))
                    (get-layer-index cv)
                    (lambda (oldval)
                      (cons (vector (px pos) (py pos) ch)
                            oldval)))))

;; Put char to position (on active layer).
(define (put-ch cv ch pos)
  (let ((layer (get-current-layer cv))
        (x (px pos))
        (y (py pos)))
    (when (> x (layer-xmax layer)) (layer-xmax-set! layer x))
    (when (> y (layer-ymax layer)) (layer-ymax-set! layer y))
    (layer-chars-set! layer (cons (vector (px pos) (py pos) ch) (layer-chars layer)))))


;; Put string of chars starting from position (on active layer).
(define (put-str cv str pos)
  (let loop ((rest (string->list str))
             (x (px pos)))
    (when (pair? rest)
      (put-ch cv
              (car rest)
              (p. x (py pos)))
      (loop (cdr rest)
            (1+ x)))))


;; Put string of chars starting from position (on active layer) to
;; selected direction.
;;
;; dirs: 'up, 'down, 'left, 'right
;;
(define (put-str-in-dir cv str pos dir)

  (define (step pos dir)
    (define (dec val) (if (> val 0) (1- val) 0))
    (define inc 1+)
    (case dir
      ((up)    (p. (px pos) (dec (py pos))))
      ((down)  (p. (px pos) (inc (py pos))))
      ((left)  (p. (dec (px pos)) (py pos)))
      ((right) (p. (inc (px pos)) (py pos)))))

  (let loop ((rest (string->list str))
             (pos pos))
    (when (pair? rest)
      (put-ch cv
              (car rest)
              pos)
      (loop (cdr rest)
            (step pos dir)))))


;; Get canvas content (lines) as vector of strings.
#;
(define (get-lines-vector cv)
  (let ((lines (apply vector (repeat (lambda (i)
                                       (make-bytevector (1+ (canvas-xmax (r/ cv)))
                                                        (char->integer #\ )))
                                     (1+ (canvas-ymax (r/ cv)))))))
    (let loop-layers ((sorted-li (stable-sort (massoc-keys (canvas-layers (r/ cv)))
                                              <)))
      (when (pair? sorted-li)
        (let loop ((chars (reverse (canvas-chars (r/ cv) (car sorted-li)))))
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


;; Get canvas content (lines) as vector of strings.
(define (get-lines-vector cv)
  (let* ((dims (dimensions cv))
         (lines (make-vector (py dims))))

    ;; Pre-fill bytevectors with spaces.
    (let lp ((i 0))
      (when (< i (py dims))
        (vector-set! lines i (make-bytevector (px dims) (char->integer #\ )))
        (lp (1+ i))))

    ;; Overwrite bytevectros with chars from layers in order.
    (let loop-layers ((sorted-li (stable-sort (massoc-keys (proxy-canvas cv))
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

    (vector-map (lambda (i line) (utf8->string line))
                lines)))


;; Get canvas content (lines) as list of strings.
(define (get-lines-list cv)
  (vector->list (get-lines-vector cv)))


;; Return canvas dimensions (size pair).
(define (dimensions cv)
  (let lp ((layers (massoc-values (proxy-canvas cv)))
           (xmax -1)
           (ymax -1))
    (if (pair? layers)
        (lp (cdr layers)
            (if (> (layer-xmax (car layers)) xmax) (layer-xmax (car layers)) xmax)
            (if (> (layer-ymax (car layers)) ymax) (layer-ymax (car layers)) ymax))
        (p. (1+ xmax) (1+ ymax)))))


;; ------------------------------------------------------------
;; Support:


(define (canvas-chars cv li)
  (layer-chars (get-layer cv li)))


;; (define (create-layer li)
;;   (make-massoc (list (cons li '()))))

(define (create-layer li)
  (make-layer '() -1 -1))


(define (ensure-layer cv li)
  (unless (massoc-has-key? (proxy-canvas cv) li)
    (massoc-set! (proxy-canvas cv) li (create-layer li))))

(define (get-current-layer cv)
  (get-layer cv (proxy-lindex cv)))

;; Get active layer.
(define (get-layer cv li)
  (massoc-ref (proxy-canvas cv) li))


;;;; x-coord of pos.
;;(define px car)
;;
;;;; y-coord of pos.
;;(define py cdr)
;;
;;;; x and y to pos.
;;(define pos cons)


#;
(begin
  (define cv (create))
  (put-ch cv #\a (p. 3 1))
  (put-ch cv #\b (p. 10 10))
  ;;(get-lines cv)
  (for-each pr (get-lines-list cv)))
