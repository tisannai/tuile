;;; module:
;;
;; Canvas is a library for drawing characters to multiple layers.
;;
;; Each layer includes characters as triplet: (x y ch). We store only
;; individual character triplets, but since we need to know what is
;; the maximum x-coord and y-coord for each layer, we maintain that
;; information per layer.
;;
;; Layers are combined in order to create the canvas content. The
;; widest layer defines the canvas width and the tallest layer defines
;; the canvas height.
;;
;; Layers are accessed through a proxy.
;;
(define-module (tuile canvas)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile massoc)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile coord)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (first second third))
  #:use-module (srfi srfi-43)
  #:export
  (
   create
   use-layer
   push-layer
   clear-layer
   del-layer

   put-ch
   put-str
   put-str-in-dir
   del-pos
   del-area

   get-lines-vector
   get-lines-list
   put-lines-list
   canvas-size

   layer-index
   layer-indeces
   dimensions
   ))


;; Layer state.
(define-record-type layer
  (fields (mutable chars)
          (mutable xmax)
          (mutable ymax)
          ))

;; Proxy to specified canvas layer.
(define-record-type proxy
  (fields canvas
          lindex
          ))



;; Create canvas at layer 0.
(define (create)
  (make-proxy (make-massoc (list (cons 0 (create-layer 0))))
              0))


;; Create proxy for "cv" at layer index "lindex".
(define (use-layer cv li)
  (ensure-layer cv li)
  (make-proxy (proxy-canvas cv) li))


;; Create proxy for "cv" at next layer index.
(define (push-layer cv)
  (if cv
      (let ((li (1+ (proxy-lindex cv))))
        (ensure-layer cv li)
        (make-proxy (proxy-canvas cv) li))
      (create)))


;; Empty layer content.
(define (clear-layer cv)
  (let ((layer (get-current-layer cv)))
    (layer-xmax-set! layer -1)
    (layer-ymax-set! layer -1)
    (layer-chars-set! layer '())))


;; Delete given layer (through proxy). The last layer is never
;; deleted, but it is emptied if delete is requested for it. Also the
;; layer lindex is turned into 0.
(define (del-layer cv)
  (massoc-del! (proxy-canvas cv) (proxy-lindex cv))
  (if (massoc-empty? (proxy-canvas cv))
      (create)
      (make-proxy (proxy-canvas cv) (car (first (proxy-canvas cv))))))


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


(define (del-by-pred cv pred)
  (let* ((layer (get-current-layer cv))
         (chars-and-dims (let lp ((chars (layer-chars layer))
                                  (xmax -1)
                                  (ymax -1)
                                  (ret '()))
                           (if (pair? chars)
                               (let* ((char (car chars))
                                      (x (vector-ref char 0))
                                      (y (vector-ref char 1)))
                                 (if (pred (p. x y))
                                     (lp (cdr chars) xmax ymax ret)
                                     (lp (cdr chars)
                                         (if (> x xmax) x xmax)
                                         (if (> y ymax) y ymax)
                                         (cons char ret))))
                               (list (reverse ret) xmax ymax)))))
    (layer-chars-set! layer (first chars-and-dims))
    (layer-xmax-set! layer (second chars-and-dims))
    (layer-ymax-set! layer (third chars-and-dims))))


;; Delete char from position (on active layer).
(define (del-pos cv pos)
  (del-by-pred cv (lambda (p) (equal? p pos))))


;; Delete char from position (on active layer).
(define (del-area cv a b)
  (del-by-pred cv (lambda (p) (p-contained? p a b))))


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
        (let loop ((chars (reverse (get-chars cv (car sorted-li)))))
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

(define (put-lines-list cv lines)
  (let lp ((lines lines)
           (y 0))
    (if (pair? lines)
        (let lp2 ((x 0))
          (if (< x (string-length (car lines)))
              (let ((ch (string-ref (car lines) x)))
                (when (not (char=? ch #\ ))
                  (put-ch cv ch (p. x y)))
                (lp2 (1+ x)))
              (lp (cdr lines)
                  (1+ y)))))))


(define (canvas-size cv)
  (let lp ((layers (proxy-canvas cv))
           (size 0))
    (if (pair? layers)
        (lp (cdr layers)
            (+ size (length (layer-chars (cdar layers)))))
        size)))


(define (layer-index cv)
  (proxy-lindex cv))

(define (layer-indeces cv)
  (massoc-keys (proxy-canvas cv)))

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

(define (get-chars cv li)
  (layer-chars (get-layer cv li)))

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


#;
(begin
  (define cv (create))
  (put-ch cv #\a (p. 3 1))
  (put-ch cv #\b (p. 10 10))
  ;;(get-lines cv)
  (for-each pr (get-lines-list cv)))
