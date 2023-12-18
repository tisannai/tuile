;;; module:
;;
;; Canvas is a library for drawing characters to multiple layers.
;;
;; Each layer includes characters as triplet: (x y ch). We store only
;; individual character triplets, but since we need to know what is
;; the maximum x-coord and y-coord for each layer, we maintain that
;; information per layer. Char is stored as vector of size 3.
;;
;; Layers are combined in order to create the canvas content. The
;; widest layer defines the canvas width and the tallest layer defines
;; the canvas height. Canvas content is an associative list with
;; layer-index layer-content pairs.
;;
;; Layers are accessed through a proxy. The proxy is
;; canvas/layer-index pair.
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
   map-layer
   from-layer
   hide-layer

   put-ch
   put-str
   put-str-in-dir
   del-pos
   del-area

   make-ch
   ch-p
   ch-x
   ch-y
   ch-c

   get-lines-vector
   get-lines-list
   put-lines-list
   canvas-size

   layer-index
   layer-indeces
   layer-content
   dimensions
   ))


;; Layer state.
(define-record-type layer
  (fields (mutable chars)               ; Char list.
          (mutable xmax)                ; Dimension x-max.
          (mutable ymax)                ; Dimension y-max.
          (mutable hide)                ; Hide layer (default: visible).
          ))

;; Proxy to specified canvas layer.
(define-record-type proxy
  (fields canvas                        ; Canvas massoc.
          lindex                        ; Current layer index.
          ))


(define make-ch vector)
(define (ch-p ch) (p. (ch-x ch) (ch-y ch))) ; Char position.
(define (ch-x ch) (vector-ref ch 0))    ; Char x.
(define (ch-y ch) (vector-ref ch 1))    ; Char y.
(define (ch-c ch) (vector-ref ch 2))    ; Char character.


;; Reset layer dimension values.
(define (layer-max-reset layer)
  (layer-xmax-set! layer -1)
  (layer-ymax-set! layer -1))


;; Update layer dimensions.
(define (layer-max-update layer char)
  (let ((x (ch-x char))
        (y (ch-y char)))
    (when (> x (layer-xmax layer)) (layer-xmax-set! layer x))
    (when (> y (layer-ymax layer)) (layer-ymax-set! layer y))))


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
    (layer-max-reset layer)
    (layer-chars-set! layer '())))


;; Delete given layer (through proxy). The last layer is never
;; deleted, but it is emptied if delete is requested for it. Also the
;; layer lindex is turned into 0.
(define (del-layer cv)
  (massoc-del! (proxy-canvas cv) (proxy-lindex cv))
  (if (massoc-empty? (proxy-canvas cv))
      (create)
      (make-proxy (proxy-canvas cv) (car (first (proxy-canvas cv))))))


;; Map layer characters to new state with proc. Proc is provided with
;; the char and it should return a new char. If return value is #f,
;; the char will be removed.
(define (map-layer cv proc)
  (let* ((layer (get-current-layer cv))
         (chars (layer-chars layer)))
    (layer-max-reset layer)
    (layer-chars-set! layer
                      (let lp ((chars chars)
                               (ret '()))
                        (if (pair? chars)
                            (let ((new-char (proc (car chars))))
                              (if new-char
                                  (begin
                                    (layer-max-update layer new-char)
                                    (lp (cdr chars)
                                        (cons new-char ret)))
                                  (lp (cdr chars)
                                      ret)))
                            (reverse ret))))))


;; Select layer characters that match pred.
(define (from-layer cv pred)
  (let* ((layer (get-current-layer cv))
         (chars (layer-chars layer)))
    (let lp ((chars chars)
             (ret '()))
      (if (pair? chars)
          (let ((res (pred (car chars))))
            (if res
                (lp (cdr chars)
                    (cons (car chars) ret))
                (lp (cdr chars)
                    ret)))
          (reverse ret)))))


;; Set layer "hide" status (false=visible, true=hidden).
(define (hide-layer cv state)
  (layer-hide-set! (get-current-layer cv) state))


;; Put char to position (on active layer).
(define (put-ch cv ch pos)
  (let ((layer (get-current-layer cv))
        (x (px pos))
        (y (py pos)))
    (when (> x (layer-xmax layer)) (layer-xmax-set! layer x))
    (when (> y (layer-ymax layer)) (layer-ymax-set! layer y))
    (layer-chars-set! layer (cons (make-ch (px pos) (py pos) ch) (layer-chars layer)))))


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


;; Delete char using position-predicate.
(define (del-by-pos-pred cv pred)
  (map-layer cv (lambda (ch) (if (pred (p. (ch-x ch) (ch-y ch)))
                                 #f
                                 ch))))


;; Delete char from position (on active layer).
(define (del-pos cv pos)
  (del-by-pos-pred cv (lambda (p) (equal? p pos))))


;; Delete char from position (on active layer).
(define (del-area cv a b)
  (del-by-pos-pred cv (lambda (p) (p-contained? p a b))))


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
            (let ((x (ch-x (car chars)))
                  (y (ch-y (car chars)))
                  (ch (ch-c (car chars))))
              (bytevector-u8-set! (vector-ref lines y) x (char->integer ch))
              (loop (cdr chars)))))
        (loop-layers (cdr sorted-li))))

    (vector-map (lambda (i line) (utf8->string line))
                lines)))


;; Get canvas content (lines) as list of strings.
(define (get-lines-list cv)
  (vector->list (get-lines-vector cv)))


;; Put lines of text to canvas.
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


;; Return the total number of characters for all layers combined.
(define (canvas-size cv)
  (let lp ((layers (proxy-canvas cv))
           (size 0))
    (if (pair? layers)
        (lp (cdr layers)
            (+ size (length (layer-chars (cdar layers)))))
        size)))


;; Current layer index.
(define (layer-index cv)
  (proxy-lindex cv))


;; All canvas indeces.
(define (layer-indeces cv)
  (massoc-keys (proxy-canvas cv)))


;; Current layer content.
(define (layer-content cv)
  (layer-chars (get-current-layer cv)))


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


;; Return chars of indexed layer, if visible.
(define (get-chars cv li)
  (let ((layer (get-layer cv li)))
    (if (layer-hide layer)
        '()
        (layer-chars layer))))

;; Create data for new layer.
(define (create-layer li)
  (make-layer '() -1 -1 #f))

;; Make sure layer exists, before use.
(define (ensure-layer cv li)
  (unless (massoc-has-key? (proxy-canvas cv) li)
    (massoc-set! (proxy-canvas cv) li (create-layer li))))

;; Return current layer.
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
