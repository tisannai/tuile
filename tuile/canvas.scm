;; module:
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
;;   #:use-module (tuile massoc)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile coord)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (first second third last find fold drop-right))
  #:use-module (srfi srfi-43)
  #:export
  (
   create
   use-layer
   push-layer
   pop-layer
   insert-layer
   clear-layer
   del-layer
   map-layer
   from-layer
   set-layer
   reset-layers
   swap-layers
   merge-layers
   hide-layer
   toggle-layer
   toggle-layers
   map-layers
   last-layer
   get-layer-copy

   put-ch
   put-str
   put-str-in-dir
   del-pos
   del-area
   del-ring

   make-ch
   ch-x
   ch-y
   ch-c
   ch-p

   get-content-view
   get-content
   get-content-view-for-active
   get-content-for-active
   put-lines-list
   canvas-size

   layer-index
   layer-last?
   layer-indeces
   layer-list
   layer-count
   layer-content
   layer-visibility
   layer-hide
   layer-hide-set!
   dimensions
   dimensions-for-active
   ))


;; Layer state.
(define-record-type layer
  (fields (mutable chars)               ; Char list.
          (mutable xmax)                ; Dimension x-max.
          (mutable ymax)                ; Dimension y-max.
          (mutable hide)                ; Hide layer (default: visible).
          ))


;; Proxy to canvas.
;;
;; Canvas user holds the proxy to the canvas. User can manipulate the
;; canvas throught this handle. Also, this proxy includes all
;; information about the canvas, and therefore it serves as the only
;; handle to the canvas resource.
;;
(define-record-type proxy
  (fields layers                        ; List of layers, in order.
          active                        ; Current layer.
          lindex                        ; Index of current layer.
          count                         ; Number of layers.
          ))


(define make-ch vector)
(define (ch-x ch) (vector-ref ch 0))    ; Char x.
(define (ch-y ch) (vector-ref ch 1))    ; Char y.
(define (ch-c ch) (vector-ref ch 2))    ; Char character.
(define (ch-c-set! ch char) (vector-set! ch 2 char))
(define (ch-p ch) (p. (ch-x ch) (ch-y ch))) ; Char position.


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


;; Create canvas as layer 0.
(define (create)
  (let ((active (create-layer)))
    (make-proxy (list active)
                active
                0
                1)))


(define (create-proxy-for-index cv li)
  (make-proxy (proxy-layers cv)
              (list-ref (proxy-layers cv) li)
              li
              (proxy-count cv)))


;; Create proxy for "cv" at layer index "lindex".
(define (use-layer cv li)
  (ensure-layer cv li))


;; Push layer as last layer.
(define (push-layer cv)
  (let ((active (create-layer)))
    (make-proxy (append (proxy-layers cv) (list active))
                active
                (proxy-count cv)
                (1+ (proxy-count cv)))))


;; Pop the last layer.
(define (pop-layer cv)
  (let ((layers (drop-right (proxy-layers cv) 1)))
    (make-proxy layers
                (last layers)
                (- (proxy-count cv) 2)
                (- (proxy-count cv) 1))))


;; Create proxy for "cv" at next layer index.
(define (insert-layer cv)
  (if (layer-last? cv)
      (push-layer cv)
      (let* ((layers (proxy-layers cv))
             (lindex (proxy-lindex cv))
             (update (let lp ((layers layers)
                              (i 0)
                              (ret '()))
                       (if (pair? layers)
                           (if (= i lindex)
                               (append (reverse ret)
                                       (list (create-layer))
                                       (cdr layers))
                               (lp (cdr layers)
                                   (1+ i)
                                   (cons (car layers) ret)))
                           (reverse (cons (create-layer) ret))))))
        (make-proxy update
                    (list-ref update (1+ lindex))
                    (1+ lindex)
                    (1+ (proxy-count cv))))))


;; Empty layer content.
;;
;; NOTE: the canvas maximum values are not
;; effected.
(define (clear-layer cv)
  (let ((layer (get-current-layer cv)))
    (layer-max-reset layer)
    (layer-chars-set! layer '())))


;; Delete indexed layer and return a new handle.
;;
;; case 1:
;;      cur -> 0 aaaa
;;             1 bbbb
;;      idx -> 2 cccc
;;             3 dddd
;;             4 eeee
;;
;; case 2:
;;             0 aaaa
;;      idx -> 1 bbbb
;;             2 cccc
;;      cur -> 3 dddd
;;             4 eeee
;;
;; case 3:
;;             0 aaaa
;;             1 bbbb
;;      idx -> 2 cccc <- cur
;;             3 dddd
;;             4 eeee
;;
;; case 4:
;;             0 aaaa
;;             1 bbbb
;;             2 cccc
;;             3 dddd
;;      idx -> 4 eeee <- cur
;;
(define (del-layer-by-index cv index)
  (if (= (proxy-count cv) 1)
      (create)
      (let* ((layers (proxy-layers cv))
             (last-layer? (= (1+ index) (proxy-count cv)))
             (update (let lp ((layers layers)
                              (i 0)
                              (ret '()))
                       (if (pair? layers)
                           (if (= i index)
                               (cond
                                ((= (1+ i) (proxy-count cv))
                                 (reverse ret))
                                (else
                                 (append (reverse ret)
                                         (cdr layers))))
                               (lp (cdr layers)
                                   (1+ i)
                                   (cons (car layers) ret)))
                           (reverse ret)))))
        (let ((updated-index (cond
                              ;; case 1:
                              ((< (proxy-lindex cv) index) (proxy-lindex cv))
                              ;; case 3 and 4:
                              ((= (proxy-lindex cv) index) (if last-layer? (1- index) index))
                              ;; case 2:
                              (else (- (proxy-lindex cv) 1)))))
          (make-proxy update
                      (list-ref update updated-index)
                      updated-index
                      (1- (proxy-count cv)))))))


;; Delete given layer (through proxy). The last layer is never
;; deleted, but it is emptied if delete is requested for it. Also the
;; layer lindex is turned into 0.
(define (del-layer cv)
  (del-layer-by-index cv (proxy-lindex cv)))


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


;; Set layer content.
(define (set-layer cv content)
  (let* ((layers (proxy-layers cv))
         (lindex (proxy-lindex cv))
         (update (let lp ((layers layers)
                          (i 0)
                          (ret '()))
                   (if (pair? layers)
                       (if (= i lindex)
                           (append (reverse ret)
                                   (list content)
                                   (cdr layers))
                           (lp (cdr layers)
                               (1+ i)
                               (cons (car layers) ret)))))))
    (make-proxy update
                (list-ref update lindex)
                lindex
                (proxy-count cv))))


;; Rename layer indeces to run from 0 to end, in order and with gaps.
(define (reset-layers cv) #f)


;; Swap content of layers, given layer indeces.
(define (swap-layers cv a b)

  (let ((layer-a (get-layer cv a))
        (layer-b (get-layer cv b)))

      (let* ((layers (proxy-layers cv))
             (lindex (proxy-lindex cv))
             (update (let lp ((layers layers)
                              (i 0)
                              (ret '()))
                       (if (pair? layers)
                           (lp (cdr layers)
                               (1+ i)
                               (cons (cond
                                      ((= i a) b)
                                      ((= i b) a)
                                      (else (car layers)))
                                     ret))
                           (reverse ret)))))
        (make-proxy update
                    (list-ref update lindex)
                    lindex
                    (proxy-count cv)))))


;; Merge to layer "a", the layer "b".
(define (merge-layers cv a b)
  (when (not (= a b))
    (let* ((in-order? (< a b)))
      (let* ((canvas (proxy-layers cv))
             (layer-a (get-layer cv a))
             (layer-b (get-layer cv b)))
        (if (and layer-a layer-b)
            (let ((content-b (layer-chars layer-b)))
              (for-each (lambda (ch)
                          (put-ch-to-layer layer-a (ch-c ch) (ch-p ch)))
                        content-b)
              (let ((cv (del-layer-by-index cv b)))
                (if in-order?
                    (create-proxy-for-index cv a)
                    (create-proxy-for-index cv (1- a)))))
            cv)))))


;; Set layer "hide" status (false=visible, true=hidden).
(define (hide-layer cv state)
  (layer-hide-set! (get-current-layer cv) state))


;; Toggle layer "hide" status.
(define (toggle-layer cv)
  (let ((cur (get-current-layer cv)))
    (layer-hide-set! cur (not (layer-hide cur)))))


(define (toggle-layers cv)
  (let* ((layers (layer-list cv))
         (has-hidden? (find (lambda (i) (layer-hide i)) layers)))
    (if has-hidden?
        (for-each (lambda (i) (layer-hide-set! i #f)) layers)
        (for-each (lambda (i) (layer-hide-set! i #t)) layers))))


(define (map-layers cv proc)
  (map proc (layer-list cv)))


;; Return proxy for last layer.
(define (last-layer cv)
  (create-proxy-for-index cv (1- (proxy-count cv))))

(define (get-layer-copy cv index)
  (let ((layer (get-layer cv index)))
    (make-layer (map vector-copy (layer-chars layer))
                (layer-xmax layer)
                (layer-ymax layer)
                (layer-hide layer))))

(define (set-layer! cv index)
  (let ((layer (get-layer cv index)))
    (make-layer (list-copy (layer-chars layer))
                (layer-xmax layer)
                (layer-ymax layer)
                (layer-hide layer))))


(define (put-ch-to-layer layer ch pos)
  (let ((x (px pos))
        (y (py pos)))
    (when (> x (layer-xmax layer)) (layer-xmax-set! layer x))
    (when (> y (layer-ymax layer)) (layer-ymax-set! layer y))
    (let ((old-ch (find (lambda (ch) (and (= x (ch-x ch))
                                          (= y (ch-y ch))))
                        (layer-chars layer))))
      (if old-ch
          (ch-c-set! old-ch ch)
          (layer-chars-set! layer (cons (make-ch (px pos) (py pos) ch) (layer-chars layer)))))))


;; Put char to position (on active layer).
(define (put-ch cv ch pos)
  (put-ch-to-layer (get-current-layer cv) ch pos))


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


;; Delete char from area (on active layer).
(define (del-area cv a b)
  (del-by-pos-pred cv (lambda (p) (p-contained? p a b))))


;; Delete char from ring (on active layer).
(define (del-ring cv a b)
  (del-by-pos-pred cv (lambda (p) (p-on-boundary? p a b))))


;; Get content view for layers
(define (get-content-view-for-layers layers p0 p1)
  (let* ((dims (p- p1 p0))
         (lines (make-vector (1+ (py dims)))))

    ;; Pre-fill bytevectors with spaces.
    (let lp ((i 0))
      (when (<= i (py dims))
        (vector-set! lines i (make-bytevector (1+ (px dims)) (char->integer #\ )))
        (lp (1+ i))))

    ;; Overwrite bytevectors with chars from layers in order.
    (let loop-layers ((layers layers))
      (when (pair? layers)
        (let loop ((chars (layer-visible-chars (car layers))))
          (when (pair? chars)
            (let ((x (ch-x (car chars)))
                  (y (ch-y (car chars)))
                  (ch (ch-c (car chars))))
              (when (p-contained? (p. x y) p0 p1)
                (bytevector-u8-set! (vector-ref lines (- y (py p0))) (- x (px p0)) (char->integer ch)))
              (loop (cdr chars)))))
        (loop-layers (cdr layers))))

    (vector-map (lambda (i line) (utf8->string line))
                lines)))


;; Return view (p0,p1) of content for specified area.
(define (get-content-view cv p0 p1)
  (get-content-view-for-layers (proxy-layers cv) p0 p1))


(define (get-content cv)
  (get-content-view cv (p. 0 0) (p- (dimensions cv) (p. 1 1))))


;; Return view (p0,p1) of content for specified area.
(define (get-content-view-for-active cv p0 p1)
  (get-content-view-for-layers (list (get-current-layer cv)) p0 p1))


;; Return pair including dimensions and vector of chars, for active layer only.
;;     ( <dims> <chars> )
(define (get-content-for-active cv)
  (get-content-view-for-active cv (p. 0 0) (p- (dimensions-for-active cv) (p. 1 1))))


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
  (let lp ((layers (proxy-layers cv))
           (size 0))
    (if (pair? layers)
        (lp (cdr layers)
            (+ size (length (layer-chars (cdar layers)))))
        size)))


;; Current layer index.
(define (layer-index cv)
  (proxy-lindex cv))


;; Current layer is last?
(define (layer-last? cv)
  (= (1+ (proxy-lindex cv)) (proxy-count cv)))


;; Return canvas indeces.
(define (layer-indeces cv)
;;   (massoc-keys (proxy-layers cv))
  (iota (1- (proxy-count cv))))


;; Return canvas layers.
(define (layer-list cv)
;;   (massoc-values (proxy-layers cv))
  (proxy-layers cv))


;; Return the number of layers.
(define (layer-count cv)
;;   (massoc-length (proxy-layers cv))
  (proxy-count cv))


;; Current layer content.
(define (layer-content cv)
  (reverse (layer-chars (get-current-layer cv))))

;; Return layer hide status.
(define (layer-visibility cv)
  (layer-hide (get-current-layer cv)))


;; Return canvas dimensions (size pair).
(define (dimensions cv)
  (let lp ((layers (layer-list cv))
           (xmax -1)
           (ymax -1))
    (if (pair? layers)
        (lp (cdr layers)
            (if (> (layer-xmax (car layers)) xmax) (layer-xmax (car layers)) xmax)
            (if (> (layer-ymax (car layers)) ymax) (layer-ymax (car layers)) ymax))
        (p. (1+ xmax) (1+ ymax)))))


;; Return canvas dimensions (size pair) for current layer only.
(define (dimensions-for-active cv)
  (let ((layer (get-current-layer cv)))
    (p. (1+ (layer-xmax layer))
        (1+ (layer-ymax layer)))))



;; ------------------------------------------------------------
;; Support:


;; Return chars of indexed layer, if visible.
(define (get-chars cv li)
  (let ((layer (get-layer cv li)))
    (if (layer-hide layer)
        '()
        (layer-chars layer))))

;; Create data for new layer.
(define (create-layer)
  (make-layer '() -1 -1 #f))

;; Return layer characters, if visible.
(define (layer-visible-chars layer)
  (if (layer-hide layer)
      '()
      (reverse (layer-chars layer))))

;; Make sure layer exists, before use.
(define (ensure-layer cv li)
  (if (< li (proxy-count cv))
      (create-proxy-for-index cv li)
      (push-layer cv)))

;; Return current layer.
(define (get-current-layer cv)
  (proxy-active cv))

;; Get active layer.
(define (get-layer cv li)
  (if (< li (proxy-count cv))
      (list-ref (proxy-layers cv) li)
      #f))


#;
(begin
  (define cv (create))
  (put-ch cv #\a (p. 3 1))
  (put-ch cv #\b (p. 10 10))
  (for-each pr (get-lines-list cv)))
