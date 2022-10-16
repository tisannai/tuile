(define-module (tuile svg)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile coord)
  #:use-module (srfi srfi-1)
  #:export
  (
   svg-create
   svg-text
   svg-text-left
   svg-text-center
   svg-text-right
   svg-poly
   svg-draw

   svg-text-width

   ))


;; SVG drawing context.
(define-record-type svgctx
  (fields (mutable elems)               ; List of figure elements.
          (mutable ux)                  ; Scaling for x-dim.
          (mutable uy)                  ; Scaling for y-dmi.
          (mutable line-color)          ; Default line color.
          (mutable line-width)          ; Default line width.
          (mutable font-size)           ; Default font size.
          (mutable dims)                ; Calculated figure dims (i.e. max elem corner).
          ))

;; Add elem to context.
(define (svg-add-elem! s elem)
  (svgctx-elems-set! s
                     (append (svgctx-elems s)
                             (list elem))))

;; Return font size.
(define (svg-font-size s)
  (svgctx-font-size s))


;; ------------------------------------------------------------
;; API:

;; Create svg-context with defaults.
(define* (svg-create #:key (ux 10) (uy 10))
  (make-svgctx (list)
               ux
               uy
               "black"
               1
               10
               #f))

;; Add text element.
;;
;;     (svg-text s (p. 1 1) 'left "foobar")
;;
(define (svg-text s pos alignment text)
  (svg-add-elem! s (list 'text pos alignment text)))

;; Add text element with left alignment for text.
(define (svg-text-left s pos text)
  (svg-add-elem! s (list 'text pos 'left text)))

;; Add text element with center alignment for text.
(define (svg-text-center s pos text)
  (svg-add-elem! s (list 'text pos 'center text)))

;; Add text element with right alignment for text.
(define (svg-text-right s pos text)
  (svg-add-elem! s (list 'text pos 'right text)))


;; Add polyline.
;;
;;     (svg-poly s (xy->points 5 1 5 2 6 2 6 1 7 1 7 2 8 2 8 1))
(define (svg-poly s points)
  (svg-add-elem! s (list 'poly points)))

;; API:
;; ------------------------------------------------------------




;; ------------------------------------------------------------
;; Utils:

;; Low-level text width calculation.
(define (text-width font-size text)

  (define svg-char-width-percentages
    (reverse (map (lambda (spec) (cons (string->list (first spec))
                                       (second spec)))
                  (list
                   '("lij|' " 40.0)
                   '("![]fI.,;/\\t" 50.0)
                   '("`-(){}r\""  60.0)
                   '("*^zcsJkvxy" 70.0)
                   '("aebdhnopqug#$L+<>=?_~FZT0123456789" 70.0)
                   '("BSPEAKVXY&UwNRCHD" 70.0)
                   '("QGOMm%W@" 100.0)
                   ))))

  (define (char-width ch)
    (let* ((table svg-char-width-percentages)
           (find-ch (lambda (ch)
                      (call/cc
                       (lambda (cc)
                         (let loop ((rest table))
                           (if (pair? rest)
                               (let ((res (find (lambda (ref) (char=? ch ref)) (caar rest))))
                                 (if res
                                     (cc (cdar rest))
                                     (loop (cdr rest))))
                               #f)))))))
      (find-ch ch)))

  (/ (* font-size (apply + (map char-width (string->list text))))
     100.0))


;; Width of text without context scaling (i.e. in drawing units).
(define (svg-text-width-without-scaling s text)
  (inexact->exact (ceiling (text-width (svgctx-font-size s) text))))


;; Width of text in with scaling (i.e. in user units).
(define (svg-text-width s text)
  (inexact->exact (ceiling (/ (text-width (svgctx-font-size s) text)
                              (svgctx-ux s)))))

;; Utils:
;; ------------------------------------------------------------


;; Scale position.
(define (svgctx-scale s pos)
  (p. (* (svgctx-ux s) (px pos))
      (* (svgctx-uy s) (py pos))))


;; Macro for "svgctx-scale" short syntax.
;;
;;     (->s <pos>)
;;
(define-syntax ->s
  (lambda (x)
    (let ((stx (syntax->datum x)))
      (with-syntax ((s (datum->syntax x 's)))
        #`(svgctx-scale s #,(datum->syntax x (second stx)))))))

;; Draw polypath.
;;
;;     <path d="M 100 100 300 100 200 300"
;;     fill="red" stroke="blue" stroke-width="3" />
;;
(define (svg-draw-poly s points)
  (list (ss "  <path")
        (apply ss (cons "      d=\"M"
                        (append
                         (map (lambda (pos)
                                (let ((sp (svgctx-scale s pos)))
                                  (ss " " (px sp) " " (py sp))))
                              points)
                         (list "\""))))
        (ss "      fill=\"none\"")
        (si "      stroke=\"#{(svgctx-line-color s)}\"")
        (si "      stroke-width=\"#{(svgctx-line-width s)}\"")
        (ss "      />")))

;; Draw text.
(define (svg-draw-text s pos alignment text)
  (let ((sp (svgctx-scale s pos))
        (align (case alignment
                 ((left) "start")
                 ((center) "middle")
                 ((right) "end"))))
    (list (ss "  <text")
          (si "      fill=\"#{(svgctx-line-color s)}\"")
          (si "      text-anchor=\"#{align}\"")
          (si "      font-family=\"DejaVu Sans\"")
          (si "      font-size=\"#{(svgctx-font-size s)}\"")
          (si "      stroke=\"#{(svgctx-line-color s)}\"")
          (si "      stroke-width=\"0.25\"")
          (si "      x=\"#{(px sp)}\"")
          (si "      y=\"#{(py sp)}\"")
          (si "      >#{text}</text>"))))


;; Elem accessor funcs.
(define svg-elem-type first)
(define svg-text-pos second)
(define svg-text-alignment third)
(define svg-text-text fourth)
(define svg-poly-points second)


;; Draw all elements in svg-context.
;;
;; Define dimensions of the drawing by anylysing the furtherest
;; corners of contained elements.
;;
(define (svg-draw s)

  ;; Find max x and y coords (separately) from the "pos-lst".
  (define (find-max-dim pos-lst)
    (let loop ((tail pos-lst)
               (x 0)
               (y 0))
      (if (pair? tail)
          (let ((d (car tail)))
            (loop (cdr tail)
                  (if (> (px d) x) (px d) x)
                  (if (> (py d) y) (py d) y)))
          (p. x y))))

  ;; Return the maximum distance of elem from origin.
  (define (get-elem-dim e)
    (case (svg-elem-type e)
      ((text) (case (svg-text-alignment e)
                ((left) (let ((pos (->s (svg-text-pos e))))
                          (p. (p+x pos (svg-text-width-without-scaling s (svg-text-text e)))
                              (py pos))))
                ((center) (let ((pos (->s (svg-text-pos e))))
                            (p. (p+x pos (quotient (svg-text-width-without-scaling s (svg-text-text e))
                                                   2))
                                (py pos))))
                ((right) (->s (svg-text-pos e)))))
      ((poly) (find-max-dim (map (lambda (p) (svgctx-scale s p)) (svg-poly-points e))))))

  ;; Find max of all elems.
  (define max-dim (find-max-dim (map get-elem-dim (svgctx-elems s))))

  ;; Assume A4 is 160mm wide.
  (define paper-width 160)
  ;; Use fixed relations to width, in order to keep x-y ratio fixed.
  (define paper-heights (list 40 80 120 160 200 240 280))

  ;; Quantisize (normalize) y to "paper-height" options.
  (define (get-dim-y y)
    (let loop ((tail paper-heights))
      (if (pair? tail)
          (if (< y (car tail))
              (car tail)
              (loop (cdr tail)))
          (last paper-heights))))

  ;; Scaling for fitting in to "fixed" paper-width.
  (define scale (if (<= (px max-dim) paper-width)
                    1
                    (1+ (quotient (1- (px max-dim)) paper-width))))

  ;; Final x and y dims.
  (define dims (p. (* scale paper-width)
                   (* scale (get-dim-y (py max-dim)))))

  (define (svg-draw-header s)
    (list (ss "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")
          (ss "<svg")
          (si "    width=\"#{paper-width}mm\"")
          (si "    height=\"#{(quotient (py dims) scale)}mm\"")
          (si "    viewBox=\"0 0 #{(px dims)} #{(py dims)}\"")
          (ss "    version=\"1.1\"")
          (ss "    >")))

  (define (svg-draw-footer s)
    (list (ss "</svg>")))

  (define (svg-draw-elem args)
    (case (svg-elem-type args)
      ((text) (apply svg-draw-text (cons s (cdr args))))
      ((poly) (apply svg-draw-poly (cons s (cdr args))))))

  (append (svg-draw-header s)
          (apply append (map svg-draw-elem (svgctx-elems s)))
          (svg-draw-footer s)))


;; Test:
;;(define s (svg-create #:ux 30 #:uy 20))
;;(svg-text-right s (p. 4 2) "clk")
;;(svg-poly s (xy->points 5 1 5 2 6 2 6 1 7 1 7 2 8 2 8 1))
;;(pl (svg-draw s))
