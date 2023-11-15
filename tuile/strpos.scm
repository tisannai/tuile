(define-module (tuile strpos)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile pr)
  #:use-module ((tuile utils) #:select (string-ref-safe char-separator? char-nonseparator?))
  #:export
  (
   strpos-make
   strpos-create
   strpos-clone
   strpos-next
   strpos-prev
   strpos-step
   strpos-next-line
   strpos-find

   strpos-set!
   strpos-next!
   strpos-prev!
   strpos-step!
   strpos-next-line!
   strpos-find!

   strpos-char
   strpos-line
   strpos-ref
   strpos-sub
   strpos-tail
   strpos-text
   strpos-index
   strpos-length
   strpos-distance
   strpos-end?
   strpos-step?
   strpos-find?
   ))


;; ------------------------------------------------------------
;; Manipulation:

;; (define-record-type strpos
;;   (fields (mutable index)
;;           text))

(define create cons)


(define (strpos-make index text)
  (create index text))

(define (strpos-create text)
  (create 0 text))

(define (strpos-clone sp)
  (create (strpos-index sp)
                 (strpos-text sp)))

(define (strpos-next sp)
  (if (strpos-end? sp)
      sp
      (create (+ (strpos-index sp) 1)
                     (strpos-text sp))))

(define (strpos-prev sp)
  (if (= (strpos-index sp) 0)
      sp
      (create (- (strpos-index sp) 1)
                     (strpos-text sp))))

(define (strpos-step sp step)
  (when (strpos-step? sp step)
    (if (< step 0)
        (create (- (strpos-index sp) step) (strpos-text sp))
        (create (+ (strpos-index sp) step) (strpos-text sp)))))

(define (strpos-next-line sp)
  (cond
   ((strpos-end? sp) sp)
   ((char=? (strpos-char sp) #\newline) (strpos-next sp))
   (else
    (strpos-next-line (strpos-next sp)))))

;; Return next position that matches "pred", a character predicate.
;;
;; Typical predicates:
;;
;;     char-whitespace?
;;     (negate char-whitespace?) | char-nonwhite?
;;     char-numeric?
;;
(define (strpos-find sp pred)
  (let ((find (strpos-find? sp pred)))
    (if find
        (create find (strpos-text sp))
        sp)))

(define (strpos-set! a b)
  (set-car! a (car b))
  (set-cdr! b (cdr b)))

(define (strpos-next! sp) (strpos-set! sp (strpos-next sp)))
(define (strpos-prev! sp) (strpos-set! sp (strpos-prev sp)))
(define (strpos-step! sp step) (strpos-set! sp (strpos-step sp step)))
(define (strpos-next-line! sp) (strpos-set! sp (strpos-next-line sp)))
(define (strpos-find! sp pred) (strpos-set! sp (strpos-find sp pred)))

;; ------------------------------------------------------------
;; Queries:

(define (strpos-char sp)
  (if (strpos-end? sp)
      #f
      (string-ref (strpos-text sp) (strpos-index sp))))

(define (strpos-line sp)
  (if (strpos-end? sp)
      #f
      ;; Find next newline or end.
      (let ((a (strpos-index sp)))
        (let lp ((b (strpos-index sp)))
          (if (or (>= b (strpos-length sp))
                  (char=? (string-ref (strpos-text sp) b) #\newline))
              (substring (strpos-text sp) a b)
              (lp (1+ b)))))))

(define (strpos-ref sp index)
  (string-ref-safe (strpos-text sp) index))

(define (strpos-sub sp len)
  (if (strpos-step? sp len)
      (substring (strpos-text sp) (strpos-index sp) (+ (strpos-index sp) len))
      #f))

(define (strpos-tail sp)
  (if (strpos-end? sp)
      #f
      (substring (strpos-text sp) (strpos-index sp))))

(define strpos-text cdr)

(define strpos-index car)

(define (strpos-length sp)
  (string-length (strpos-text sp)))

(define (strpos-distance sa sb)
  (- (strpos-index sb) (strpos-index sa)))

(define (strpos-end? sp)
  (>= (strpos-index sp) (strpos-length sp)))

(define (strpos-step? sp step)
  (cond
   ((>= step 0) (<= (+ (strpos-index sp) step) (strpos-length sp)))
   (else (> (- (strpos-index sp) step) 0))))

(define (strpos-find? sp pred)
  (let* ((text (strpos-text sp))
         (len (string-length text)))
    (let lp ((i (strpos-index sp)))
      (cond
       ((pred (string-ref-safe text i)) i)
       ((>= i len) #f)
       (else (lp (1+ i)))))))

#;
(begin
  (define sp (strpos-create "foobar hii haa"))
  (pd (strpos-find sp char-whitespace?))
  )
