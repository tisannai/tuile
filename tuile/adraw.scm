(define-module (tuile adraw)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-43)
  #:export
  (
   open
   put-ch
   put-str
   get-lines-vector
   get-lines-list
   ))


(define-record-type canvas
  (fields (mutable chars)
          (mutable xmax)
          (mutable ymax)))

(define (open)
  (make-canvas '() 0 0))

(define (put-ch dr pos ch)
  (let ((x (car pos))
        (y (cdr pos)))
    (when (> x (canvas-xmax dr)) (canvas-xmax-set! dr x))
    (when (> y (canvas-ymax dr)) (canvas-ymax-set! dr y))
    (canvas-chars-set! dr (cons (vector (car pos) (cdr pos) ch) (canvas-chars dr)))))

(define (put-str dr pos str)
  (let loop ((rest (string->list str))
             (i 0))
    (when (pair? rest)
      (put-ch dr (cons (+ (car pos) i) (cdr pos)) (car rest))
      (loop (cdr rest)
            (1+ i)))))

(define (get-lines-vector dr)
  (let ((lines (apply vector (repeat (lambda (i)
                                       (make-bytevector (1+ (canvas-xmax dr))
                                                        (char->integer #\ )))
                                     (1+ (canvas-ymax dr))))))
    (let loop ((chars (canvas-chars dr)))
      (when (pair? chars)
        (let ((x (vector-ref (car chars) 0))
              (y (vector-ref (car chars) 1))
              (ch (vector-ref (car chars) 2)))
          (bytevector-u8-set! (vector-ref lines y) x (char->integer ch))
          (loop (cdr chars)))))
    (vector-map (lambda (i line)
                  (utf8->string line))
                lines)))

(define (get-lines-list dr)
  (vector->list (get-lines-vector dr)))


#;
(begin
  (define dr (open))
  (put-ch dr '(3 . 1) #\a)
  (put-ch dr '(10 . 10) #\b)
  ;;(get-lines dr)
  (for-each pr (get-lines-as-string-list dr)))

