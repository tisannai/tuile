(define-module (tuile table)
  #:use-module ((srfi srfi-1)  #:select (first second third fold drop))
  #:use-module (tuile compatible)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module (tuile re)
  #:export
  (
   ))


;; Return (x . y) dimensions of the table.
(define (table-geometry table)
  (let loop-lines ((table table)
                   (max-x 0)
                   (y 0))
    (if (pair? table)
        (let ((x (let loop-columns ((columns (car table))
                                 (x 0))
                   (if (pair? columns)
                       (loop-columns (cdr columns)
                                  (1+ x))
                       x))))
          (loop-lines (cdr table)
                      (max x max-x)
                      (1+ y)))
        (cons max-x y))))


;; Return max-width for each column.
(define (table-column-geometry table)
  (define (vector-ref-if vec idx)
    (if (>= idx (vector-length vec))
        0
        (vector-ref vec idx)))
  (let loop-lines ((table table)
                   (max-x (make-vector (car (table-geometry table)) 0)))
    (if (pair? table)
        (let ((x (let loop-columns ((columns (car table))
                                 (x 0))
                   (if (pair? columns)
                       (begin 
                         (when (> (string-length (car columns))
                                  (vector-ref-if max-x x))
                           (vector-set! max-x x (string-length (car columns))))
                         (loop-columns (cdr columns)
                                    (1+ x)))
                       x))))
          (loop-lines (cdr table)
                      max-x))
        (vector->list max-x))))


;;(define table (list (list "aaa" "bb")
;;                    (list "c" "dd" "eeee")))
;;(table-column-geometry test-table)

(define (table-format-params style)
  (case style
    ((default) (list (cons 'indent 4)
                     (cons 'align 'left)
                     (cons 'lgap 1)
                     (cons 'rgap 1)
                     (cons 'vsep #\|)
                     (cons 'hsep #\-)
                     (cons 'csep #\+)))
    (else (list (cons 'indent 4)
                (cons 'align 'left)
                (cons 'lgap 1)
                (cons 'rgap 1)
                (cons 'vsep #\|)
                (cons 'hsep #\-)
                (cons 'csep #\+)))))


(define (table-as-cell-lines table column-geometry)

  ;; Check if column fits in the geometry. If it fits, then create a
  ;; list of one cell-line. If it doesn't, split the column by space
  ;; separators. If the indifidual words fit to geometry, fill as many
  ;; words as possible to each cell-line, and create a list of
  ;; cell-lines. Otherwise simply use the geometry value to split the
  ;; column to full length strings (ignoring word boundaries) to list
  ;; of cell-lines.
  (define (format-column-to-cell-lines column geometry)
    (if (> (string-length column) geometry)
        (let* ((words (re-split "[ \t\n]+" column))
               (word-wrap (>= geometry (apply max (map string-length words)))))
          (if word-wrap
              )
          )
        (list column)))


  (let loop-lines ((table table)
                   (ret-lines '()))
    (if (pair? table)
        (let ((x (let loop-columns ((columns (car table))
                                    (geometry column-geometry)
                                    (ret-columns '()))
                   (if (pair? columns)
                       (if (> (string-length (car columns))
                              (car geometry))
                           (loop-columns (cdr columns)
                                         (cdr geometry)
                                         (cons ))
                           )

                       (begin 
                         (when (> (string-length (car columns))
                                  (vector-ref-if max-x x))
                           (vector-set! max-x x (string-length (car columns))))
                         (loop-columns (cdr columns)
                                    (1+ x)))
                       x))))
          (loop-lines (cdr table)
                      max-x))
        (vector->list max-x))))
