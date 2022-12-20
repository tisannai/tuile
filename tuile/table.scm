(define-module (tuile table)
  #:use-module ((srfi srfi-1)  #:select (first second third fold drop take))
  #:use-module (tuile compatible)
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module (tuile re)
  #:export
  (
   table-geometry
   table-style
   table->cells
   table-render-cells
   table-render
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
        (begin
          (let loop-columns ((columns (car table))
                             (x 0))
            (when (pair? columns)
              (when (> (string-length (car columns))
                       (vector-ref-if max-x x))
                (vector-set! max-x x (string-length (car columns))))
              (loop-columns (cdr columns)
                            (1+ x))))
          (loop-lines (cdr table)
                      max-x))
        (vector->list max-x))))


;;(define table (list (list "aaa" "bb")
;;                    (list "c" "dd" "eeee")))
;;(table-column-geometry test-table)

(define (table-style style)
  (case style
    ((default) (list (cons 'indent 0)   ; Left indent of complete table.
                     (cons 'align 'left) ; Text alignment within cells.
                     (cons 'lgap 1)     ; Left gap for cell line.
                     (cons 'rgap 1)     ; Right gap for cell line.
                     (cons 'tgap 0)     ; Top gap for cell line.
                     (cons 'bgap 0)     ; Bottom gap for cell line.
                     (cons 'vsep #\|)   ; Vertical separator (runs vertically).
                     (cons 'hsep #\-)   ; Horizontal separator (runs horizontally).
                     (cons 'csep #\+))) ; Corner separator (cell corner marker).
    (else (table-style 'default))))


(define (table->cells table column-geometry)

  ;; Check if column fits in the geometry. If it fits, then create a
  ;; list of one cell-line. If it doesn't, split the column by space
  ;; separators. If the individual words fit to geometry, fill as many
  ;; words as possible to each cell-line, and create a list of
  ;; cell-lines. Otherwise simply use the geometry value to split the
  ;; column to full length strings (ignoring word boundaries) to list
  ;; of cell-lines.
  ;;
  ;;      123456789012345
  ;;     "foobar was here"
  ;;
  ;;      geometry = 20    ->     "foobar was here"
  ;;      geometry = 12    ->     "foobar was"
  ;;                              "here"
  ;;      geometry = 4     ->     "foob"
  ;;                              "ar w"
  ;;                              "as h"
  ;;                              "ere"
  ;;
  (define (format-column-to-cell-lines column geometry)
    (if (> (string-length column) geometry)
        (let* ((words (re-split "[ \t\n]+" column))
               (word-wrap (>= geometry (apply max (map string-length words)))))
          (if word-wrap
              (let loop-words ((words words)
                               (line '())
                               (count 0)
                               (ret '()))
                (if (pair? words)
                    (if (> (+ count (string-length (car words)) 1)
                           geometry)
                        (loop-words words
                                    '()
                                    0
                                    (cons (string-join (reverse line) " ")
                                          ret))
                        (loop-words (cdr words)
                                    (cons (car words) line)
                                    (+ count (string-length (car words)) (if (null? line) 0 1))
                                    ret))
                    (if (null? line)
                        (reverse ret)
                        (reverse (cons (string-join (reverse line) " ")
                                       ret)))))
              (let loop-geometry ((chars (string->list column))
                                  (ret '()))
                (if (> (length chars) geometry)
                    (loop-geometry (drop chars geometry)
                                   (cons (list->string (take chars geometry))
                                         ret))
                    (if (= (length chars) 0)
                        (reverse ret)
                        (reverse (cons (list->string chars)
                                       ret)))))))
        (list column)))

  (let loop-table ((table table)
                   (ret-lines '()))
    (if (pair? table)
        (loop-table (cdr table)
                    (cons (let loop-columns ((columns (car table))
                                             (geometry column-geometry)
                                             (ret-cell-lines '()))
                            (if (pair? geometry)
                                (if (null? columns)
                                    (loop-columns '()
                                                  (cdr geometry)
                                                  (cons (list "") ret-cell-lines))
                                    (loop-columns (cdr columns)
                                                  (cdr geometry)
                                                  (cons (format-column-to-cell-lines (car columns)
                                                                                     (car geometry))
                                                        ret-cell-lines)))
                                (reverse ret-cell-lines)))
                          ret-lines))
        (reverse ret-lines))))

;;(define table (list (list "foobar was here" "foobar was here")
;;                    (list "foobar wasn't here")))
;;(define column-geometry (list 12 4))

(define (table-render-cells cells column-geometry style)

  ;; Helper functions for style attributes.
  (define (indent) (assoc-ref style 'indent))
  (define (align)  (assoc-ref style 'align))
  (define (lgap)   (assoc-ref style 'lgap))
  (define (rgap)   (assoc-ref style 'rgap))
  (define (tgap)   (assoc-ref style 'tgap))
  (define (bgap)   (assoc-ref style 'bgap))
  (define (vsep)   (assoc-ref style 'vsep))
  (define (hsep)   (assoc-ref style 'hsep))
  (define (csep)   (assoc-ref style 'csep))

  ;;  ;; Return nth item from list, otherwise #f.
  ;;  (define (nth lst n)
  ;;    (if (>= n (length lst))
  ;;        #f
  ;;        (list-ref lst n)))


  ;; Format one cell-line.
  (define (format-cell-line cell-line geometry aligment)
    (let* ((len (string-length cell-line))
           (pad (- geometry len)))
      (if (> pad 0)
          (case aligment
            ((left) (string-append cell-line (make-string pad #\ )))
            ((right) (string-append (make-string pad #\ ) cell-line))
            ((center) (let* ((lpad (quotient pad 2))
                             (rpad (- pad lpad)))
                        (string-append (make-string lpad #\ )
                                       cell-line
                                       (make-string rpad #\ )))))
          cell-line)))

  ;; Create horizontal separator string.
  ;;
  ;;     +----+---+-------+
  ;;
  (define (format-horizontal-separator column-geometry indent lgap rgap hsep csep)
    ;;    (pde column-geometry
    ;;         indent
    ;;         lgap
    ;;         rgap
    ;;         hsep
    ;;         csep)
    (let loop ((geometry column-geometry)
               (ret (list (string-append (make-string indent #\ )
                                         (string csep)))))
      (if (pair? geometry)
          (loop (cdr geometry)
                (cons (string-append (make-string (+ lgap (car geometry) rgap) hsep)
                                     (string csep))
                      ret))
          (string-concatenate (reverse ret)))))


  ;; Create one text line for table.
  ;;
  ;;     | foo  | bar  | diiduu   |
  ;;
  (define (format-line cell-line-slice column-geometry indent lgap rgap vsep align)
    (let loop ((slice cell-line-slice)
               (geometry column-geometry)
               (align (if (list? align)
                          align
                          (make-list (length cell-line-slice) align)))
               (ret (list (string-append (make-string indent #\ )
                                         (string vsep)))))
      (if (pair? slice)
          (loop (cdr slice)
                (cdr geometry)
                (cdr align)
                (cons (string-append (make-string lgap #\ )
                                     (format-cell-line (car slice)
                                                       (car geometry)
                                                       (car align))
                                     (make-string rgap #\ )
                                     (string vsep))
                      ret))
          (string-concatenate (reverse ret)))))

  ;; Create empty cell line for tgap and bgap purposes.
  (define (format-filler-lines column-geometry indent lgap rgap vsep vgap)
    (if (= vgap 0)
        (list)
        (make-list vgap
                   (format-line (make-list (length column-geometry) "")
                                column-geometry
                                indent
                                lgap
                                rgap
                                vsep
                                'left))))


  (let ((horizontal-separator (format-horizontal-separator column-geometry
                                                           (indent)
                                                           (lgap)
                                                           (rgap)
                                                           (hsep)
                                                           (csep)))
        (top-filler (format-filler-lines column-geometry
                                         (indent)
                                         (lgap)
                                         (rgap)
                                         (vsep)
                                         (tgap)))
        (bottom-filler (format-filler-lines column-geometry
                                            (indent)
                                            (lgap)
                                            (rgap)
                                            (vsep)
                                            (bgap))))
    (let loop-cell-rows ((cell-rows cells)
                         (ret (list horizontal-separator)))
      (if (pair? cell-rows)
          (loop-cell-rows (cdr cell-rows)
                          (append (let ((cell-height (apply max (map length (car cell-rows)))))
                                    (let loop-cell-line ((cell-columns (car cell-rows))
                                                         (line-index 0)
                                                         (ret top-filler))
                                      (if (< line-index cell-height)
                                          (loop-cell-line cell-columns
                                                          (1+ line-index)
                                                          (cons (format-line (map (lambda (cell-lines)
                                                                                    (if (>= line-index (length cell-lines))
                                                                                        ""
                                                                                        (list-ref cell-lines line-index)))
                                                                                  cell-columns)
                                                                             column-geometry
                                                                             (indent)
                                                                             (lgap)
                                                                             (rgap)
                                                                             (vsep)
                                                                             (align))
                                                                ret))
                                          (append (list horizontal-separator) bottom-filler ret))))
                                  ret))
          (string-concatenate (map (lambda (line) (string-append line "\n"))
                                   (reverse ret)))))))



(define* (table-render table
                       #:key
                       (set-style #f)
                       (set-style-name #f)
                       (set-style-options '())
                       (create-aligns #f)
                       (set-geometry #f)
                       (create-geometry #f))
  (let* ((column-geometry (cond
                           (set-geometry set-geometry)
                           (create-geometry (create-geometry (table-column-geometry table)))
                           (else (table-column-geometry table))))
         (selected-style (cond
                          (set-style set-style)
                          (set-style-name (table-style set-style-name))
                          (else (table-style 'default))))
         (modified-style (if (null? set-style-options)
                             selected-style
                             (let loop ((updates set-style-options)
                                        (style selected-style))
                               (if (pair? updates)
                                   (loop (cdr updates)
                                         (assoc-set! style
                                                     (caar updates)
                                                     (cdar updates)))
                                   style))))
         (alignments (assoc-ref modified-style 'align))
         (modified-alignments (if create-aligns
                                  (create-aligns alignments column-geometry)
                                  alignments))
         (final-style (assoc-set! modified-style 'align modified-alignments))
         (cells (table->cells table column-geometry)))
    (table-render-cells cells
                        column-geometry
                        final-style)))


#;
(define (test-basics)

  (use-modules (tuile pr))

  (define table (list (list "foobar was here" "foobar was here")
                      (list "foobar wasn't here")
                      (list "foobar was here" "foobar was")))
  ;;(pr (table-geometry table))
  ;;(pd (table-column-geometry table))

  (define column-geometry (list 12 4))

  (define cells (table->cells table
                            column-geometry))
  ;;(pp cells)

  ;;(pr (table-render-cells cells
  ;;                        column-geometry
  ;;                        (table-style 'default)))

  (pr (table-render-cells cells
                          column-geometry
                          (assoc-set! (table-style 'default)
                                      'indent
                                      4))))


#;
(define (test-renderer)

  (use-modules (tuile pr))

  (define table (list (list "foobar was here" "foobar was here")
                      (list "foobar wasn't here")
                      (list "foobar was here" "foobar was")))

  (pr (table-render table
                    #:create-geometry (lambda (g) (list 12 4))
                    #:create-aligns (lambda (a g) (make-list (length g) 'left))
                    #:set-style-options (list (cons 'tgap 2)
                                              (cons 'bgap 1)))))

;;(test-basics)
;;(test-renderer)
