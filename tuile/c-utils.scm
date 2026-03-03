(define-module (tuile c-utils)
  #:use-module ((tuile edit) #:prefix #{eu:}#)
  #:export
  (
   get-function-prototypes
   ))


(define (get-function-prototypes filename static)
  ;;                  int           *         ilog2              (  int  )
  ;;                  \\             \     \   \                  \  \    \   \
  (let* ((re-fn-start "^([a-zA-Z0-9_]+[*]*)([ ]+[a-zA-Z0-9_]+[*]*)+\\(.*(,|\\))$")
         (re-fn-end   "\\)$")
         (fn-start? (lambda (eu)
                      ((if static identity not) (eu:match-re? eu "^static"))))
         (eu (eu:read filename)))
    (let loop-lines ((protos '()))
      (if (eu:search-re eu re-fn-start)
          (if (fn-start? eu)
              (loop-lines (append protos
                                  (let loop-proto ((lines '()))
                                    (let ((cur-line (eu:get eu)))
                                      (if (eu:match-re? eu re-fn-end)
                                          (begin
                                            (eu:step eu)
                                            (append lines (list (string-append cur-line ";"))))
                                          (begin
                                            (eu:step eu)
                                            (loop-proto (append lines
                                                                (list cur-line)))))))))
              (begin
                (eu:step eu)
                (loop-lines protos)))
          protos))))
