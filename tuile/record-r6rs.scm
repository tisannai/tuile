(define-module (tuile record-r6rs)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module ((rnrs records inspection) #:select (record-type-field-names))
  #:use-module ((rnrs records procedural) #:select (record-accessor))
  #:use-module ((srfi srfi-43) #:select (vector-index))
  ;; Export rnrs style define-record-type.
  #:re-export (define-record-type)
  #:export
  (
   record-type
   record-field-by-symbol
   ))

;; Return record-type as symbol.
(define (record-type rec)
  (record-type-name (record-type-descriptor rec)))

;; Return record-field by symbol name.
(define (record-field-by-symbol rec sym)
  (let* ((fields (record-type-field-names (record-type-descriptor rec)))
         (index (vector-index (lambda (elem) (eq? elem sym)) fields)))
    (if index
        ((record-accessor (record-type-descriptor rec) index) rec)
        #f)))
