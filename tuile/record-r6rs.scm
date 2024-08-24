(define-module (tuile record-r6rs)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module ((rnrs records inspection) #:select (record-type-field-names))
  #:use-module ((rnrs records procedural) #:select (record-accessor record-mutator))
  #:use-module ((srfi srfi-43) #:select (vector-index))
  ;; Export rnrs style define-record-type.
  #:re-export (define-record-type)
  #:export
  (
   record-type
   record-field-by-symbol
   record-get-by-symbol
   record-set-by-symbol!
   record->list
   record->alist
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

;; Return record-field by symbol name.
(define (record-get-by-symbol rec sym)
  (let* ((fields (record-type-field-names (record-type-descriptor rec)))
         (index (vector-index (lambda (elem) (eq? elem sym)) fields)))
    (if index
        ((record-accessor (record-type-descriptor rec) index) rec)
        *unspecified*)))

;; Return record-field by symbol name.
(define (record-set-by-symbol! rec sym value)
  (let* ((fields (record-type-field-names (record-type-descriptor rec)))
         (index (vector-index (lambda (elem) (eq? elem sym)) fields)))
    (if index
        ((record-mutator (record-type-descriptor rec) index) rec value)
        *unspecified*)))

(define (record->list rec)
  (let* ((rtd (record-type-descriptor rec))
         (len (vector-length (record-type-field-names rtd))))
    (let lp ((i 0)
             (ret '()))
      (if (< i len)
          (lp (1+ i)
              (cons ((record-accessor rtd i) rec) ret))
          (reverse ret)))))

(define (record->alist rec)
  (let* ((rtd (record-type-descriptor rec))
         (field-names (record-type-field-names rtd))
         (len (vector-length field-names)))
    (let lp ((i 0)
             (ret '()))
      (if (< i len)
          (lp (1+ i)
              (cons (cons (vector-ref field-names i)
                          ((record-accessor rtd i) rec))
                    ret))
          (reverse ret)))))

;; (define-record-type foo
;;   (fields (mutable f1)
;;           (mutable f2)))
;;
;; (use-modules (tuile pr))
;; (define f (make-foo 12 24))
;; (pr (record-type f))
;; (pr (record-type-descriptor f))
;; (ppr (record->list f))
