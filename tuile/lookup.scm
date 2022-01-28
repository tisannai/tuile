(define-module (tuile lookup)
;;  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module (tuile compatible)
  #:export
  (
   lookup-make
   lookup-set!
   lookup-del!
   lookup-ref
   lookup-has-key?
   lookup-each
   lookup-keys
   lookup-values
   ))


;;(define hash-has-key? hash-get-handle)

;; ------------------------------------------------------------
;; Lookup for ordered lookups.

;; Lookup record.
;;
;; Hash table and list of items in order.
(define-record-type lookup
  (fields
   (mutable lst)
   hsh))


;; Make lookup.
(define (lookup-make)
  (make-lookup '() (comp-hash-make)))


;; Set value in lookup.
(define (lookup-set! lup key val)
  (unless (comp-hash-contains? (lookup-hsh lup) key)
    (lookup-lst-set! lup (cons key (lookup-lst lup))))
  (comp-hash-set! (lookup-hsh lup) key val))


;; Delete value from lookup.
(define (lookup-del! lup key)
  (when (comp-hash-contains? (lookup-hsh lup) key)
    (lookup-lst-set! lup (delete key (lookup-lst lup)))
    (comp-hash-remove! (lookup-hsh lup) key)))


;; Reference value in lookup.
(define (lookup-ref lup key)
  (if (comp-hash-contains? (lookup-hsh lup) key)
      (comp-hash-ref (lookup-hsh lup) key)
      #f))


;; Check if lookup includes key.
(define (lookup-has-key? lup key)
  (comp-hash-contains? (lookup-hsh lup) key))


;; Run proc for each lookup entry (in order) with key as argument.
(define (lookup-each lup proc)
  (for-each proc (reverse (lookup-lst lup))))


;; Return list of lookup keys (in order).
(define (lookup-keys lup)
  (reverse (lookup-lst lup)))


;; Return list of lookup values (in order).
(define (lookup-values lup)
  (map (lambda (key)
         (comp-hash-ref (lookup-hsh lup) key))
       (lookup-keys lup)))
