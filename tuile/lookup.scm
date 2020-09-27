(define-module (tuile lookup)
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:export
  (
   make-lookup
   lookup-set!
   lookup-del!
   lookup-ref
   lookup-has-key?
   lookup-each
   lookup-keys
   ))


(define hash-has-key? hash-get-handle)

;; ------------------------------------------------------------
;; Lookup for ordered lookups.

;; Lookup record.
;;
;; Hash table and list of items in order.
(define-record-type <lookup>
  (new-lookup lst hsh)
  lookup?
  (lst     lookup-lst set-lookup-lst!)
  (hsh     lookup-hsh))


;; Make lookup.
(define (make-lookup)
  (new-lookup '() (make-hash-table)))


;; Set value in lookup.
(define (lookup-set! lup key val)
  (unless (hash-has-key? (lookup-hsh lup) key)
    (set-lookup-lst! lup (cons key (lookup-lst lup))))
  (hash-set! (lookup-hsh lup) key val))


;; Delete value from lookup.
(define (lookup-del! lup key)
  (when (hash-has-key? (lookup-hsh lup) key)
    (set-lookup-lst! lup (delete key (lookup-lst lup)))
    (hash-remove! (lookup-hsh lup) key)))


;; Reference value in lookup.
(define (lookup-ref lup key)
  (if (hash-has-key? (lookup-hsh lup) key)
      (hash-ref (lookup-hsh lup) key)
      #f))


;; Check if lookup includes key.
(define (lookup-has-key? lup key)
  (hash-has-key? (lookup-hsh lup) key))


;; Run proc for each lookup entry (in order) with key as argument.
(define (lookup-each lup proc)
  (for-each proc (reverse (lookup-lst lup))))


;; Return list of lookup keys (in order).
(define (lookup-keys lup)
  (reverse (lookup-lst lup)))
