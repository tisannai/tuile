(define-module (tuile lookup)
  #:use-module (common base)
  #:use-module ((tuile hash) #:prefix hash:)
;;  #:use-module ((srfi srfi-9) #:select (define-record-type))
;;   #:use-module (tuile compatible)
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
  (fields (mutable lst)
          hsh))


;; Make lookup.
(define (lookup-make)
  (make-lookup '() (hash:hash-make)))


;; Set value in lookup.
(define (lookup-set! lup key val)
  (unless (hash:hash-contains? (lookup-hsh lup) key)
    (lookup-lst-set! lup (cons key (lookup-lst lup))))
  (hash:hash-set! (lookup-hsh lup) key val))


;; Delete value from lookup.
(define (lookup-del! lup key)
  (when (hash:hash-contains? (lookup-hsh lup) key)
    (lookup-lst-set! lup (delete key (lookup-lst lup)))
    (hash:hash-remove! (lookup-hsh lup) key)))


;; Reference value in lookup.
(define (lookup-ref lup key)
  (if (hash:hash-contains? (lookup-hsh lup) key)
      (hash:hash-ref (lookup-hsh lup) key)
      #f))


;; Check if lookup includes key.
(define (lookup-has-key? lup key)
  (hash:hash-contains? (lookup-hsh lup) key))


;; Run proc for each lookup entry (in order) with key as argument.
(define (lookup-each lup proc)
  (for-each proc (reverse (lookup-lst lup))))


;; Return list of lookup keys (in order).
(define (lookup-keys lup)
  (reverse (lookup-lst lup)))


;; Return list of lookup values (in order).
(define (lookup-values lup)
  (map (lambda (key)
         (hash:hash-ref (lookup-hsh lup) key))
       (lookup-keys lup)))
