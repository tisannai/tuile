(define-module (tuile hash)
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
  #:export
  (
   hash-make
   hash-ref
   hash-set!
   hash-contains?
   hash-remove!
   hash-keys
   hash-values
   hash-copy
   hash-size
   ))

;; Make hash table with define key-type: symbol, string.
;;
;; NOTE: key-type is not in active use, currently.
;;
(define (hash-make . key-type)
  (make-hash-table))

(define hash-ref hash-ref)

(define hash-set! hash-set!)

(define (hash-contains? hsh key)
  (pair? (hash-get-handle hsh key)))

(define hash-remove! hash-remove!)

(define (hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))

(define (hash-values hsh)
  (hash-map->list (lambda (k v) v) hsh))

(define (hash-copy hsh)
  (alist->hash-table (list-copy (hash-map->list cons hsh))))

(define (hash-size hsh)
  (hash-count (const #t) hsh))
