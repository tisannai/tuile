(define-module (tuile hash)
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
  #:re-export
  (
   make-hash-table
   hash-ref
   hash-set!
   hash-remove!
   )
  #:export
  (
   hash-make
;;    hash-ref
;;    hash-set!
   hash-update!
   hash-contains?
;;    hash-remove!
   hash-keys
   hash-values
   hash-copy
   hash-size

   alist->hash
   hash->alist
   ))

;; Make hash table with define key-type: symbol, string.
;;
;; NOTE: key-type is not in active use, currently.
;;
(define (hash-make . key-type)
  (make-hash-table))

;; (define hash-ref hash-ref)

;; (define hash-set! hash-set!)

;; Update value of key with proc, original value passed to proc.
(define (hash-update! hsh key proc)
  (let* ((org (hash-ref hsh key))
         (new (proc org)))
    (hash-set! hsh key new)
    hsh))

;; (define hash-remove! hash-remove!)

(define (hash-contains? hsh key)
  (pair? (hash-get-handle hsh key)))

(define (hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))

(define (hash-values hsh)
  (hash-map->list (lambda (k v) v) hsh))

(define (hash-copy hsh)
  (alist->hash-table (list-copy (hash-map->list cons hsh))))

(define (hash-size hsh)
  (hash-count (const #t) hsh))

(define (alist->hash alist)
  (alist->hash-table alist))

(define (hash->alist hsh)
  (hash-map->list cons hsh))
