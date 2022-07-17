(define-module (tuile interop)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:export (
            comp:eval
            comp:hash-make
            comp:hash-ref
            comp:hash-set!
            comp:hash-contains?
            comp:hash-remove!
            comp:hash-keys
            comp:hash-values
            comp:hash-copy
            comp:substring
            comp:datum->string
            comp:error
            comp:dir-list
            comp:command-line
            comp:sort
            comp:with-output-to-file
            )
  ;; Export rnrs style define-record-type.
  #:re-export (define-record-type)
  )

;; CONTINUE 220512_1643:
;;
;; Add (maybe) only rnrs style records on Guile side and override all
;; conflicts on the chez scheme side in the corresponding interop
;; module.

(define (comp:eval datum)
  (eval datum (interaction-environment)))

;; Make hash table with define key-type: symbol, string.
(define (comp:hash-make key-type)
  (make-hash-table))

(define comp:hash-ref hash-ref)

(define comp:hash-set! hash-set!)

(define (comp:hash-contains? hsh key)
  (pair? (hash-get-handle hsh key)))

(define comp:hash-remove! hash-remove!)

(define (comp:hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))

(define (comp:hash-values hsh)
  (hash-map->list (lambda (k v) v) hsh))

(define (comp:hash-copy hsh)
  (alist->hash-table (list-copy (hash-map->list cons hsh))))

(define comp:substring substring)

(define (comp:datum->string datum)
  (with-output-to-string (lambda ()
                           (write datum))))

(define comp:error error)

(define (comp:dir-list dir)
  (list-tail (scandir dir) 2))

(define (comp:command-line)
  (cdr (command-line)))

(define comp:sort stable-sort)

(define comp:with-output-to-file with-output-to-file)
