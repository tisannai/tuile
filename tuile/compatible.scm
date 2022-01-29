(define-module (tuile compatible)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:export (
            comp-eval
            comp-hash-make
            comp-hash-ref
            comp-hash-set!
            comp-hash-contains?
            comp-hash-remove!
            comp-hash-keys
            comp-hash-values
            comp-substring
            comp-datum->string
            comp-error
            )
  ;; Export rnrs style define-record-type.
  #:re-export (define-record-type)
  )

(define (comp-eval datum)
  (eval datum (interaction-environment)))

;; Make hash table with define key-type: symbol, string.
(define (comp-hash-make key-type)
  (make-hash-table))

(define comp-hash-ref hash-ref)

(define comp-hash-set! hash-set!)

(define (comp-hash-contains? hsh key)
  (pair? (hash-get-handle hsh key)))

(define comp-hash-remove! hash-remove!)

(define (comp-hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))

(define (comp-hash-values hsh)
  (hash-map->list (lambda (k v) v) hsh))

(define comp-substring substring)

(define (comp-datum->string datum)
  (with-output-to-string (lambda ()
                           (write datum))))

(define comp-error error)
