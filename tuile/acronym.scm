(define-module (tuile acronym)
  #:use-module ((tuile utils) #:select (assoc-has-key? assoc-update! assoc-repeat!))
  #:use-module ((srfi srfi-1) #:select (drop-right last))
  #:export
  (
   h-new
   h=
   h.
   h-rm!
   h-each
   h-key?
   h-keys
   h-values
   h-entries
   h-count
   a=
   a.
   a-key?
   a-update!
   a-repeat!

   s-new
   s=
   s.
   s=?
   s->l
   s-append
   s-concat
   s-join

   l-new
   l->s
   l=
   l.
   l-rm
   l-rm!
   l-append
   l-tail
   l-head
   l-drop
   l-last
   ))


(define h-new make-hash-table)
(define h= hash-set!)
(define h. hash-ref)
(define h-rm! hash-remove!)
(define h-each hash-for-each)
(define h-key? hash-get-handle)
(define h-keys (lambda (h) (hash-map->list (lambda (k v) k) h)))
(define h-values (lambda (h) (hash-map->list (lambda (k v) v) h)))
(define h-entries (lambda (h) (hash-map->list (lambda (k v) (cons k v)) h)))
(define h-count hash-count)


(define a= assoc-set!)
(define a. assoc-ref)
(define a-key? assoc-has-key?)
(define a-update! assoc-update!)
(define a-repeat! assoc-repeat!)


(define s-new make-string)
(define s= string-set!)
(define s. substring)
(define s=? string=?)
(define s->l string->list)
(define s-append string-append)
(define s-concat string-concatenate)
(define s-join string-join)


(define l-new list)
(define l->s list->string)
(define l= list-set!)
(define l. list-ref)
(define l-rm delete)
(define l-rm! delete!)
(define l-append append)
(define l-tail list-tail)
(define l-head list-head)
(define l-drop drop-right)
(define l-last last)
