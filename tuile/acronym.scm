(define-module (tuile acronym)
  #:use-module ((tuile utils) #:select (assoc-has-key? assoc-update! assoc-repeat!))
  #:use-module ((srfi srfi-1) #:select (drop-right last concatenate))
  #:use-module (srfi srfi-43)
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
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
   a->hash
   a-len

   s-new
   s->l
   s->n
   s=
   s.
   s+
   s&
   s?
   s=?
   s-len
   s-join

   l-new
   l->s
   l->v
   l=
   l.
   l+
   l&
   l?
   l-len
   l-any?
   l-empty?
   l-rm
   l-rm!
   l-tail
   l-head
   l-drop
   l-last
   l-copy

   n->s
   n?
   nie
   nei

   v-new
   v->l
   v=
   v.
   v+
   v&
   v?
   v-len
   v-any?
   v-empty?
   v->l
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
(define a->hash alist->hash-table)
(define a-len length)


(define s-new make-string)
(define s->l string->list)
(define s->n string->number)
(define s= string-set!)
(define s. substring)
(define s+ string-append)
(define s& string-concatenate)
(define s? string?)
(define s=? string=?)
(define s-len string-length)
(define s-join string-join)


(define l-new list)
(define l->s list->string)
(define l->v list->vector)
(define l= list-set!)
(define l. list-ref)
(define l+ append)
(define l& concatenate)
(define l? list?)
(define l-len length)
(define l-any? pair?)
(define l-empty? null?)
(define l-rm delete)
(define l-rm! delete!)
(define l-tail list-tail)
(define l-head list-head)
(define l-drop drop-right)
(define l-last last)
(define l-copy list-copy)


(define n->s number->string)
(define n? number?)
(define nie inexact->exact)
(define nei exact->inexact)


(define v-new vector)
(define v->l vector->list)
(define v= vector-set!)
(define v. vector-ref)
(define v+ vector-append)
(define v& vector-concatenate)
(define v? vector?)
(define v-len vector-length)
(define v-any? (lambda (v) (not (vector-empty? v))))
(define v-empty? vector-empty?)
(define v->l vector->list)
