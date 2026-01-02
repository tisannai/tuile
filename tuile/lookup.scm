(define-module (tuile lookup)
  #:use-module (common base)
  #:use-module ((tuile hash) #:prefix #{hash:}#)
  #:use-module ((tuile utils) #:select (list-split))
;;  #:use-module ((srfi srfi-9) #:select (define-record-type))
;;   #:use-module (tuile compatible)
  #:export
  (
   lookup-make
   alist->lookup
   lookup->alist
   lookup-set!
   lookup-del!
   lookup-ref
   lookup-has-key?
   lookup-each
   lookup-keys
   lookup-values
   lookup-size
   lookup-update!
   lookup-delete-first!
   lookup-delete-last!

   lookup-augment
   lookup-overlay

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

;; Convert alist to lookup.
(define (alist->lookup alist)
  (make-lookup (reverse (map car alist))
               (hash:alist->hash alist)))

;; Convert lookup to alist.
(define (lookup->alist lup)
  (let ((hsh (lookup-hsh lup)))
    (let lp ((keys (lookup-lst lup))
             (ret '()))
      (if (pair? keys)
          (lp (cdr keys)
              (cons (cons (car keys) (hash:hash-ref hsh (car keys))) ret))
          (reverse ret)))))


;; Set value in lookup.
(define (lookup-set! lup key val)
  (unless (hash:hash-contains? (lookup-hsh lup) key)
    (lookup-lst-set! lup (cons key (lookup-lst lup)))
    (hash:hash-set! (lookup-hsh lup) key val)))


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


;; Return lookup size.
(define (lookup-size lup)
  (hash:hash-size (lookup-hsh lup)))


;; Update (or set if missing) value behind the key, with a proc
;; that takes the current value as argument, or #f if missing.
(define (lookup-update! lup key proc)
  (if (lookup-has-key? lup key)
      (lookup-set! lup key (proc (lookup-ref lup key)))
      (lookup-set! lup key (proc #f))))


;; Delete newest value.
(define (lookup-delete-first! lup)
  (let ((size (lookup-size lup)))
    (when (> size 0)
      (cond
       ((= size 1) (let ((item (car (lookup-lst lup))))
                     (hash:hash-remove! (lookup-hsh lup) item)
                     (lookup-lst-set! lup '())))
       (else (let ((parts (list-split (lookup-lst lup) 1)))
               (hash:hash-remove! (lookup-hsh lup) (caar parts))
               (lookup-lst-set! lup (cdr parts))))))))


;; Delete oldest value.
(define (lookup-delete-last! lup)
  (let ((size (lookup-size lup)))
    (when (> size 0)
      (cond
       ((= size 1) (let ((item (car (lookup-lst lup))))
                     (hash:hash-remove! (lookup-hsh lup) item)
                     (lookup-lst-set! lup '())))
       (else (let ((parts (list-split (lookup-lst lup) (- size 1))))
               (hash:hash-remove! (lookup-hsh lup) (cadr parts))
               (lookup-lst-set! lup (car parts))))))))


;; Augment lup with alist, i.e. add all from alist that are missing from lup.
(define (lookup-augment lup alist)
  (let ((hsh (lookup-hsh lup)))
    (let lp ((augment alist))
      (if (pair? augment)
          (begin
            (unless (hash:hash-contains? hsh (caar augment))
              (lookup-lst-set! lup (cons (caar augment) (lookup-lst lup)))
              (hash:hash-set! hsh (caar augment) (cdar augment)))
            (lp (cdr augment)))
          lup))))


;; Overlay lup with alist, i.e. add all from alist and overwrite all existing in lup.
(define (lookup-overlay lup alist)
  (let lp ((overlay alist))
    (if (pair? overlay)
        (begin
          (lookup-set! lup (caar overlay) (cdar overlay))
          (lp (cdr overlay)))
        lup)))
