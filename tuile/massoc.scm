(define-module (tuile massoc)
  #:use-module ((srfi srfi-1) #:select (assoc alist-copy))
  #:export
  (
   make-massoc
   massoc-has-key?
   massoc-ref
   massoc-add!
   massoc-val!
   massoc-set!
   massoc-update!
   massoc-repeat!
   massoc-copy
   ))



;; Create an empty massoc, which used to add entries.
;;
;; NOTE: a null-list is not usable, empty massoc must have empty list
;; as first entry.
(define (make-massoc)
  (list '()))


;; Return pair if pair with key exists.
(define (massoc-has-key? ma key)
  (assoc key ma))


;; Reference massoc value
(define massoc-ref assoc-ref)


;; Add pair (key/val) if not in massoc.
(define (massoc-add! ma key val)
  (cond
   ((null? (car ma)) (set-car! ma (cons key val))) ; Empty massoc.
   ((massoc-has-key? ma key) #f)        ; Key exists.
   (else                                ; Add new to non-empty massoc.
    (set-cdr! ma (alist-copy ma))
    (set-car! ma (cons key val))))
  ma)


;; Set pair value if not in massoc.
(define (massoc-val! ma key val)
  (when (massoc-has-key? ma key)
    (set-cdr! (assoc key ma)
              val)
    ma))


;; Set pair value in massoc. If not in massoc, add it.
(define (massoc-set! ma key val)
  (cond
   ((null? (car ma))         (massoc-add! ma key val))
   ((massoc-has-key? ma key) (massoc-val! ma key val))
   (else                     (massoc-add! ma key val))))


;; Update pair value in massoc using "proc".
(define (massoc-update! ma key proc)
  (when (massoc-has-key? ma key)
    (massoc-val! ma key (proc (massoc-ref ma key))))
  ma)


;; Evaluate "proc" for massoc for each pair in set-list.
(define (massoc-repeat! ma proc set-list)
  (if (pair? set-list)
      (massoc-repeat! (proc ma
                            (caar set-list)
                            (cdar set-list))
                      proc
                      (cdr set-list))
      ma))


;; Return copy of massoc.
(define (massoc-copy ma)
  (alist-copy ma))
