(define-module (tuile massoc)
  #:use-module ((srfi srfi-1) #:select (assoc alist-copy))
  #:export
  (
   make-massoc
   massoc-empty?
   massoc-has-key?
   massoc-keys
   massoc-values
   massoc-ref
   massoc-add!
   massoc-val!
   massoc-set!
   massoc-del!
   massoc-update!
   massoc-repeat!
   massoc-copy
   ))


(use-modules (tuile pr))

;; Create an empty massoc (default) or pre-fill it with given entries
;; (alist).
;;
;; NOTE: a null-list is not usable, empty massoc must have empty list
;; as first entry.
(define (make-massoc . rest)
  (if (and (pair? rest)
           (pair? (car rest)))
      (filter (lambda (p) (pair? p)) (car rest))
      (list '())))


;; Return true if massoc is empty.
(define (massoc-empty? ma)
  (null? (car ma)))


;; Return pair if pair with key exists.
(define (massoc-has-key? ma key)
  (if (massoc-empty? ma)
      #f
      (assoc key ma)))


;; Return massoc keys.
(define (massoc-keys ma)
  (if (massoc-empty? ma)
      '()
      (map car (reverse ma))))


;; Return massoc values.
(define (massoc-values ma)
  (if (massoc-empty? ma)
      '()
      (map cdr (reverse ma))))


;; Reference massoc value.
(define (massoc-ref ma key)
  (if (massoc-empty? ma)
      #f
      (assoc-ref ma key)))


;; Add pair (key/val) if not in massoc.
(define (massoc-add! ma key val)
  (cond
   ((massoc-empty? ma) (set-car! ma (cons key val))) ; Empty massoc.
   ((massoc-has-key? ma key) #f)        ; Key exists.
   (else                                ; Add new to non-empty massoc.
    (set-cdr! ma (alist-copy ma))
    (set-car! ma (cons key val))))
  ma)


;; Set value of existing pair in massoc.
(define (massoc-val! ma key val)
  (when (massoc-has-key? ma key)
    (set-cdr! (assoc key ma)
              val)
    ma))


;; Set pair value in massoc. If not in massoc, add it.
(define (massoc-set! ma key val)
  (cond
   ((massoc-empty? ma)       (massoc-add! ma key val))
   ((massoc-has-key? ma key) (massoc-val! ma key val))
   (else                     (massoc-add! ma key val))))


;; Delete pair from massoc.
(define (massoc-del! ma key)
  (let ((tmp (assoc-remove! ma key)))
    (if (null? tmp)
        (begin
          (set-car! ma '())
          (set-cdr! ma '()))
        (begin
          (set-car! ma (car tmp))
          (set-cdr! ma (cdr tmp)))))
  ma)


;; Update pair value in massoc using "proc" with one argument.
(define (massoc-update! ma key proc)
  (when (massoc-has-key? ma key)
    (massoc-val! ma key (proc (massoc-ref ma key))))
  ma)


;; Evaluate "proc" (of three args) for massoc for each pair in set-list.
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
  (if (massoc-empty? ma)
      (make-massoc)
      (alist-copy ma)))
