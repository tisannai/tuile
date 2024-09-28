;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2024
;;;; All rights reserved.


;;; module:
;;
;; Hierarchical key-value store based on alist.
;;
;; treelist contains a tree built out of lists. Key itself is a list
;; which leads to the leaf of the tree.
;;
;; Values can be added and removed from the tree. When the last item
;; from a branch is removed, the complete branch is removed, i.e. each
;; branch contains at least one leaf level item any given time. The
;; only exception to this is an empty tree.
;;
;; treelist is an alist hierarchy.
;;
;;     ((a . a)
;;      (b . ((b-a . b-a)
;;            (b-b . b-b))))
;;
(define-module (tuile treelist)
  #:export (
            treelist-make
            treelist-assoc
            treelist-get
            treelist-set!
            treelist-fill!
            treelist-delete!
            treelist-copy
            ))

(use-modules (tuile pr))
(use-modules (tuile utils))


;; Create empty (initial) tree.
(define (treelist-make)
  (list (list)))


;; Return keyed entry as key-value pair, or #f.
(define (treelist-assoc treelist key)
  (if (null? (car treelist))
      #f
      ;; Make treelist look like an alist entry in order to allow simple
      ;; recursion.
      (let lp ((entry (cons '*root* treelist))
               (key key))
        (cond
         ((and (pair? entry) (pair? key))
          (aif (assoc (car key) (cdr entry))
               (lp it (cdr key))
               #f))
         (else entry)))))


;; Return keyed entry as value, or #f.
(define (treelist-get treelist key)
  (let ((value (treelist-assoc treelist key)))
    (if (pair? value)
        (cdr value)
        #f)))


;; Set keyed value, existing or not, and create path to it, if not
;; existing.
(define (treelist-set! treelist key value)

  (define (new-branch key value)
    (cond
     ((null? (cdr key)) (list (cons (car key) value)))
     (else (list (cons (car key) (new-branch (cdr key) value))))))

  (define (recurse treelist key value)
    (let ((branch (assoc (car key) treelist)))
      (cond
       ((not branch)
        ;; New branch.
        (set-cdr! treelist (append (cdr treelist) (new-branch key value))))
       ((null? (cdr key))
        ;; Leaf level overwrite.
        (set-cdr! branch value))
       (else (recurse (cdr branch) (cdr key) value)))))

  (cond
   ((null? (car treelist))
    ;; Empty, initial treelist.
    (set-car! treelist (car (new-branch key value)))
    (set-cdr! treelist '()))
   (else (recurse treelist key value))))


;; Set keyed value only if missing, i.e. no overwrite.
(define (treelist-fill! treelist key value)
  (let ((leaf (treelist-assoc treelist key)))
    (unless leaf
      (treelist-set! treelist key value))))


;; Delete keyed entry and destroy the whole branch if it becomes
;; empty.
(define (treelist-delete! treelist key)
  
  (define (del-proper branch key coll id)
    (cond
     ((not (assoc (car key) branch))
      ;; Key not found.
      #f)
     ((null? (cdr key))
      ;; At leaf.
      (let* ((long (> (length branch) 1))
             (branch (if long branch coll))
             (key (if long key (list id))))
        (cond
         ((equal? (caar branch) (car key))
          ;; First in list.
          (let ((deleted (car branch))
                (tail (cdr branch)))
            (cond
             ((pair? tail)
              ;; Non-empty tree.
              (set-car! branch (car tail))
              (set-cdr! branch (cdr tail)))
             (else
              ;; Empty tree (initial state).
              (set-car! branch (list))))
            deleted))
         (else
          ;; Non-first in list.
          (let lp ((lst (cdr branch))
                   (result '()))
            (if (pair? lst)
                (if (equal? (caar lst) (car key))
                    (let ((delete (car lst)))
                      (set-cdr! branch (append (reverse result) (cdr lst)))
                      delete)
                    (lp (cdr lst)
                        (cons (car lst) result)))
                (reverse result)))))))
     (else
      ;; Recurse.
      (let* ((long (> (length branch) 1))
             (sub (cdr (assoc (car key) branch))))
        (del-proper sub
                    (cdr key)
                    (if long branch coll)
                    (if long (car key) id))))))

  (del-proper treelist key treelist (car key)))


;; Recursively copy the tree.
(define (treelist-copy treelist)
  (if (null? (car treelist))
      (list (list))
      (cdr
       ;; Make treelist look like an alist entry in order to allow simple
       ;; recursion.
       (let lp ((entry (cons '*root* treelist)))
         (cond
          ((and (pair? entry)
                (not (pair? (cdr entry))))
           ;; Leaf.
           (cons (car entry) (cdr entry)))
          (else
           (cons (car entry) (map lp (cdr entry)))))))))


;; (define treelist (treelist-make))
;; (ppre (treelist-get treelist '(a b)))

(when #f
  (let ()

    (define treelist #f)

    (set! treelist (treelist-make))
    (treelist-set! treelist '(a) 'a)
    (ppr treelist)
    (treelist-set! treelist '(b) 'b)
    (ppr treelist)

    (set! treelist (treelist-make))
    (treelist-set! treelist '(a) 'a)
    (ppr treelist)
    (ppr (treelist-get treelist '(a)))
    (treelist-set! treelist '(b b-a) 'b-a)
    (ppr treelist)
    (ppr (treelist-get treelist '(b b-a)))
    (treelist-set! treelist '(b b-b) 'b-b)
    (ppr treelist)
    (treelist-set! treelist '(c c-a c-a-a) 'c-a-a)
    (ppr treelist)
    (treelist-set! treelist '(c c-b) 'c-b)
    (ppr treelist)
    (treelist-set! treelist '(c c-c c-c-a) 'c-c-a)
    (ppr treelist)
    (treelist-fill! treelist '(c c-c c-c-a) 'c-c-a-2)
    (ppr treelist)
    (treelist-fill! treelist '(c c-c c-c-b) 'c-c-b)
    (ppr treelist)
    ;; (ppr (treelist-get treelist '(c c-c c-c-a)))
    ;; (ppr (treelist-get treelist '(c c-c c-c-b)))
    (treelist-delete! treelist '(c c-c c-c-a))
    ;; (treelist-delete! treelist '(c c-c c-c-b))
    (ppr treelist)

    (set! treelist (treelist-make))
    (treelist-set! treelist '(a) 'a)
    (ppr treelist)
    (treelist-delete! treelist '(a))
    (ppr treelist)
    (treelist-set! treelist '(a) 'a)
    (ppr treelist)
    (treelist-delete! treelist '(a))
    (ppr treelist)
    (treelist-set! treelist '(c c-c c-c-a) 'c-c-a)
    (ppr treelist)
    (treelist-delete! treelist '(c c-c c-c-a))
    (ppr treelist)
    (treelist-set! treelist '(a) 'a)
    (treelist-set! treelist '(c c-c c-c-a) 'c-c-a)
    (ppr treelist)
    (treelist-delete! treelist '(c c-c c-c-a))
    (ppr treelist)

    (define tl '((a . a) (b . ((b-a . b-a) (b-b . b-b)))))
    (ppr tl)
    (define tc (treelist-copy tl))
    (ppr tc)
    (treelist-delete! tc '(b))
    (ppr tc)
    ))
