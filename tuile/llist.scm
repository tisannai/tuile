;; Access list based assoc lists.

(define-module (tuile llist)
  #:use-module ((tuile utils) #:select (aif))
  #:export
  (
   llist-ref
   llist-ref-path
   ))


(define ep cadr)

(define (llist-ref ll key)
  (ep (assoc key ll)))

(define (llist-ref-path ll key . rest)
  (let ((keys (if (pair? key) key (cons key rest))))
    (let lp ((ll ll)
             (keys keys))
      (cond
       ((null? ll) #f)
       ((null? (cdr keys))
        (ep (assoc (car keys) ll)))
       (else (aif (assoc (car keys) ll)
                  (lp (ep it) (cdr keys))
                  #f))))))

;; (use-modules (tuile pr))
;; (define ll
;;   `(("copro" ((project-type                 "c-executable")
;;               (library-dependencies         ("como" "slider" "plinth"))
;;               (compile-options-for-release  ("-Wall" "-fPIC""-O3"))
;;               (version                      "0.0.1")))))
;;
;; (ppr (llist-ref ll "copro"))
;; (ppr (llist-ref-path ll "copro" 'version))
;; (ppr (llist-ref-path ll (list "copro" 'version)))
