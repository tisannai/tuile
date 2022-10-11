(define-module (tuile module)
  #:export
  (
   module-forward
   ))

;; Collect the list of "modules" and forward (re-export) all
;; symbols. In other word, create a collection type module from the
;; "modules".
;;
;; Example:
;;
;;     (module-forward '(tuile utils)
;;                     '(tuile pr))
;;
(define (module-forward . modules)
  (for-each (lambda (module)
              (module-use! (current-module)
                           (resolve-interface module))
              (module-re-export! (current-module)
                                 (hash-map->list (lambda (a b) a)
                                                 (module-obarray (resolve-interface module)))))
            modules))
