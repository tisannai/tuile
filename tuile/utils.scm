(define-module (tuile utils)
  #:export
  (aif
   ))


;; ------------------------------------------------------------
;; Internal functions:


;; ------------------------------------------------------------
;; External functions:


;; Anaphoric macro.
;;
;; (aif (1+ i)
;;   it
;;   #f)
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       (syntax-case (datum->syntax x 'it) ()
         (it
          #'(let ((it test))
              (if it then else))))))))
