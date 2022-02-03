(define-module (tuile chez)
  #:use-module (ice-9 exceptions)
  #:export
  (
   assert
   ))

;; Assert truthness of expr.
(define (assert expr)
  (if expr
      expr
      (raise-exception (make-non-continuable-error))))
