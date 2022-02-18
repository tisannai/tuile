(define-module (tuile record-r6rs)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  ;; Export rnrs style define-record-type.
  #:re-export (define-record-type)
  )
