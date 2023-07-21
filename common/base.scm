;; Base definitions for all used schemes environments.

(cond-expand

 (guile-3
  (define-module (common base)
    #:use-module ((rnrs records syntactic) #:select (define-record-type))
    #:re-export (define-record-type)))

 )
