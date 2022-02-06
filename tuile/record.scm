(define-module (tuile record)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
  #:export
  (
   define-im-record
   define-fp-record
   define-mu-record
   ))

;; Create immutable record.
;;
;; Expand this:
;;   (define-im-record foo bar hii)
;;
;; To this:
;;   (define-record-type <foo>
;;     (make-foo bar hii)
;;     foo?
;;     (bar   foo-bar)
;;     (hii   foo-hii)
;;     )
;;
(define-syntax define-im-record
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(define-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">")))
          (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx)))))
           #,@(map (lambda (i) (datum->syntax x i)) (cddr stx)))
          #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?")))
          #,@(map
              (lambda (i)
                (list
                 (datum->syntax x i)
                 (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "-" (symbol->string i))))))
              (cddr stx))))))


;; Create record for functional programming.
;;
;; The setters will modify the record to a record copy with the
;; requested change. This supports functional programming.
;;
;; Expand this:
;;   (define-fp-record foo bar hii)
;;
;; To this:
;;   (define-immutable-record-type <foo>
;;     (make-foo bar hii)
;;     foo?
;;     (bar   foo-bar set-foo-bar)
;;     (hii   foo-hii set-foo-hii)
;;     )
;;
(define-syntax define-fp-record
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(define-immutable-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">")))
          (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx)))))
           #,@(map (lambda (i) (datum->syntax x i)) (cddr stx)))
          #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?")))
          #,@(map
              (lambda (i)
                (list
                 (datum->syntax x i)
                 (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i))))
                 (datum->syntax x (string->symbol (string-append "set-"
                                                                 (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i))))))
              (cddr stx))))))


;; Create mutable record.
;;
;; Expand this:
;;   (define-mu-record foo bar hii)
;;
;; To this:
;;   (define-record-type <foo>
;;     (make-foo bar hii)
;;     foo?
;;     (bar   foo-bar set-foo-bar!)
;;     (hii   foo-hii set-foo-hii!)
;;     )
;;
(define-syntax define-mu-record
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(define-record-type #,(datum->syntax x (string->symbol (string-append "<" (symbol->string (cadr stx)) ">")))
          (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx)))))
           #,@(map (lambda (i) (datum->syntax x i)) (cddr stx)))
          #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?")))
          #,@(map
              (lambda (i)
                (list
                 (datum->syntax x i)
                 (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i))))
                 (datum->syntax x (string->symbol (string-append "set-"
                                                                 (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i)
                                                                 "!")))))
              (cddr stx))))))
