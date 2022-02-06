(define-module (tuile oop)
  #:use-module (srfi srfi-88)
  #:use-module ((srfi srfi-1) #:select (drop-right))
  #:use-module (oop goops)
  #:use-module (tuile re)
  #:export
  (
   define-this-class
   define-this-method
   this-ref
   this-set!
   ))


;; Define class with members.
;;
;; Members have initial values and keyword initializers.
;;
;;     (define-this-class <my-class> (<string>)
;;       mem1
;;       (mem2 2))
;;
;; Becomes:
;;
;;      (define-class <my-class> (<string>)
;;        (mem1 #:init-keyword #:mem1 #:init-form #f)
;;        (mem2 #:init-keyword #:mem2 #:init-form 2))
;;
(define-syntax define-this-class
  (lambda (x)
    (let* ((stx     (syntax->datum x))
           (klass   (cadr stx))
           (bases   (caddr stx))
           (memdefs (cdddr stx))
           (->syn   datum->syntax))
      #`(define-class #,(->syn x klass) #,(->syn x bases)
          #,@(map (lambda (memdef)
                    (->syn x (let ((mem-name (if (pair? memdef)
                                                 (car memdef)
                                                 memdef))
                                   (val (if (pair? memdef)
                                            (cadr memdef)
                                            #f)))
                               (list mem-name
                                     #:init-form
                                     val
                                     #:init-keyword
                                     (string->keyword (symbol->string mem-name))))))
                  memdefs)))))


;; Define method in a compact form.
;;
;;     (define-this-method <my-class> (my-method a1 a2 . rest)
;;       body ...)
;;
;; Becomes:
;;
;;      (define-method (my-method (this <my-class>) a1 a2 . rest)
;;        body ...)
;;
(define-syntax define-this-method
  (lambda (x)
    (let* ((stx   (syntax->datum x))
           (klass (cadr stx))
           (metod (caaddr stx))
           (args  (cdaddr stx))
           (body  (cdddr stx))
           (->syn datum->syntax))
      #`(define-method #,(if (pair? args)
                             ;; Has multiple arguments.
                             (if (cdr (last-pair args))
                                 ;; Has ". rest".
                                 (append (list (->syn x metod)
                                               (list (->syn x 'this)
                                                     (->syn x klass)))
                                         (map (lambda (i)
                                                (->syn x i))
                                              (drop-right args 1))
                                         (->syn x (last-pair args)))
                                 ;; Fixed list of args.
                                 (append (list (->syn x metod)
                                               (list (->syn x 'this)
                                                     (->syn x klass)))
                                         (->syn x args)))
                             ;; No arguments (or only ". rest").
                             (if (null? args)
                                 ;; No arguments.
                                 (list (->syn x metod)
                                       (list (->syn x 'this)
                                             (->syn x klass)))
                                 ;; Only ". rest".
                                 (append (list (->syn x metod))
                                         (cons (list (->syn x 'this)
                                                     (->syn x klass))
                                               (->syn x args)))))
          #,@(->syn x body)))))


;; Reference object member.
;;
;; (this-ref :name)
;;   ->
;; (slot-ref this ':name)
(define-syntax this-ref
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (->syn datum->syntax))
      #`(slot-ref #,(->syn x 'this) (quote #,(->syn x (cadr stx)))))))


;; Set object member.
;;
;; (this-set! :name value)
;;   ->
;; (slot-set! this ':name value)
(define-syntax this-set!
  (lambda (x)
    (let* ((stx (syntax->datum x))
           ;; (->str symbol->string)
           ;; (->sym string->symbol)
           (->syn datum->syntax))
      (with-syntax ((this (->syn x 'this)))
        #`(slot-set! this (quote #,(->syn x (cadr stx))) #,@(->syn x (cddr stx)))))))
