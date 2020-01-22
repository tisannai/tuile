(define-module (tuile utils)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:export
  (
   command-line-arguments
   dir-list
   string->procedure
   aif
   for
   define-im-record
   this-met
   this-ref
   this-set!
   ))


;; ------------------------------------------------------------
;; Internal functions:


;; ------------------------------------------------------------
;; External functions:

;; Return command line arguments, excluding the executable.
(define (command-line-arguments)
  (cdr (command-line)))


;; List given directory entries without the dot files.
(define (dir-list dir)
  (list-tail (scandir dir) 2))


;; Convert string to procedure.
(define (string->procedure str)
  (eval (read (open-input-string str)) (interaction-environment)))


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


;; (for ((i lst))
;;   (display i)
;;   (newline))
(define-syntax for
  (lambda (x)
    (syntax-case x ()
      ((for ((i1 l1)) body ...)
       #'(for-each
          (lambda (i1)
            body ...)
          l1))
      ((for ((i1 l1) (i2 l2)) body ...)
       #'(for-each
          (lambda (i1 i2)
            body ...)
          l1 l2)))))


;; Expand this:
;;   (define-im-record foo bar hii)
;;
;; To this:
;;   (define-record-type foo
;;     (make-foo bar)
;;     foo?
;;     (bar   foo-bar)
;;     (hii   foo-hii)
;;     )
;;
(define-syntax define-im-record
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(define-record-type #,(datum->syntax x (cadr stx))
          (#,(datum->syntax x (string->symbol (string-append "make-" (symbol->string (cadr stx)))))
           #,@(map (lambda (i) (datum->syntax x i)) (cddr stx)))
          #,(datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "?")))
          #,@(map
              (lambda (i)
                (list
                 (datum->syntax x i)
                 (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx)) "-" (symbol->string i))))))
              (cddr stx))))))

;; TODO: define-mu-record, with all fields mutable (i.e. provide a set!).


;; Define method in a compact form.
;;
;;     (def-met my-class (my-method a1 a2)
;;       body ...)
;;
;; Becomes:
;;
;;      (define-method (my-class-my-method (this <my-class>) a1 a2)
;;        body ...)
;;
(define-syntax this-met
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (klass (cadr stx))
           (metod (caaddr stx))
           (args (cdaddr stx))
           (body (cdddr stx))
           (->syn datum->syntax))
      #`(define-method (#,(->syn x (symbol-append klass '- metod))
                        (#,(->syn x 'this) #,(->syn x (symbol-append '< klass '>)))
                        #,@(->syn x args))
          #,@(->syn x body)))))


;; Reference object variable.
;;
;; (this-ref :name)
;;   ->
;; (slot-ref this ':name)
(define-syntax this-ref
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (->syn datum->syntax))
      #`(slot-ref #,(->syn x 'this) (quote #,(->syn x (cadr stx)))))))


;; Set object variable.
;;
;; (this-set! :name value)
;;   ->
;; (slot-set! this ':name value)
(define-syntax this-set!
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (->str symbol->string)
           (->sym string->symbol)
           (->syn datum->syntax))
      (with-syntax ((this (->syn x 'this)))
        #`(slot-set! this (quote #,(->syn x (cadr stx))) #,@(->syn x (cddr stx)))))))

;; Usage:
;;
;;     (define-structure tree left right)
;;     (define t
;;       (make-tree
;;         (make-tree 0 1)
;;         (make-tree 2 3)))
#;
(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
          (string->symbol
            (apply string-append
              (map (lambda (x)
                     (if (string? x)
                         x
                         (symbol->string (syntax->datum x))))
                   args))))))
    (syntax-case x ()
      [(_ name field ...)
       (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                     [predicate (gen-id #'name #'name "?")]
                     [(access ...)
                      (map (lambda (x) (gen-id x #'name "-" x))
                           #'(field ...))]
                     [(assign ...)
                      (map (lambda (x)
                             (gen-id x "set-" #'name "-" x "!"))
                           #'(field ...))]
                     [structure-length (+ (length #'(field ...)) 1)]
                     [(index ...)
                      (let f ([i 1] [ids #'(field ...)])
                        (if (null? ids)
                            '()
                            (cons i (f (+ i 1) (cdr ids)))))])
         #'(begin
             (define constructor
               (lambda (field ...)
                 (vector 'name field ...)))
             (define predicate
               (lambda (x)
                 (and (vector? x)
                      (= (vector-length x) structure-length)
                      (eq? (vector-ref x 0) 'name))))
             (define access
               (lambda (x)
                 (vector-ref x index)))
             ...
             (define assign
               (lambda (x update)
                 (vector-set! x index update)))
             ...))])))

#;
(define-syntax sim
  (lambda (x)
    (syntax 12)))

#;
(define-syntax sim-foo
  (lambda (x)
    (syntax 'sim-foo)))

#;
(define-syntax sim
  (lambda (x)
    (let ((tmp (syntax->datum x)))
      #`(display #,(string-append "make-" (number->string (cadr tmp)) "\n")))))

#;
(define-syntax case
  (lambda (x)
    (syntax-case x ()
      [(_ e c1 c2 ...)
       #`(let ([t e])
           #,(let f ([c1 #'c1] [cmore #'(c2 ...)])
               (if (null? cmore)
                   (syntax-case c1 (else)
                     [(else e1 e2 ...) #'(begin e1 e2 ...)]
                     [((k ...) e1 e2 ...)
                      #'(if (memv t '(k ...)) (begin e1 e2 ...))])
                   (syntax-case c1 ()
                     [((k ...) e1 e2 ...)
                      #`(if (memv t '(k ...))
                            (begin e1 e2 ...)
                            #,(f (car cmore) (cdr cmore)))]))))])))


#;
(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break (datum->syntax #'k 'break)])
         #'(call/cc
             (lambda (break)
               (let f () e ... (f)))))])))

#;
(let ([n 3] [ls '()])
  (loop
    (if (= n 0) (break ls))
    (set! ls (cons 'a ls))
    (set! n (- n 1))))

#;
(define-syntax with-syntax
   (lambda (x)
      (syntax-case x ()
         ((_ () e1 e2 ...)
          #'(let () e1 e2 ...))
         ((_ ((out in)) e1 e2 ...)
          #'(syntax-case in ()
              (out (let () e1 e2 ...))))
         ((_ ((out in) ...) e1 e2 ...)
          #'(syntax-case (list in ...) ()
              ((out ...) (let () e1 e2 ...)))))))

#;
(define-syntax syntax-rules
  (lambda (xx)
    (syntax-case xx ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       #'(lambda (x)
           ;; embed patterns as procedure metadata
           #((macro-type . syntax-rules)
             (patterns pattern ...))
           (syntax-case x (k ...)
             ((_ . pattern) #'template)
             ...)))
      ((_ (k ...) docstring ((keyword . pattern) template) ...)
       (string? (syntax->datum #'docstring))
       #'(lambda (x)
           ;; the same, but allow a docstring
           docstring
           #((macro-type . syntax-rules)
             (patterns pattern ...))
           (syntax-case x (k ...)
             ((_ . pattern) #'template)
             ...))))))
