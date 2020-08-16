(define-module (tuile utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:export
  (
   flatten
   flatten-1
   command-line-arguments
   dir-list
   string->procedure
   aif
   for
   define-im-record
   define-mu-record
   this-met
   this-ref
   this-set!
   re-split
   re-match?
   re-match
   re-matches
   re-sub
   re-gsub
   extname
   vector-range
   vector-reverse
   vector-insert
   vector-delete
   assoc-has-key?
   assoc-update!
   assoc-repeat!
   hash-has-key?
   hash-keys
   read-lines
   with-each-line-from-port
   with-each-line-from-file
   file->lines
   lines->file
   capture-shell-command
   memf
   with-exception-terminate
   ))


;; ------------------------------------------------------------
;; Internal functions:


;; ------------------------------------------------------------
;; External functions:


;; Flatten list as deep as list goes.
(define (flatten lst)
  (let loop ((lst lst)
             (res '()))
    (cond
     ((null? lst) res)
     ((pair? lst) (loop (car lst)
                        (loop (cdr lst)
                              res)))
     (else
      (cons lst res)))))


;; Flatten list one level.
(define (flatten-1 lst)
  (let loop ((lst lst))
    (if (pair? lst)
        ;; Select proper proc with if.
        ((if (pair? (car lst)) append cons)
         (car lst)
         (loop (cdr lst)))
        '())))


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


(define (fn-pipe arg . chain)
  (let loop ((res arg)
             (chain chain))
    (if (pair? chain)
        (loop ((car chain) res)
              (cdr chain))
        res)))

(define -> fn-pipe)


;; Create immutable record.
;;
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

;; Create mutable record.
;;
;; Expand this:
;;   (define-mu-record foo bar hii)
;;
;; To this:
;;   (define-record-type foo
;;     (make-foo bar)
;;     foo?
;;     (bar   foo-bar set-foo-bar!)
;;     (hii   foo-hii set-foo-hii!)
;;     )
;;
(define-syntax define-mu-record
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
                 (datum->syntax x (string->symbol (string-append (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i))))
                 (datum->syntax x (string->symbol (string-append "set-"
                                                                 (symbol->string (cadr stx))
                                                                 "-"
                                                                 (symbol->string i)
                                                                 "!")))))
              (cddr stx))))))

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


;; Split string with regexp.
;;
;; Examples:
;;     guile> (re-split "[-x]+" "foo--x--bar---what--")
;;     ("foo" "bar" "what" "")
;;     guile> (re-split "[-x]+" "foo--x--bar---what--"  'trim)
;;     ("foo" "bar" "what")
;;     guile> (re-split "[-x]+" "foo--x--bar---what"  'keep)
;;     ("foo" "--x--" "bar" "---" "what")
;;
(define (re-split re str . options)
  (let ((keep #f) (trim #f))
    (when (member 'keep options)
      (set! options (delete 'keep options))
      (set! keep #t))
    (when (member 'trim options)
      (set! options (delete 'trim options))
      (set! trim #t))
    (let* ((matches (apply list-matches re str options))
           (indices
            (append '(0)
                    (fold-right
                     (lambda (m acc)
                       (cons (match:start m)
                             (cons (match:end m) acc))) '()
                             matches)
                    (list (string-length str))))
           (substrings
            (pair-fold-right
             (lambda (lst accum)
               (if (or (even? (length lst))
                       (and keep (> (length lst) 1)))
                   (cons (apply substring str (take lst 2)) accum)
                   accum))
             '()
             indices)))
      (if trim
          (reverse! (drop-while
                     string-null?
                     (reverse! (drop-while string-null? substrings))))
          substrings))))


;; Return true if regexp matches str.
(define (re-match? re str)
  (regexp-match? (regexp-exec (make-regexp re) str)))

;; Return regexp match str or false.
(define (re-match re str)
  (aif (string-match re str)
       (match:substring it)
       #f))

;; Return regexp match string list or empty list.
(define (re-matches re str)
  (map match:substring (list-matches re str)))

;; Substitute regexp in string with replacement once.
(define (re-sub re str rep)
  (aif (string-match re str)
       (regexp-substitute #f it 'pre rep 'post)
       str))

;; Substitute regexp in string with replacement globally.
(define (re-gsub re str rep)
  (aif (string-match re str)
       (regexp-substitute/global #f re str 'pre rep 'post)
       str))

;; Return filename suffix (without the dot).
(define (extname filename)
  (car (last-pair (string-split filename #\.))))


;; Get vector elements by range: [a,b).
;;
;; a is inclusive and b is exclusive.
;;
(define (vector-range vec a b)
  (let ((new-vec (make-vector (- b a))))
    (vector-move-left! vec a b new-vec 0)
    new-vec))

;; Immutable vector reverse.
(define (vector-reverse vec)
  (vector-reverse-copy vec))

(define (args-to-vector args)
  (cond
   ((vector? args) args)
   ((list? args) (list->vector args))
   (else (vector args))))

;; Insert items to vector position and return a new vector.
(define (vector-insert vec pos items)
  (vector-append (vector-range vec 0 pos)
                 (args-to-vector items)
                 (vector-range vec pos (vector-length vec))))

;; Delete count number of items from vector at pos.
(define (vector-delete vec pos count)
  (vector-append (vector-range vec 0 pos)
                 (vector-range vec (+ pos count) (vector-length vec))))


;; Assoc list has key?
(define (assoc-has-key? assoc key)
  (find (lambda (i) (equal? key i))
        (map car assoc)))


;; Update assoc entry using one argument procedure.
(define (assoc-update! assoc-list key proc)
  (if (assoc-has-key? assoc-list key)
      (let ((pair (assoc key assoc-list)))
        (set-cdr! pair
                  (proc (cdr pair)))
        assoc-list)
      #f))


;; Evaluate "proc" for massoc for each pair in set-list.
(define (assoc-repeat! assoc-list proc set-list)
  (if (pair? set-list)
      (assoc-repeat! (proc assoc-list
                           (caar set-list)
                           (cdar set-list))
                     proc
                     (cdr set-list))
      assoc-list))


;; Hash table has key?
(define (hash-has-key? hsh key)
  (hash-get-handle hsh key))

;; Return list of hash table keys.
(define (hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))


;; Read all lines for port to list.
(define (read-lines port)
  (define (read-clean-line port)
    (let ((line (read-line port)))
      (if (eof-object? line)
          line
          (string-trim-right line #\return))))
  (list->vector
   (let loop ((line (read-clean-line port)))
     (if (eof-object? line)
         '()
         (cons line (loop (read-clean-line port)))))))


;; Call "proc" with each line from port.
(define (with-each-line-from-port port proc)
  (let ((line (read-line port)))
    (when (not (eof-object? line))
      (proc line)
      (with-each-line-from-port port proc))))


;; Call "proc" with each line from input file.
(define (with-each-line-from-file file proc)
  (call-with-input-file file
    (lambda (port)
      (with-each-line-from-port port proc))))


;; Get all lines from file.
(define* (file->lines filename #:key (binary #f))
  (call-with-input-file filename
    (lambda (port)
      (read-lines port))
    #:binary binary))


;; Write lines to file.
(define* (lines->file filename lines #:key (binary #f))
  (call-with-output-file filename
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                lines)
      #:binary)))


;; Execute shell command and return responses as list.
;;
;; Responses: status-code stdout stderr
;;
(define (capture-shell-command cmd)
  (let* ((stdout #f)
         (stderr #f)
         (status #f))
    (define (command cmd)
      (let ((fh (open-input-pipe cmd)))
        (set! stdout (get-string-all fh))
        (set! status (close-pipe fh))))
    (let ((err-pipe (pipe)))
      (with-error-to-port (cdr err-pipe)
        (lambda ()
          (command cmd)))
      (close-port (cdr err-pipe))
      (set! stderr (get-string-all (car err-pipe))))
    (list (status:exit-val status) stdout stderr)))


;; Find all items matched with one argument proc "fn" from "lst".
(define (memf fn lst)
  (let loop ((rest lst)
             (res '()))
    (if (pair? rest)
        (if (fn (car rest))
            (loop (cdr rest)
                  (cons (car rest)
                        res))
            (loop (cdr rest)
                  res))
        (reverse res))))


;; Terminate exception (with "handler" of one argument) if exception
;; is generated by "proc" (a thunk).
(define (with-exception-terminate handler proc)
  (with-exception-handler handler
                  proc
                  #:unwind? #t))

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
