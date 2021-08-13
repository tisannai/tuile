(define-module (tuile utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module ((ice-9 control) #:select (% abort))
  #:use-module ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:use-module ((srfi srfi-88) #:select (string->keyword))
  #:export
  (
   any?
   empty?
   len-0?
   len-1?
   len-2?
   len-3?
   most
   best
   flatten
   flatten-0
   flatten-1

   pi
   ->integer-fraction
   unspecified
   uns

   command-line-arguments

   dir-list
   dir-glob
   dir-glob-re
   extname
   expand-file-name

   datum->string
   string->procedure
   common-eval
   funcall

   aif
   awhen
   for
   forever
   map-except-last
   map-except-first
   repeat-times
   from-to
   from-to-step
   nop

   define-im-record
   define-fp-record
   define-mu-record

   define-this-class
   define-this-method
   this-ref
   this-set!

   re-split
   re-match?
   re-match
   re-matches
   re-sub
   re-gsub

   vector-range
   vector-reverse
   vector-insert
   vector-delete

   assoc-has-key?
   assoc-update!
   assoc-repeat!
   assoc-merge

   hash-has-key?
   hash-keys
   hash-values

   read-lines-from-port
   with-each-line-from-port
   with-each-line-from-file
   open-output-port-with-filename
   close-output-port-with-filename
   with-output-to-filename
   file->lines
   lines->file
   ;;   file->line-list
   ;;   line-list->file

   capture-shell-command-values
   capture-shell-command
   capture-shell-command-stdout

   timestamp
   datestamp
   days-in-month

   old-memf
   find-all
   find-first

   with-exception-terminate
   string-clip
   make-string-list
   sequence
   range
   ))


;; ------------------------------------------------------------
;; Internal functions:


;; ------------------------------------------------------------
;; External functions:


(define any?   pair?)
(define empty? null?)
(define len-0? null?)
(define len-1? (lambda (lst) (and (pair? lst) (null? (cdr lst)))))
(define len-2? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))))
(define len-3? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (pair? (cddr lst)) (null? (cdddr lst)))))

;; Find the item in "lst" that has largest value returned by property
;; getter "fn".
;;
;;     (most identity '(1 2 3))
;;
(define (most fn lst)
  (if (null? lst)
      #f
      (let loop ((tail (cdr lst))
                 (wins (car lst))
                 (max (fn (car lst))))
        (if (pair? tail)
            (let ((score (fn (car tail))))
              (if (> score max)
                  (loop (cdr tail)
                        (car tail)
                        score)
                  (loop (cdr tail)
                        wins
                        max)))
            wins))))

;; Find the item in "lst" that is best by property
;; comparator "fn".
;;
;;     (best (lambda (i curwin) (> i curwin)) '(1 2 3))
;;
(define (best fn lst)
  (if (null? lst)
      #f
      (let loop ((tail (cdr lst))
                 (wins (car lst)))
        (if (pair? tail)
            (if (fn (car tail) wins)
                (loop (cdr tail)
                      (car tail))
                (loop (cdr tail)
                      wins))
            wins))))


;; Flatten (and join) argument list as deep as list goes.
(define (flatten . rest)
  (let loop ((lst rest)
             (res '()))
    (cond
     ((null? lst) res)
     ((pair? lst) (loop (car lst)
                        (loop (cdr lst)
                              res)))
     (else
      (cons lst res)))))


;; Flatten (and join) argument list without collapsing.
(define (flatten-0 . rest)
  (let loop ((lst rest))
    (if (pair? lst)
        ;; Select proper proc with if.
        ((if (pair? (car lst)) append cons)
         (car lst)
         (loop (cdr lst)))
        '())))


;; Flatten (and join) argument list by one level.
(define (flatten-1 . rest)
  (let loop ((lst rest)
             (res '()))
    (if (pair? lst)
        (cond
         ((pair? (car lst))
          ;; List item.
          (loop (cdr lst)
                (append res (apply flatten-0 (car lst)))))
         (else
          ;; Non-list item
          (loop (cdr lst)
                (append res (list (car lst))))))
        res)))


(define (clean-list lst)
  (let filter ((rest lst))
    (if (pair? rest)
        (if (unspecified? (car rest))
            (filter (cdr rest))
            (cons (car rest) (filter (cdr rest))))
        '())))


;; PI = 3.141592653589793
(define pi (* 2 (acos 0)))

;; Return integer and fractional parts of real as pair.
(define (->integer-fraction real decimals)
  (call-with-values (lambda ()
                      (let ((scaler (expt 10 decimals)))
                        (floor/ (* real scaler) scaler)))
    (lambda (q r)
      (cons (inexact->exact (round q))
            (inexact->exact (round r))))))

(define unspecified (if #f #t))
(define uns unspecified)


;; Return command line arguments, excluding the executable.
(define (command-line-arguments)
  (cdr (command-line)))


;; List given directory entries without the dot files.
(define (dir-list dir)
  (list-tail (scandir dir) 2))


;; Glob directory.
;;
;;     (dir-glob "../foo" "*.{c,cc}")
;;
(define (dir-glob dir pat)

  ;; Glob pattern to regexp.
  (define (glob->regexp pat)
    (let ((len (string-length pat))
          (in-selection 0))
      (string-concatenate
       (append
        (list "^")
        (let loop ((i 0))
          (if (< i len)
              (let ((char (string-ref pat i)))
                (case char
                  ((#\*) (cons "[^.]*" (loop (1+ i))))
                  ((#\?) (cons "[^.]" (loop (1+ i))))
                  ((#\[) (cons "[" (loop (1+ i))))
                  ((#\]) (cons "]" (loop (1+ i))))
                  ((#\{) (begin
                           (set! in-selection (1+ in-selection))
                           (cons "(" (loop (1+ i)))))
                  ((#\}) (begin
                           (set! in-selection (1- in-selection))
                           (cons ")" (loop (1+ i)))))
                  ((#\,) (if (> in-selection 0)
                             (cons "|" (loop (1+ i)))
                             (cons "," (loop (1+ i)))))
                  ((#\\)
                   (cons (list->string (list char (string-ref pat (1+ i))))
                         (loop (+ i 2))))
                  (else
                   (cons (regexp-quote (make-string 1 char))
                         (loop (1+ i))))))
              '()))
        (list "$")))))

  (dir-glob-re dir (glob->regexp pat)))


;; Glob directory with regexp.
;;
;;     (dir-glob-re "../foo" ".*[.](c|cc)")
;;
(define (dir-glob-re dir pat)
  (let ((rx (make-regexp pat)))
    (filter (lambda (x) (regexp-exec rx x)) (dir-list dir))))


;; Return filename suffix (without the dot).
(define (extname filename)
  (car (last-pair (string-split filename #\.))))

;; Convert filename to absolute, and canonicalize it (as in Emacs).
(define (expand-file-name filename)
  (if (eq? (string-ref filename 0) #\~)
      (string-append (getenv "HOME") (substring filename 1))
      (canonicalize-path filename)))


(define (datum->string datum)
  (with-output-to-string (lambda ()
                           (write datum))))

;; Convert string to procedure.
(define (string->procedure str)
  (eval (read (open-input-string str)) (interaction-environment)))

;; Eval datum.
(define (common-eval datum)
  (eval datum (interaction-environment)))

;; Funcall macro (to complement apply).
;;
;; NOTE: One does not have to use this macro in Scheme, since Scheme
;; is a Lisp-1 language, but it can be used for highlighting a special
;; function application case.
;;
;; Write as a function:
;;     (define (funcall fn . rest)
;;       (apply fn rest))
(define-syntax-rule (funcall fn arg1 ...)
  (apply fn (list arg1 ...)))


;; Anaphoric if macro.
;;
;;     (aif (1+ i)
;;          it
;;          #f)
(define-syntax aif
  (lambda (x)
    ;; "lst": Convert syntax to datum and convert back to a list of syntax.
    (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
      ;; Create template variable "it".
      (with-syntax ((it (datum->syntax x 'it)))
        #`(let ((it #,(cadr lst)))
            (if it #,(caddr lst) #,(cadddr lst)))))))

;;
;; aif - alternative implementations:
;;
;;(define-syntax aif
;;  (lambda (x)
;;    (syntax-case x ()
;;      ((_ test then else)
;;       (syntax-case (datum->syntax x 'it) ()
;;         (it
;;          #'(let ((it test))
;;              (if it then else))))))))
;;
;;(define-syntax aif
;;  (lambda (x)
;;    (syntax-case x ()
;;      ((_ test then else)
;;       (with-syntax ((it (datum->syntax x 'it)))
;;         #'(let ((it test))
;;             (if it then else)))))))


;; Anaphoric when macro.
;;
;;     (awhen (1+ i)
;;          it)
(define-syntax awhen
  (lambda (x)
    ;; "lst": Convert syntax to datum and convert back to a list of syntax.
    (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
      ;; Create template variable "it".
      (with-syntax ((it (datum->syntax x 'it)))
        #`(let ((it #,(cadr lst)))
            (when it #,(caddr lst)))))))


;; Execute for each in list.
;;
;;     (for ((i lst))
;;       (display i)
;;       (newline))
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


;; Loop forever.
;;
;;     (forever
;;      (display "hello")
;;      (newline))
;;
(define-syntax forever
  (lambda (x)
    (syntax-case x ()
      ((forever body ...)
       #'(let loop-forever ()
           body ...
           )))))


;; Map all list entries except last.
(define (map-except-last fn lst)
  (reverse (let process ((tail lst)
                         (res '()))
             (cond
              ((null? tail)
               res)
              ((pair? (cdr tail))
               (process (cdr tail)
                        (cons (fn (car tail))
                              res)))
              (else
               (process (cdr tail)
                        (cons (car tail)
                              res)))))))


;; Map all list entries except last.
(define (map-except-first fn lst)
  (reverse (let process ((tail lst)
                         (res '()))
             (cond
              ((null? tail)
               res)
              ((null? res)
               (process (cdr tail)
                        (cons (car tail)
                              res)))
              (else
               (process (cdr tail)
                        (cons (fn (car tail))
                              res)))))))


(define (fn-pipe arg . chain)
  (let loop ((res arg)
             (chain chain))
    (if (pair? chain)
        (loop ((car chain) res)
              (cdr chain))
        res)))

(define -> fn-pipe)

(define-syntax repeat-times
  (lambda (x)
    (syntax-case x ()
      ((_ cnt body ...)
       (with-syntax ((i (datum->syntax x 'i)))
         #'(let repeat-loop ((i 0))
             (when (< i cnt)
               (begin
                 body ...
                 (repeat-loop (1+ i))))))))))

;; Run "i" from "from" to "to" with step of 1.
(define-syntax from-to
  (lambda (x)
    (syntax-case x ()
      ((_ from to body ...)
       (with-syntax ((i (datum->syntax x 'i)))
         #'(let repeat-loop ((i from))
             (when (< i (1+ to))
               (begin
                 body ...
                 (repeat-loop (1+ i))))))))))

;; Run "i" from "from" to "to" with step of "step".
(define-syntax from-to-step
  (lambda (x)
    (syntax-case x ()
      ((_ from to step body ...)
       (with-syntax ((i (datum->syntax x 'i)))
         #'(let repeat-loop ((i from))
             (when (< i (1+ to))
               (begin
                 body ...
                 (repeat-loop (+ i step))))))))))


;; No operation, i.e. pass argument forward.
(define (nop arg)
  arg)


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
;;           (->str symbol->string)
;;           (->sym string->symbol)
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


;; Append item to list.
;;
;;     (append-item! lst 21)
(define (append-item! lst item)
  (append! lst (list item)))

;; Prepend item to list.
;;
;;     (prepend-item! lst 21)
(define (prepend-item! lst item)
  (set-cdr! lst (list-copy lst))
  (set-car! lst item))


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


;; Combine two mutable assoc lists.
;;
;;Override and complement entries in A with entries from B.
(define (assoc-merge a b)
  (define (amerge a b)
    (if (pair? b)
        (amerge (assoc-set! a (caar b) (cdar b))
                (cdr b))
        a))
  (amerge (amerge (list) a) b))


;; Hash table has key?
(define (hash-has-key? hsh key)
  (hash-get-handle hsh key))

;; Return list of hash table keys.
(define (hash-keys hsh)
  (hash-map->list (lambda (k v) k) hsh))

;; Return list of hash table keys.
(define (hash-values hsh)
  (hash-map->list (lambda (k v) v) hsh))


;; Read all lines from port to list (or vector).
;;
;; Key Args:
;;     with-newline    Leave newline to line end.
;;     as-vector       Return lines as vector (not list).
;;
(define* (read-lines-from-port port
                               #:key
                               (with-newline #f)
                               (as-vector #f))
  (define (->vector lst)
    (if as-vector
        (list->vector lst)
        lst))

  (let ((line-filter (if with-newline
                         (lambda (line)
                           (string-append (car line) "\n"))
                         (lambda (line)
                           (car line)))))

    (->vector (let loop ((line (read-line port 'split)))
                (if (eof-object? (car line))
                    '()
                    (cons (line-filter line) (loop (read-line port 'split))))))))

;;;; Read all lines without newline from port to vector.
;;(define (read-lines port)
;;  (define (read-clean-line port)
;;    (let ((line (read-line port)))
;;      (if (eof-object? line)
;;          line
;;          (string-trim-right line #\return))))
;;  (list->vector
;;   (let loop ((line (read-clean-line port)))
;;     (if (eof-object? line)
;;         '()
;;         (cons line (loop (read-clean-line port)))))))
;;
;;
;;;; Read all lines from port to list.
;;(define (read-lines-to-list port)
;;  (let loop ((line (read-line port)))
;;    (if (eof-object? line)
;;        '()
;;        (cons line (loop (read-line port))))))


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


;; Open output port for filename. Open a real file port, unless
;; filename is "<stdout>".
(define (open-output-port-with-filename filename)
  (if (string=? "<stdout>" filename)
      (current-output-port)
      (open-output-file filename)))

;; Close output port. Close the port, unless port is
;; "(current-output-port)" i.e. opened filename was "<stdout>".
(define (close-output-port-with-filename port)
  (when (not (equal? port
                     (current-output-port)))
    (close-output-port port)))

;; Execute one arg proc with port. If filename is "<stdout>", then
;; port is stdout, otherwise a fileport is opened and finally closed
;; after proc has been executed.
(define (with-output-to-filename filename proc)
  (let ((port (open-output-port-with-filename filename)))
    (proc port)
    (close-output-port-with-filename port)))

;; Get all lines from file to list (or vector).
;;
;; Key Args:
;;     with-newline    Leave newline to line end.
;;     as-vector       Return lines as vector (not list).
;;     binary          Access file in binary mode.
;;
(define* (file->lines filename
                      #:key
                      (with-newline #f)
                      (as-vector #f)
                      (binary #f))
  (call-with-input-file filename
    (lambda (port)
      (read-lines-from-port port
                            #:with-newline with-newline
                            #:as-vector as-vector))
    #:binary binary))


;; Write lines of list (or vector) to file adding newlines (or not).
;;
;; Key Args:
;;     with-newline    Line has newline, output line as is.
;;     binary          Access file in binary mode.
;;
(define* (lines->file filename lines
                      #:key
                      (with-newline #f)
                      (binary #f))
  (call-with-output-file filename
    (lambda (port)
      (let ((line-out (if with-newline
                          (lambda (line)
                            line)
                          (lambda (line)
                            (string-append line "\n")))))
        (cond
         ((vector? lines)
          (vector-for-each (lambda (i line)
                             (display line port)
                             (newline port))
                           lines))
         (else
          (for-each (lambda (line)
                      (display line port)
                      (newline port))
                    lines)))))
    #:binary binary))


;;;; Get all lines from file to list (including newlines).
;;(define* (file->line-list filename #:key (binary #f))
;;  (call-with-input-file filename
;;    (lambda (port)
;;      (read-lines-to-list port))
;;    #:binary binary))
;;
;;
;;;; Write lines (including newlines) to file (without adding newlines).
;;(define* (line-list->file filename lines #:key (binary #f))
;;  (call-with-output-file filename
;;    (lambda (port)
;;      (for-each (lambda (line)
;;                  (display line port)
;;                  lines)
;;                #:binary))))


;; Execute shell command and return responses as values.
;;
;; Responses: status-code stdout stderr
;;
(define (capture-shell-command-values cmd)
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
    (values (status:exit-val status) stdout stderr)))


;; Execute shell command and return responses as list.
;;
;; Responses: status-code stdout stderr
;;
(define (capture-shell-command cmd)
  (let-values (((status stdout stderr) (capture-shell-command-values cmd)))
    (list status stdout stderr)))


;; Execute shell command and return only the stdout, or false if
;; failure.
;;
(define (capture-shell-command-stdout cmd)
  (let-values (((status stdout stderr) (capture-shell-command-values cmd)))
    (if (equal? status 0)
        stdout
        #f)))


;; Current time w/o time segment.
;;
;;     Format (without time): <yymmdd>
;;     Format (with    time): <yymmdd_HHMM>
;;
(define* (timestamp #:key (use-time #f) (use-sec #f))
  (let ((formatter (cond
                    (use-sec
                     "~y~m~d_~H~M~S")
                    (use-time
                     "~y~m~d_~H~M")
                    (else
                     "~y~m~d"))))
    (srfi:date->string (srfi:current-date)
                       formatter)))


;; Current date.
;;
;;     Format: <yyyy-mm-dd>
;;
(define (datestamp)
  (srfi:date->string (srfi:current-date)
                     "~Y-~m-~d"))


;; Return the number of days in given month.
;;
(define (days-in-month year month)
  (case month
    ((1 3 5 7 8 10 12)
     31)
    ((4 6 9 11)
     30)
    (else
     ;; February has varying number of days.
     (if (or (= (remainder year 400) 0)
             (and (= (remainder year 4) 0)
                  (not (= (remainder year 100) 0))))
         29
         28))))


;; Find all items matched with one argument proc "fn" from "lst".
(define (old-memf fn lst)
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


;; Find all items matched with one argument proc "fn" from "lst".
(define (find-all fn lst)
  (let loop ((tail lst)
             (found< '()))
    (if (pair? tail)
        (if (fn (car tail))
            (loop (cdr tail)
                  (cons (car tail)
                        found<))
            (loop (cdr tail)
                  found<))
        (reverse found<))))


;; Find first item matching with one argument proc "fn" from "lst".
;;
;; Return item or false if no match.
;;
;; NOTE: Using a delimited continuation, with default-prompt-handler.
;;
(define (find-first fn lst)
  (% (let loop ((tail lst))
       (if (pair? tail)
           (if (fn (car tail))
               (abort (lambda (k)       ; Default prompt handler
                                        ; requires a proc.
                        (car tail)))
               (loop (cdr tail)))
           #f))))


;; Terminate exception (with "handler" of one argument) if exception
;; is generated by "proc" (a thunk).
(define (with-exception-terminate handler proc)
  (with-exception-handler handler
    proc
    #:unwind? #t))


;; Return substring with possibly negative indeces.
(define (string-clip str . rest)
  (define (normalize index)
    (if (< index 0)
        (+ (+ (string-length str) 1)
           index)
        index))

  (define (order i1 i2)
    (let ((n1 (normalize i1))
          (n2 (normalize i2)))
      (if (< n2 n1)
          (values n2 n1)
          (values n1 n2))))

  (cond
   ((= 0 (length rest))
    str)
   ((= 1 (length rest))
    (substring str
               0
               (normalize (first rest))))
   (else
    (call-with-values (lambda () (order (first rest) (second rest)))
      (lambda (n1 n2)
        (substring str
                   n1
                   n2))))))

;; Create list of string from symbols, i.e. non-quoted text.
;;
;;     (make-string-list foo bar dii)
;;
(define-syntax make-string-list
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (-> datum->syntax))
      #`(map symbol->string (quote #,(-> x (cdr stx)))))))


(define (sequence start len . rest)
  (let ((step (if (pair? rest) (car rest) 1)))
    (let loop ((num start))
      (if (< num (+ len start))
          (cons num (loop (+ num step)))
          '()))))

(define (range start end . rest)
  (let ((step (if (pair? rest) (car rest) 1)))
    (let loop ((num start))
      (if (< num end)
          (cons num (loop (+ num step)))
          '()))))


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
