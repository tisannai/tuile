(define-module (tuile utils)
  #:use-module (srfi srfi-1)
;;  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  ;;  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module ((ice-9 control) #:select (% abort))
;;  #:use-module ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:use-module ((srfi srfi-88) #:select (string->keyword))
  #:use-module ((ice-9 exceptions) #:select (make-non-continuable-error))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module (tuile re)
;;   #:use-module ((tuile pr) #:select (ss si :in :rj))
  #:use-module ((tuile pr) #:select (ss si))
  #:use-module ((tuile fmt) #:select (fmt))
  #:use-module ((tuile fnmatch) #:select (fnmatch-for-glob))
;;   #:use-module (tuile compatible)
  #:export
  (

   aif
   awhen
   for
   forever
   map-except-last
   map-except-first
   repeat
   repeat-times
   for-n
   for-n!
   from-to
   from-to-step
   nop

   any?
   empty?
   len-0?
   len-1?
   len-2?
   len-3?
   most
   best
   all
   one
   lref-0
   lref-1
   lref-2
   lref-3
   lref-4
   l0
   l1
   l2
   l3
   l4
   l5
   l6
   l7
   l8
   l9
   flatten
   flatten-0
   flatten-1
   nth
   delete-nth
;;    list/
   listify
   list->cons
   cons->list
   list-join
   list-clean
   list-specified
   list-split
   list-slice
   list-range
   list-pick
   list-compact
;;    list-clean
   list-randomize
   list-update
   list-update!
   make-list-stack
   list-push!
   list-pop!
   map-compact
   clean-list

   pi
   div
   factorial
   ->integer-fraction
   prime-numbers
   specified?

   command-line-arguments

   dir-list
   dir-glob
   dir-glob-re
   dir-glob-with-path
   extname
   expand-file-name
   is-file?
   is-directory?
   file-modify-time
   file-update?
   file-newer?
   file-older?


   datum->string
   string->procedure
   common-eval
   opt-arg

;;   record-type
;;   define-im-record
;;   define-fp-record
;;   define-mu-record

;;   re-split-old
;;   re-match?
;;   re-match
;;   re-matches
;;   re-sub
;;   re-gsub

   vector-range
   vector-reverse
   vector-insert
   vector-delete
   vector-head->list

   alist
   assoc-has-key?
   assoc-update!
   assoc-repeat!
   assoc-merge
   assoc-ref-deep
   assoc-ref-single

   hash-has-key?
   ;; Now in hash.scm
;;    hash-keys
;;    hash-values

   read-lines-from-port
   with-each-line-from-port
   with-each-line-from-file
   with-input-from-file-if
   open-output-port-with-filename
   close-output-port-with-filename
   with-output-to-filename
   with-output-to-filename-if
   file->lines
   lines->file
   file->code
   file-read-line
   file-write-line
   ;;   file->line-list
   ;;   line-list->file

   capture-shell-command-values
   capture-shell-command
   capture-shell-command-alist
   capture-shell-command-stdout

   timestamp
   datestamp
   days-in-month

   terminal-dimensions

   old-memf
   find-all
   find-first
   find-member

   with-exception-terminate
   with-exception-return-false
   char-separator?
   char-nonseparator?
   string-whitespace?
   string-ref-safe
   string-clip
   string-escape
   string-unscape
   string-split-unscape
   string-split-with
   string-split-param
   string-gen
   make-string-list
   number-sequence
   number-range
   number-span

   with-file-or-fail
   make-temporary-file

   define-with-memoize

   seconds->minutes:seconds
   ))


;; ------------------------------------------------------------
;; Internal functions:


;; ------------------------------------------------------------
;; External functions:



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

(define (repeat cnt fn)
  (let lp ((i 0)
           (ret '()))
    (if (< i cnt)
        (lp (1+ i)
            (cons (fn i) ret))
        (reverse ret))))

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


;; Run indeces through specified values and execute the body with the
;; swept value. Collect the body return values to a list.
;;
;;     (for-n (i 8) i)
;;     (for-n (i 8 2) i)
;;     (for-n (i 8 -2) i)
;;     (for-n (i (0 8)) i)
;;     (for-n (i (8 0)) i)
;;     (for-n (i (0 8) 2) i)
;;     (for-n (i (8 0) 2) i)
;;     (for-n (i (8 0) -2) i)
;;

;; Helper functions for for-n and for-n!.

(define (for-repeat-n op var ini lim step body)
  #`(let repeat-loop ((#,var #,ini)
                      (ret '()))
      (if (#,op #,var #,lim)
          (repeat-loop (+ #,var #,step)
                       (cons (begin #,@body) ret))
          (reverse ret))))

(define (for-repeat-n! op var ini lim step body)
  #`(let repeat-loop ((#,var #,ini))
      (when (#,op #,var #,lim)
        (begin
          #,@body
          (repeat-loop (+ #,var #,step))))))

(define for-trans
  (lambda (x fn)
    (define -> datum->syntax)
    (define => syntax->datum)
    (syntax-case x ()

      ;;     (for-n (i (0 8) 2) i)
      ((_ (var (ini lim) step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        ((> (=> #'ini) (=> #'lim))
         (fn (-> x '>)
             #'var
             #'ini
             #'lim
             (- (=> #'step))
             #'(body ...)))
        (else
         (fn (-> x '<) #'var 0 #'lim #'step #'(body ...)))))

      ;;     (for-n (i (0 8)) i)
      ((_ (var (ini lim)) body ...)
       (cond
        ((> (=> #'ini) (=> #'lim))
         (fn (-> x '>)
             #'var
             #'ini
             #'lim
             -1
             #'(body ...)))
        (else
         (fn (-> x '<) #'var #'ini #'lim 1 #'(body ...)))))

      ;;     (for-n (i 8) i)
      ((_ (var lim) body ...)
       (fn (-> x '<) #'var 0 #'lim 1 #'(body ...)))

      ;;     (for-n (i 8 2) i)
      ((_ (var lim step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        (else
         (fn (-> x '<) #'var 0 #'lim #'step #'(body ...))))))))

(define-syntax for-n (lambda (x) (for-trans x for-repeat-n)))

;; Run indeces through specified values and execute the body with the
;; swept value. Nothing is done to the body result values.
(define-syntax for-n! (lambda (x) (for-trans x for-repeat-n!)))

;; (for-n (i 10) (display i) (newline) i)
;; (for-n! (i 10) (display i) (newline))


#;
(define-syntax for-n
  (lambda (x)

    (define (for-core op var ini lim step body)
      #`(let repeat-loop ((#,var #,ini)
                          (ret '()))
          (if (#,op #,var #,lim)
              (repeat-loop (+ #,var #,step)
                           (cons #,@body ret))
              (reverse ret))))

    (define -> datum->syntax)
    (define => syntax->datum)

    (syntax-case x ()

      ((_ (var (ini lim) step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        ((> (=> #'ini) (=> #'lim))
         (for-core (-> x '>)
              #'var
              #'ini
              #'lim
              (- (=> #'step))
              #'(body ...)))
        (else
         (for-core (-> x '<) #'var 0 #'lim #'step #'(body ...)))))

      ((_ (var (ini lim)) body ...)
       (cond
        ((> (=> #'ini) (=> #'lim))
         (for-core (-> x '>)
              #'var
              #'ini
              #'lim
              -1
              #'(body ...)))
        (else
         (for-core (-> x '<) #'var #'ini #'lim 1 #'(body ...)))))

      ((_ (var lim) body ...)
       (for-core (-> x '<) #'var 0 #'lim 1 #'(body ...)))

      ((_ (var lim step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        (else
         (for-core (-> x '<) #'var 0 #'lim #'step #'(body ...))))))))

#;
(define-syntax for-n!
  (lambda (x)

    (define (for-core op var ini lim step body)
      #`(let repeat-loop ((#,var #,ini))
          (when (#,op #,var #,lim)
            (begin
              #,@body
              (repeat-loop (+ #,var #,step))))))

    (define -> datum->syntax)
    (define => syntax->datum)

    (syntax-case x ()

      ((_ (var (ini lim) step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        ((> (=> #'ini) (=> #'lim))
         (for-core (-> x '>)
              #'var
              #'ini
              #'lim
              (- (=> #'step))
              #'(body ...)))
        (else
         (for-core (-> x '<) #'var 0 #'lim #'step #'(body ...)))))

      ((_ (var (ini lim)) body ...)
       (cond
        ((> (=> #'ini) (=> #'lim))
         (for-core (-> x '>)
              #'var
              #'ini
              #'lim
              -1
              #'(body ...)))
        (else
         (for-core (-> x '<) #'var #'ini #'lim 1 #'(body ...)))))

      ((_ (var lim) body ...)
       (for-core (-> x '<) #'var 0 #'lim 1 #'(body ...)))

      ((_ (var lim step) body ...)
       (cond
        ((< (=> #'step) 0) #'(quote ()))
        (else
         (for-core (-> x '<) #'var 0 #'lim #'step #'(body ...))))))))


;; Execute body the given number of times with the variable.
;;
;;     (for ((i lst))
;;       (display i)
;;       (newline))
(define-syntax dotimes
  (lambda (x)
    (syntax-case x ()
      ((dotimes (var cnt) body ...)
       #'(let lp ((var 0))
           (when (< var cnt)
             body ...
             (lp (1+ var))))))))

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


;; Return #t if all are non-false.
(define (all lst)
  (let lp ((lst lst))
    (if (pair? lst)
        (if (car lst)
            (lp (cdr lst))
            #f)
        #t)))


;; Return #t if one is non-false.
(define (one lst)
  (let lp ((lst lst))
    (if (pair? lst)
        (if (car lst)
            #t
            (lp (cdr lst)))
        #f)))


;; Create function definitions (by x amount) using list-ref as:
;;
;;     (define (lref-0 lst) (list-ref lst 0))
;;     (define (lref-1 lst) (list-ref lst 1))
;;     ...
;;
(define-syntax expand-list-ref-index-functions
  (lambda (x)
    (define (get-list-ref-function index)
      (list 'define
            (list (string->symbol (string-append "lref-" (number->string index))) 'lst)
            (list 'list-ref 'lst index)))
    (let* ((-> datum->syntax))
      #`(begin #,@(-> x (map get-list-ref-function (iota (cadr (syntax->datum x)))))))))

(expand-list-ref-index-functions 5)


;; Create function definitions (by x amount) using list-ref as:
;;
;;     (define (l0 lst) (list-ref lst 0))
;;     (define (l1 lst) (list-ref lst 1))
;;     ...
;;
(define-syntax expand-short-list-ref-index-functions
  (lambda (x)
    (define (get-list-ref-function index)
      (list 'define
            (list (string->symbol (string-append "l" (number->string index))) 'lst)
            (list 'list-ref 'lst index)))
    (let* ((-> datum->syntax))
      #`(begin #,@(-> x (map get-list-ref-function (iota (cadr (syntax->datum x)))))))))

(expand-short-list-ref-index-functions 10)

;; Flatten (and join) argument list as deep as list goes.
;;
;; This is a tail-recursive version of flatten. Stack represents what
;; is left to process. We examine the top of stack. If it simple item,
;; we add it to result and remove it from stack. If it is a list, we
;; add head of list and rest of list to the top of stack. We recurse
;; until stack is empty and all items are in result.
;;
;;     stack: (1 ((2) 3) 4)
;;     res: ()
;;     stack: (((2) 3) 4)
;;     res: (1)
;;     stack: ((2) (3) 4)
;;     res: (1)
;;     stack: (2 () (3) 4)
;;     res: (1)
;;     stack: (() (3) 4)
;;     res: (2 1)
;;     stack: ((3) 4)
;;     res: (2 1)
;;     stack: (3 () 4)
;;     res: (2 1)
;;     stack: (() 4)
;;     res: (3 2 1)
;;     stack: (4)
;;     res: (3 2 1)
;;     stack: ()
;;     res: (4 3 2 1)
;;
;;     => (1 2 3 4)
;;
(define (flatten . rest)
  (let loop ((stack rest)
             (res '()))
    (cond
     ((null? stack)
      ;; We are done, reverse the result.
      (reverse res))
     ((null? (car stack))
      ;; Eat out empty heads.
      (loop (cdr stack)
            res))
     ((pair? (car stack))
      ;; Convert stack into: (<head> <tail> <rest-of-stack>).
      (loop (cons (caar stack)
                  (cons (cdar stack)
                        (cdr stack)))
            res))
     (else
      ;; Add head to result.
      (loop (cdr stack)
            (cons (car stack) res))))))


(define (non-tail-flatten . rest)
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


;; Return nth element of list.
(define (nth i lst)
  (list-ref lst i))

;; Delete nth element from list.
(define (delete-nth lst nth)
  (let loop ((head '())
             (tail lst)
             (i 0))
    (if (and (pair? tail)
             (< i nth))
        (loop (cons (car tail) head)
              (cdr tail)
              (1+ i))
        (append (reverse head)
                (if (pair? tail) (cdr tail) '())))))


;; Return item as list, no change for list.
(define (listify item)
  (cond
   ((pair? item) item)
   ((not item) (list))
   (else (list item))))

;; Convert list to a cons.
(define (list->cons arg)
  (if (pair? arg)
      (cons (first arg) (second arg))
      (cons arg #f)))

;; Convert cons to a list.
(define (cons->list arg)
  (list (car arg) (cdr arg)))

;; Append all arguments to flat list.
(define (list-join . rest)
  (apply append (map (lambda (i) (if (list? i) i (list i)))
                     rest)))

;; Return a clean list (no falses, no unspecified).
(define (list-clean lst)
  (let loop ((lst lst)
             (ret (list)))
    (if (pair? lst)
        (loop (cdr lst)
              (if (or (unspecified? (car lst))
                      (not (car lst)))
                  ret
                  (cons (car lst) ret)))
        (reverse ret))))


;; Return a clean list (no unspecified).
(define (list-specified lst)
  (let loop ((lst lst)
             (ret (list)))
    (if (pair? lst)
        (loop (cdr lst)
              (if (unspecified? (car lst))
                  ret
                  (cons (car lst) ret)))
        (reverse ret))))


;; ;; Create a clean list, i.e. don't include unspecified entries.
;; (define (list/ . rest)
;;   (cond
;;    ((null? rest) (list))
;;    (else
;;     (let loop ((rest rest)
;;                (ret '()))
;;       (if (pair? rest)
;;           (if (unspecified? (car rest))
;;               (loop (cdr rest) ret)
;;               (loop (cdr rest) (cons (car rest) ret)))
;;           (reverse ret))))))


;; Split list from Nth element and return results as pair.
(define (list-split lst n)
  (cond
   ((>= n (length lst)) (cons '() lst))
   ((<= (length lst) 1)
    (case n
      ((0) (cons '() lst))
      ((1) (cons lst '()))))
   (else (let lp ((rest lst)
                  (head '())
                  (i 0))
           (if (< i n)
               (lp (cdr rest)
                   (cons (car rest) head)
                   (1+ i))
               (cons (reverse head) rest))))))


;; Take a slice of the list.
;;
;; "list-slice" accepts negative indeces and they are taken as
;; references from the list end. If "a" and "b" are in reverse order,
;; the list slice will be reverse order as well.
;;
;;     (list-slice (list 1 2 3 4) -3 -2)
;;
(define (list-slice lst a b)
  (let* ((len (length lst))
         (a (if (< a 0) (+ len a) a))
         (b (if (< b 0) (+ len b) b))
         (left (if (<= a b) a b))
         (count (1+ (if (<= a b) (- b a) (- a b)))))
    (if (> (+ left count) len)
        lst
        (let ((slice (take (drop lst left) count)))
          (if (> a b)
              (reverse slice)
              slice)))))


;; Take a slice of the list by start point and length.
(define (list-range lst a len)
  (list-slice lst a (1- (+ a len))))


;; Pick item from lst by providing list of indeces in spec. The
;; indeces are used to travel through the lst hierarchy.
(define (list-pick lst spec)
  (if (pair? spec)
      (list-pick (list-ref lst (car spec))
                 (cdr spec))
      lst))


;; Compact list by removing (by default) unspecified and false values.
(define (list-compact lst . opt-pred)
  (let ((pred (if (pair? opt-pred)
                  (car opt-pred)
                  (lambda (item) (not (or (unspecified? item)
                                          (not item)))))))
    (filter pred lst)))


;; ;; Create a clean list, excluding any unspecified entries.
;; (define (list-clean . lst)
;;   (let loop ((lst lst)
;;            (ret (list)))
;;     (if (pair? lst)
;;         (loop (cdr lst)
;;             (if (unspecified? (car lst))
;;                 ret
;;                 (cons (car lst) ret)))
;;         (reverse ret))))


;; Randomize the list ordering.
;;
;; If optional argument is given as #f the random state from platform
;; is applied for seeding. If non-#f argument is given, the given
;; value is used for seeding.
(define (list-randomize lst . rest)

  (when (pair? rest)
    (if (car rest)
        (set! *random-state* (seed->random-state (car rest)))
        (set! *random-state* (random-state-from-platform))))

  (let* ((vec (list->vector lst))
         (size (vector-length vec)))
    (let loop ((cnt 0))
      (if (< cnt size)
          (begin
            (vector-swap! vec cnt (random size))
            (loop (1+ cnt)))
          (vector->list vec)))))


;; Update list content and return a new list with updated content.
;;
;; Modify the list item at position "index" with "modifier" proc.
;;
(define (list-update lst index modifier)
  (cond
   ((= index 0)
    (cons (modifier (list-ref lst index))
          (cdr lst)))
   ((= index (- (length lst) 1))
    (append (take lst index)
            (list (modifier (list-ref lst index)))))
   ((>= index (length lst))
    lst)
   (else
    (append (take lst index)
            (list (modifier (list-ref lst index)))
            (drop lst (1+ index))))))


;; Update list content by mutation.
;;
;; Modify the list item at position "index" with "modifier" proc.
;;
(define (list-update! lst index modifier)
  (cond
   ((>= index (length lst)) lst)
   (else
    (list-set! lst index (modifier (list-ref lst index)))
    lst)))


(define (make-list-stack . rest)
  (cond
   ((null? rest) (list *unspecified*))
   (else rest)))


(define (list-push! lst item)
  (cond
   ((unspecified? (car lst))
    (set-cdr! lst (list))
    (set-car! lst item))
   (else
    (set-cdr! lst (list-copy lst))
    (set-car! lst item)))
  lst)


(define (list-pop! lst)
  (let ((item (car lst)))
    (cond
     ((= (length lst) 1)
      (set-cdr! lst (list))
      (set-car! lst *unspecified*))
     (else
      (set-car! lst (cadr lst))
      (set-cdr! lst (cddr lst))))
    item))


;; Map list by removing (by default) unspecified values.
(define (map-compact proc lst)
  (let loop ((rest lst)
             (ret '()))
    (if (pair? rest)
        (let ((res (proc (car rest))))
          (if (not (unspecified? res))
              (loop (cdr rest)
                    (cons res ret))
              (loop (cdr rest)
                    ret)))
        (reverse ret))))


(define (clean-list lst)
  (let filter ((rest lst))
    (if (pair? rest)
        (if (unspecified? (car rest))
            (filter (cdr rest))
            (cons (car rest) (filter (cdr rest))))
        '())))


;; PI = 3.141592653589793
(define pi (* 2 (acos 0)))

;; Perform either float or integer divisions, based on the divider
;; type.
(define (div divident . dividers)
  (define (divide divident dividers)
    (if (pair? dividers)
      (if (or (inexact? (car dividers))
              (inexact? divident))
          (divide (/ divident (car dividers))
               (cdr dividers))
          (divide (quotient divident (car dividers))
               (cdr dividers)))
      divident))
  (divide divident dividers))

(define (factorial n)
  (let lp ((i 1)
           (res 1))
    (if (<= i n)
        (lp (1+ i)
            (* i res))
        res)))

;; Return integer and fractional parts of real as pair.
(define (->integer-fraction real decimals)
  (if (< real 0.0)
      (call-with-values (lambda ()
                          (let ((scaler (expt 10 decimals)))
                            (floor/ (* real (- scaler)) scaler)))
        (lambda (q r)
          (cons (- (inexact->exact (round q)))
                (inexact->exact (round r)))))
      (call-with-values (lambda ()
                          (let ((scaler (expt 10 decimals)))
                            (floor/ (* real scaler) scaler)))
        (lambda (q r)
          (cons (inexact->exact (round q))
                (inexact->exact (round r)))))))

(define (prime-numbers n)

  (define (sieve-of-eratosthenes-org n)
    (define sieve (make-vector (+ n 1) #t)) ; Initialize a vector to track primes.

    (define (mark-multiples p)
      (do ((i (* p p) (+ i p))) ((>= i (+ n 1)))
        (vector-set! sieve i #f))) ; Mark multiples of primes as non-prime

    (do ((p 2 (+ p 1))) ((>= p (sqrt n)))
      (if (vector-ref sieve p)
          (mark-multiples p))) ; Mark multiples starting from 2 up to sqrt(n)

    (filter (lambda (x) (vector-ref sieve x)) (iota (- n 1) 2))) ; Collect prime numbers from the sieve


  (define (sieve-of-eratosthenes n)

    ;; Initialize a vector to track primes.
    (define sieve (make-vector (+ n 1) #t))

    ;; Mark multiples of primes as non-prime.
    (define (mark-multiples p)
      (let lp ((i (* p p)))
        (when (< i (+ n 1))
          (vector-set! sieve i #f)
          (lp (+ i p)))))

    ;; Mark multiples starting from 2 up to sqrt(n).
    (let lp ((p 2))
      (when (< p (sqrt n))
        (when (vector-ref sieve p)
          (mark-multiples p))
        (lp (+ p 1))))

    ;; Collect prime numbers from the sieve.
    (let lp ((i 2)
             (ret '()))
      (if (<= i n)
          (lp (1+ i)
              (aif (vector-ref sieve i)
                   (cons i ret)
                   ret))
          (reverse ret))))

  (sieve-of-eratosthenes n))

;; (use-modules (tuile pr))
;; (ppre (prime-numbers 10000))


;; *unspecified* exists already
;;(define unspecified (if #f #t))
;;(define uns unspecified)
(define (specified? obj) (not (unspecified? obj)))


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
(define (old-dir-glob dir pat)

  ;; Glob pattern to regexp.
  (define (glob->regexp pat)
    (if (string=? pat "*")
        ;; Plain star is a special case and matches both dirs and
        ;; files.
        "^.*$"
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
                       (cons (re-quote (make-string 1 char))
                             (loop (1+ i))))))
                  '()))
            (list "$"))))))

  (dir-glob-re dir (glob->regexp pat)))


(define (dir-glob dir pat)
  (filter (lambda (str) (fnmatch-for-glob pat str)) (dir-list dir)))


;; Glob directory with regexp.
;;
;;     (dir-glob-re "../foo" ".*[.](c|cc)")
;;
(define (dir-glob-re dir pat)
  ;;  (let ((rx (make-regexp pat)))
  ;;    (filter (lambda (x) (regexp-exec rx x)) (dir-list dir)))
  (let ((rx (re-comp pat)))
    (filter (lambda (x) (re-match rx x)) (dir-list dir))))


(define (dir-glob-with-path dir pat)
  (let ((files (dir-glob dir pat)))
    (map (lambda (file) (string-append dir "/" file)) files)))

;; Return filename suffix (without the dot).
(define (extname filename)
  (car (last-pair (string-split filename #\.))))

;; Convert filename to absolute, and canonicalize it (as in Emacs).
(define (expand-file-name filename)
  (if (eq? (string-ref filename 0) #\~)
      (string-append (getenv "HOME") (substring filename 1))
      (canonicalize-path filename)))

(define (is-file? name)
  (eq? (stat:type (stat name)) 'regular))

(define (is-directory? name)
  (eq? (stat:type (stat name)) 'directory))


(define (file-modify-time file)
  (stat:mtime (stat file)))

;; Compare modify time of source(s) to target(s). If any of the
;; sources is newer than any of the targets or a target is missing,
;; return true.
(define (file-update? sources targets)

  (define (file-time file)
    (if (not (file-exists? file))
        0
        (file-modify-time file)))

  (let ((source-times (map file-time (listify sources)))
        (target-times (map file-time (listify targets))))
    (> (apply max source-times)
       (apply min target-times))))

;; Compare modify time of base to ref (+rest).
(define (file-newer? base ref-or-refs)
  (let ((base-time (file-modify-time base)))
    (let lp ((refs (listify ref-or-refs)))
      (if (pair? refs)
          (if (> base-time (file-modify-time (car refs)))
              #t
              (lp (cdr refs)))
          #f))))

;; Compare modify time of base to ref (+rest).
(define (file-older? base ref-or-refs)
  (let ((base-time (file-modify-time base)))
    (let lp ((refs (listify ref-or-refs)))
      (if (pair? refs)
          (if (< base-time (file-modify-time (car refs)))
              #t
              (lp (cdr refs)))
          #f))))


(define (datum->string datum)
  (with-output-to-string (lambda ()
                           (write datum))))
;; (define datum->string comp:datum->string)

;; Convert string to procedure.
(define (string->procedure str)
  (eval (read (open-input-string str)) (interaction-environment)))

;; Eval datum.
(define common-eval primitive-eval)

;; Return car of rest or backup if rest is not a pair.
(define (opt-arg rest backup)
  (if (pair? rest)
      (car rest)
      backup))


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
#;
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
#;
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
#;
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
#;
(define (re-split-old re str . options)
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
          substrings)))
  (error "TI: Use (tuile re)")
  )


;; Return true if regexp matches str.
#;
(define (re-match? re str)
;;  (regexp-match? (regexp-exec (make-regexp re) str))
  (error "TI: Use (tuile re)"))

;; Return regexp match str or false.
#;
(define (re-match re str)
;;  (aif (string-match re str)
;;       (match:substring it)
;;       #f)
  (error "TI: Use (tuile re)")
  )

;; Return regexp match string list or empty list.
#;
(define (re-matches re str)
;;  (map match:substring (list-matches re str))
  (error "TI: Use (tuile re)"))

;; Substitute regexp in string with replacement once.
#;
(define (re-sub re str rep)
;;  (aif (string-match re str)
;;       (regexp-substitute #f it 'pre rep 'post)
;;       str)
  (error "TI: Use (tuile re)"))

;; Substitute regexp in string with replacement globally.
#;
(define (re-gsub re str rep)
;;  (aif (string-match re str)
;;       (regexp-substitute/global #f re str 'pre rep 'post)
;;       str)
  (error "TI: Use (tuile re)"))


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


;; Create list from vector using the first "sel" items.
(define (vector-head->list vector sel)
  (let lp ((i (1- sel))
           (ret '()))
    (if (>= i 0)
        (lp (1- i)
            (cons (vector-ref vector i) ret))
        ret)))


;; Create assoc list from elements in a flat list. The list must
;; contain key-value pairs, i.e. the number of arguments must be even.
(define (alist . pairs)
  (if (let ((len (length pairs)))
        (and (> len 0)
             (even? len)))
      (let loop ((pairs pairs)
                 (ret '()))
        (if (pair? pairs)
            (loop (cddr pairs)
                  (cons (cons (car pairs) (cadr pairs))
                        ret))
            (reverse ret)))
      '()))

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


(define (assoc-ref-deep alist key . rest)
  (define (return res)
    (if res
        (cdr res)
        *unspecified*))
  (let ((keys (cons key rest)))
    (if (null? (cdr keys))
        (return (assoc (car keys) alist))
        (let lp ((alist alist)
                 (keys keys))
          (if (list? alist)
              (if (null? (cdr keys))
                  (return (assoc (car keys) alist))
                  (lp (return (assoc (car keys) alist))
                      (cdr keys)))
              *unspecified*)))))

(define (assoc-ref-single alist tag)
  (let ((val (assoc-ref alist tag)))
    (if (pair? val)
        (car val)
        *unspecified*)))

;; Hash table has key?
(define (hash-has-key? hsh key)
  (hash-get-handle hsh key))

;; Return list of hash table keys.
;; (define (hash-keys hsh)
;;   (hash-map->list (lambda (k v) k) hsh))

;; Return list of hash table keys.
;; (define (hash-values hsh)
;;   (hash-map->list (lambda (k v) v) hsh))


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


;; Process input from file, or return false is file access fails.
(define (with-input-from-file-if file proc)
  (with-exception-return-false
   (lambda ()
     (with-input-from-file file proc))))

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
    (with-output-to-port port proc)
    (close-output-port-with-filename port)))

(define (with-output-to-filename-if filename proc)
  (with-exception-return-false
   (lambda ()
     (with-output-to-filename filename proc))))

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


(define (file->code filename)
  (call-with-input-file filename
    (lambda (port)
      (let lp ((ret '()))
        (let ((datum (read port)))
            (if (not (eof-object? datum))
                (lp (cons datum ret))
                (reverse ret)))))))


(define (file-read-line filename)
  (car (file->lines filename #:with-newline #f)))

(define (file-write-line filename line)
  (with-output-to-file filename
    (lambda ()
      (display line)
      (newline))))

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

;; This old version hangs if the shell command only outputs stderr, but no stdout.
#;
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


(define (capture-shell-command-values command)
  (let* ((stdout-pipe (pipe))
         (stderr-pipe (pipe))
         (pid (primitive-fork)))

    (if (= pid 0)
        (begin
          ;; child
          ;; redirect stdout
          (dup2 (port->fdes (cdr stdout-pipe)) 1)
          ;; redirect stderr
          (dup2 (port->fdes (cdr stderr-pipe)) 2)
          (close-port (car stdout-pipe))
          (close-port (car stderr-pipe))
          (close-port (cdr stdout-pipe))
          (close-port (cdr stderr-pipe))
          (execl "/bin/sh" "sh" "-c" command))
        (begin
          ;; parent
          (close-port (cdr stdout-pipe))
          (close-port (cdr stderr-pipe))
          (let loop ((stdout "")
                     (stderr ""))
            (let ((fds (select (list (car stdout-pipe) (car stderr-pipe)) '() '() 1.0)))
              (match fds
                ((readable _ _)
                 (let ((new-out (if (member (car stdout-pipe) readable)
                                    (get-string-all (car stdout-pipe))
                                    ""))
                       (new-err (if (member (car stderr-pipe) readable)
                                    (get-string-all (car stderr-pipe))
                                    "")))
                   (if (and (string-null? new-out) (string-null? new-err))
                       (let ((status (waitpid pid)))
                         (close-port (car stdout-pipe))
                         (close-port (car stderr-pipe))
                         (values (cdr status) stdout stderr))
                       (loop (string-append stdout new-out)
                             (string-append stderr new-err)))))
                (_ ;; timeout or nothing
                 (loop stdout stderr)))))))))



;; Execute shell command and return responses as list.
;;
;; Responses: status-code stdout stderr
;;
(define (capture-shell-command cmd)
  (let-values (((status stdout stderr) (capture-shell-command-values cmd)))
    (list status stdout stderr)))


;; Execute shell command and return responses as assoc.
;;
;; Responses: status-code stdout stderr
;;
(define (capture-shell-command-alist cmd)
  (let-values (((status stdout stderr) (capture-shell-command-values cmd)))
    (list (cons 'status status)
          (cons 'stdout stdout)
          (cons 'stderr stderr))))


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


;; Return width and height of the terminal window as pair: '(<width> . <height>).
(define (terminal-dimensions)
  (let ((get-tput-attr (lambda (attr)
                         (string->number
                          (string-trim-right
                           (capture-shell-command-stdout
                            (si "tput #{attr}")))))))
    (cons (get-tput-attr "cols")
          (get-tput-attr "lines"))))

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
;;
;; Deprecated
#;
(define (find-first fn lst)
  (% (let loop ((tail lst))
       (if (pair? tail)
           (if (fn (car tail))
               (abort (lambda (k)       ; Default prompt handler
                                        ; requires a proc.
                        (car tail)))
               (loop (cdr tail)))
           #f))))
(define find-first find)

;; Find first matching item from lst.
(define (find-member item lst)
  (let ((res (member item lst)))
    (if res (car res) res)))


;; Terminate exception (with "handler" of one argument) if exception
;; is generated by "proc" (a thunk).
(define (with-exception-terminate handler proc)
  (with-exception-handler handler
    proc
    #:unwind? #t))


;; Terminate exception (with "handler" of one argument) if exception
;; is generated by "proc" (a thunk).
(define (with-exception-return-false proc)
  (with-exception-handler (lambda (exp) #f)
    proc
    #:unwind? #t))


(define (char-separator? char)
  (or (not char) (char-whitespace? char)))

(define (char-nonseparator? char)
  (or (not char) (not (char-whitespace? char))))

;; Check if string is either empty or has only whitespace characters?
(define (string-whitespace? str)
  (or (string-null? str)
      (let lp ((i 0))
        (if (< i (string-length str))
            (if (char-whitespace? (string-ref str i))
                (lp (1+ i))
                #f)
            #t))))

;; Return char or #f, if past string end.
(define (string-ref-safe s i)
  (if (>= i (string-length s))
      #f
      (string-ref s i)))

;; Return substring with possibly negative indeces.
;;
;;     (string-clip "foobar" 1 -2)
;;
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

;; Escape character "ch" from "str". Use optional argument as escape
;; character (default: #\\).
(define (string-escape str ch . rest)
  (let ((escape-char (if (pair? rest) (car rest) #\\)))
    (let loop ((chars (string->list str))
               (ret '()))
      (if (pair? chars)
          (if (or (char=? (car chars) ch)
                  (char=? (car chars) escape-char))
              (loop (cdr chars)
                    (append (list (car chars) escape-char)
                            ret))
              (loop (cdr chars)
                    (cons (car chars)
                          ret)))
          (list->string (reverse ret))))))


;; Remove escape character from "str". Use optional argument as escape
;; character (default: #\\).
(define (string-unscape str . rest)
  (let ((escape-char (if (pair? rest) (car rest) #\\)))
    (let loop ((chars (string->list str))
               (ret '()))
      (if (pair? chars)
          (if (char=? (car chars) escape-char)
              (loop (cdr chars)
                    ret)
              (loop (cdr chars)
                    (cons (car chars)
                          ret)))
          (list->string (reverse ret))))))


;; Split string ("str") using "ch", but at the same time unscape the
;; splitted string fields.
(define (string-split-unscape str ch . rest)
  (let ((escape-char (if (pair? rest) (car rest) #\\))
        (->word (lambda (word) (list->string (reverse word)))))
      (let loop ((chars (string->list str))
                 (word '())
                 (ret '()))
        (if (pair? chars)
            (cond
             ((char=? (car chars) escape-char)
              (loop (cddr chars)
                    (cons (cadr chars) word)
                    ret))
             ((char=? (car chars) ch)
              (loop (cdr chars)
                    '()
                    (cons (->word word) ret)))
             (else
              (loop (cdr chars)
                    (cons (car chars) word)
                    ret)))
            (reverse (cons (->word word) ret))))))


;; Split string with pattern. Optionally split only once (default:
;; false).
(define (string-split-with str pat . opt-once)
  (let ((pat-len (string-length pat))
        (once (opt-arg opt-once #f)))
    (cond
     (once (let ((pos (string-contains str pat)))
             (list (substring str 0 pos)
                   (substring str (+ pos pat-len)))))
     (else
      (let loop ((tail str)
                 (lst '()))
        (if (> (string-length tail)
               0)
            (let ((pos (string-contains tail pat)))
              (if pos
                  (loop (substring tail (+ pos pat-len))
                        (append lst (list (substring tail 0 pos))))
                  (append lst (list tail))))
            lst))))))


;; Split string as command line parameter definition, e.g. foo=bar.
(define (string-split-param str)
  (string-split-with str "=" #t))


;; Generate strings.
;;
;;     (* 3 (: 9 0))           ; Multiply whole
;;     (/ 10 (: 2 0))          ; Multiply chars
;;     (: a z)                 ; Range
;;     (: z a)                 ; Range
;;     (+ "a" (* 3 (: 9 0))    ; Concatenate
;;     (* 3 (: 0 9))           ; Multiply whole
;;     (- "foobar" "ob")       : Subtract
;;     (- "foobar" "kk")       ; Subtract
;;
(define (string-gen spec)

  (define vtab #f)

  (define (gen-multi spec)
    (let ((mul (second spec))
          (str (third spec)))
      (string-concatenate (make-list mul str))))

  (define (gen-repeat spec)
    (let ((mul (second spec))
          (str (third spec)))
      (string-concatenate (map list->string
                               (map (lambda (i) (make-list mul i))
                                    (string->list str))))))

  (define (gen-concat spec)
    (string-concatenate (map gen-item (cdr spec))))

  (define (gen-remove spec)
    (let ((str (second spec))
          (rem (third spec)))
      (aif (string-contains str rem)
           (string-append (substring str 0 it)
                          (substring str (+ it (string-length rem))))
           str)))

  (define (gen-range spec)

    (define (number->charval n)
      (+ (char->integer #\0) n))

    (define (symbol->charval n)
      (char->integer (string-ref (symbol->string n) 0)))

    (define (gen-up ll rl)
      (apply string (map integer->char (iota (1+ (- rl ll)) ll))))

    (define (gen-down ll rl)
      (apply string (map integer->char (iota (1+ (- ll rl)) ll -1))))

    (define (gen ll rl)
      (cond
       ((<= ll rl) (gen-up ll rl))
       (else (gen-down ll rl))))

    (match spec
      ((? (lambda (spec) (symbol? (second spec))) (#{:}# ll rl))
       (gen (symbol->charval ll) (symbol->charval rl)))
      ((#{:}# ll rl)
       (gen (number->charval ll) (number->charval rl)))
      (else "")))

  (define (gen-head spec)
    (string-take (gen-item (third spec)) (second spec)))

  (define (gen-tail spec)
    (string-take-right (gen-item (third spec)) (second spec)))

  (define (gen-clip spec)
    (substring (gen-item (l3 spec)) (l1 spec) (l2 spec)))

  (define (gen-drop spec)
    (let* ((base (l3 spec))
           (left (string-take base (l1 spec)))
           (right (substring base (l2 spec))))
      (string-append left right)))

  (define (gen-vset spec)
    (when (not vtab)
      (set! vtab (make-hash-table)))
    (let ((value (gen-item (l2 spec))))
      (hash-set! vtab (l1 spec) value)
      value))

  (define (gen-vget spec)
    (if vtab
        (hash-ref vtab (l1 spec))
        ""))

  (define (gen-item spec)
    (if (list? spec)
        (case (car spec)
          ((*) (gen-multi (list (first spec)
                                (second spec)
                                (gen-item (third spec)))))
          ((/) (gen-repeat (list (first spec)
                                 (second spec)
                                 (gen-item (third spec)))))
          ((+) (gen-concat spec))
          ((-) (gen-remove spec))
          ((#{:}#) (gen-range spec))
          ((<) (gen-head spec))
          ((>) (gen-tail spec))
          ((^) (gen-clip spec))
          ((_) (gen-drop spec))
          ((!) (gen-vset spec))
          ((=) (gen-vget spec))
          )
        spec))

  (gen-item spec))

;; Create list of string from symbols, i.e. non-quoted text.
;;
;;     (make-string-list foo bar dii)
;;
(define-syntax make-string-list
  (lambda (x)
    (let* ((stx (syntax->datum x))
           (-> datum->syntax))
      #`(map symbol->string (quote #,(-> x (cdr stx)))))))


;; Create sequence of numbers from start by length.
(define (number-sequence start len . rest)
  (let ((step (if (pair? rest) (car rest) 1)))
    (let loop ((num start))
      (if (< num (+ len start))
          (cons num (loop (+ num step)))
          '()))))


;; Create sequence of numbers from start to end (exclusive).
(define (number-range start end . rest)
  (let ((step (if (pair? rest) (car rest) 1)))
    (if (< start end)
        (let loop ((num start))
          (if (< num end)
              (cons num (loop (+ num step)))
              '()))
        (let loop ((num start))
          (if (> num end)
              (cons num (loop (- num step)))
              '())))))


;; Create sequence of numbers from start to end (inclusive).
(define (number-span start end . rest)
  (let ((step (if (pair? rest) (car rest) 1)))
    (if (< start end)
        (let loop ((num start))
          (if (<= num end)
              (cons num (loop (+ num step)))
              '()))
        (let loop ((num start))
          (if (>= num end)
              (cons num (loop (- num step)))
              '())))))


;; Create progress bar for "done" of "todo" (the total work).
;;
;; Use "display" to print out the result, in order to show the
;; progress on the same line as animation.
;;
;;     [++++++++-------] 50%
;;
(define* (progress-bar done todo #:key (terminal-width 72) (gap-right 2) (gap-left 2))
  (define (->int v) (inexact->exact (ceiling v)))
  (define (->flo v) (exact->inexact v))
  (let* ((non-bar-width (+ 7 gap-right gap-left))
         (width (- terminal-width non-bar-width))
         (done-int (if (inexact? done) (->int done) done))
         (todo-int (if (inexact? todo) (->int todo) todo))
         (done-pct (quotient (* 100 done-int) todo-int))
         (done-len (quotient (* width done-pct) 100))
         (todo-len (- width done-len))
         (done-str (make-string done-len #\+))
         (todo-str (make-string todo-len #\-)))
    ;; (string-append "\r" (:in gap-left) (:rj 3 " " done-pct) "% " "[" done-str todo-str "]" )
    (string-append "\r" (fmt 'ind gap-left) (fmt 'ral 3 done-pct) "% " "[" done-str todo-str "]" )
    ))

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

;;(use-modules (tuile pr))
;;(define done 30)
;;(define todo 100)
;;;;(pr (progress-bar done todo #:terminal-width (car (terminal-dimensions)) #:gap-right 6 #:gap-left 6))
;;(pr (progress-bar done todo #:terminal-width (car (terminal-dimensions))))

(define (with-file-or-fail filename proc)
  (if (file-exists? filename)
      (proc filename)
      (raise-exception &error)))

;; Create a temporary file by name. User can select the temp file
;; directory with opts (default: /dev/shm).
(define (make-temporary-file name . opts)
  (let* ((dir (opt-arg opts "/dev/shm"))
         (port (mkstemp (ss dir "/" name "-XXXXXX"))))
    (chmod port (logand #o777 (lognot (umask))))
    (let ((temp-filename (port-filename port)))
      (close port)
      temp-filename)))

;; Create function with memoization.
;;
;; param:
;;       fn-name       Function name.
;;       fn-args       Function arguments.
;;       fn-body       Function body.
;;
;;
;;     (define-with-memoize (get-files config)
;;       (assoc-ref config 'files))
;;
;;     =>
;;
;;     (define get-files
;;       (let ((memoize-var #f))
;;         (lambda (config)
;;           (if memoize-var
;;               memoize-var
;;               (begin
;;                 (set! memoize-var (assoc-ref config 'files))
;;                 memoize-var)))))
;;
(define-syntax define-with-memoize
  (lambda (x)
    (syntax-case x ()
      ((_ (fn-name fn-args ...) fn-body)
       (with-syntax ((memoize-var (datum->syntax x 'memoize-var)))
         #`(define fn-name
             (let ((memoize-var #f))
               (lambda (fn-args ...)
                 (if memoize-var
                     memoize-var
                     (begin
                       (set! memoize-var ((lambda (fn-args ...) fn-body)
                                          fn-args ...))
                       memoize-var))))))))))


(define (seconds->minutes:seconds seconds)
  (let ((mins (quotient seconds 60))
        (secs (remainder seconds 60)))
    (fmt `(ral (2 "0") ,mins) ":" `(ral (2 "0") ,secs))))
;;
;; (use-modules (tuile pr))
;; (pr (seconds->minutes:seconds 100))

;; (use-modules (tuile pr))
;; (when #t
;;   (pr (string-gen `(/ 10 (: 2 0))))
;;   (pr (string-gen `(* 3 (: 9 0))))
;;   (pr (string-gen `(: a z)))
;;   (pr (string-gen `(: z a)))
;;   (pr (string-gen `(+ "nums: " (* 3 (: 9 0)))))
;;   (pr (string-gen `(* 3 (: 0 9))))
;;   (pr (string-gen `(- "foobar" "ob")))
;;   (pr (string-gen `(- "foobar" "kk")))
;;   (pr (string-gen `(< 3 "foobar")))
;;   (pr (string-gen `(> 3 "foobar")))
;;   (pr (string-gen `(^ 2 4 "foobar")))
;;   (pr (string-gen `(_ 2 4 "foobar")))
;;   (pr (string-gen `(+ (! a "dii") (= a))))
;;   )
