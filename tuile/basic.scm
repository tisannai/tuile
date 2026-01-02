(define-module (tuile basic)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module ((ice-9 match) #:select (match))

  #:export
  (
   aif
   awhen

   uif
   uwhen

   any?
   empty?
   len-0?
   len-1?
   len-2?
   len-3?
   lr
   lr0
   lr1
   lr2
   lr3
   lr4
   lr5
   lr6
   lr7
   lr8
   lr9
   flatten
   flatten-0
   flatten-1
   delete-nth
   listify
   list->cons
   cons->list
   list-join
   list-clean
   list-specified
   list-split
   list-split-tail
   list-slice
   list-range
   list-pick
   list-compact
   list-randomize
   list-update
   list-update!
   make-list-stack
   list-push!
   list-pop!
   map-compact
   clean-list
   list-copy-deep


   command-line-arguments

   dir-list

   common-eval
   opt-arg

   string-gen
   make-string-list

   ))


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

;; Anaphoric when macro.
;;
;;     (awhen (1+ i)
;;          it)
;; (define-syntax awhen
;;   (lambda (x)
;;     ;; "lst": Convert syntax to datum and convert back to a list of syntax.
;;     (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
;;       ;; Create template variable "it".
;;       (with-syntax ((it (datum->syntax x 'it)))
;;         #`(let ((it #,(cadr lst)))
;;             (when it #,(caddr lst)))))))

(define-syntax awhen
  (lambda (x)
    (syntax-case x ()
      ((_ test then ...)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it test))
             (when it then ...)))))))


;; Anaphoric if macro for *unspecified*.
(define-syntax uif
  (lambda (x)
    ;; "lst": Convert syntax to datum and convert back to a list of syntax.
    (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
      ;; Create template variable "it".
      (with-syntax ((it (datum->syntax x 'it)))
        #`(let ((it #,(cadr lst)))
            (if (not (unspecified? it)) #,(caddr lst) #,(cadddr lst)))))))

;; Anaphoric when macro for *unspecified*.
;;
;;     (uwhen (1+ i)
;;          it)

;; (define-syntax uwhen
;;   (lambda (x)
;;     ;; "lst": Convert syntax to datum and convert back to a list of syntax.
;;     (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
;;       ;; Create template variable "it".
;;       (with-syntax ((it (datum->syntax x 'it)))
;;         #`(let ((it #,(cadr lst)))
;;             (when (not (unspecified? it)) #,(caddr lst)))))))

(define-syntax uwhen
  (lambda (x)
    (syntax-case x ()
      ((_ test then ...)
       (with-syntax ((it (datum->syntax x 'it)))
         #'(let ((it test))
             (when (not (unspecified? it)) then ...)))))))

(define any?   pair?)
(define empty? null?)
(define len-0? null?)
(define len-1? (lambda (lst) (and (pair? lst) (null? (cdr lst)))))
(define len-2? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (null? (cddr lst)))))
(define len-3? (lambda (lst) (and (pair? lst) (pair? (cdr lst)) (pair? (cddr lst)) (null? (cdddr lst)))))

(define lr list-ref)

(define lr0 first)
(define lr1 second)
(define lr2 third)
(define lr3 fourth)
(define lr4 fifth)

;; Create function definitions (by x amount) using list-ref as:
;;
;;     (define (lr0 lst) (list-ref lst 0))
;;     (define (lr1 lst) (list-ref lst 1))
;;     ...
;;
(define-syntax expand-short-list-ref-index-functions
  (lambda (x)
    (define (get-list-ref-function index)
      (list 'define
            (list (string->symbol (string-append "lr" (number->string index))) 'lst)
            (list 'list-ref 'lst index)))
    (let* ((-> datum->syntax))
      #`(begin #,@(-> x (map get-list-ref-function (iota (cadr (syntax->datum x)) 5)))))))

(expand-short-list-ref-index-functions 5)


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
#;
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


;; Split list to head and tail, and return list parts as pair, where
;; tail length matches count.
(define (list-split-tail lst count)
  (let lp ((lst (reverse lst))
           (tail '())
           (count count))
    (if (and (pair? lst)
             (> count 0))
        (lp (cdr lst)
            (cons (car lst) tail)
            (1- count))
        (cons (reverse lst)
              tail))))


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


;; Push to item to front of list. Empty list should be defined as:
;; (list *unspecified*).
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


;; Copy list, recursively.
(define (list-copy-deep lst)
  (cond
    ((pair? lst)
     (map list-copy-deep lst))
    ((vector? lst)
     (list->vector (map list-copy-deep (vector->list lst))))
    (else lst)))


;; Return command line arguments, excluding the executable.
(define (command-line-arguments)
  (cdr (command-line)))


;; List given directory entries without the dot files.
(define (dir-list dir)
  (list-tail (scandir dir) 2))


;; Eval datum.
(define common-eval primitive-eval)

;; Return car of rest or backup if rest is not a pair.
(define (opt-arg rest backup)
  (if (pair? rest)
      (car rest)
      backup))


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
    (substring (gen-item (lr3 spec)) (lr1 spec) (lr2 spec)))

  (define (gen-drop spec)
    (let* ((base (lr3 spec))
           (left (string-take base (lr1 spec)))
           (right (substring base (lr2 spec))))
      (string-append left right)))

  (define (gen-vset spec)
    (when (not vtab)
      (set! vtab (make-hash-table)))
    (let ((value (gen-item (lr2 spec))))
      (hash-set! vtab (lr1 spec) value)
      value))

  (define (gen-vget spec)
    (if vtab
        (hash-ref vtab (lr1 spec))
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
