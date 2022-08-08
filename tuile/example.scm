(define-module (tuile example)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (tuile pr)
  #:use-module (tuile utils)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-43) #:select (vector-append))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  )


;; Missing:
;;
;; * ...
;;


;; ------------------------------------------------------------
;; # Basics:

;   This is a comment
;;  This is a comment
;;; This is a comment

;; This is integer 12.
12

;; This is symbol 'bar'.
'bar

;; This is string 'bar'.
"bar"

;; This is a immutable list.
'(12 foo)                               ; Short syntax for quotation.
(quote (12 foo))                        ; Verbose syntax for quotation.

;; This is a mutable list.
(list 12 'foo)                          ; 'foo' requires quoting.

;; Procedure ('+') application (call), i.e. add two numbers.
(+ 1 2)

;; Procedure ('list') application, i.e. create list.
(list 12 'foo)

;; This is anonymous function ('proc') with two arguments, i.e. a
;; procedure.
(lambda (a b) (+ a b))

;; Apply the 'proc' with two arguments.
;;     proc                     arg1    arg2
(      (lambda (a b) (+ a b))   1       2)

;; Define 'foo' to be value of: 12
(define foo 12)

;; Define 'foo' as procedure, 'proc'.
(define foo (lambda (a b) (+ a b)))

;; Define 'foo' as procedure without 'proc'.
(define (foo a b) (+ a b))

;; Apply 'foo'.
(foo 1 2)

;; Apply 'foo' with arguments in a list.
(apply foo '(1 2))

;; Define 'bar' with one or more arguments to sum.
(define (bar a . rest)
  (apply + (cons a rest)))

;; Apply 'bar' with different number of arguments.
(bar 1)
(bar 1 2)
(bar 1 2 3)
(apply bar '(1 2 3 4))

;; # Basics:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; # Datatypes:

;; ------------------------------------------------------------
;; ## Boolean:

;; ------------------------------------------------------------
;; ### Literal:
#t
#f

;; ------------------------------------------------------------
;; ### Type predicate:
(boolean? #f)
(boolean? #t)
(boolean? '()) ; #f

;; ------------------------------------------------------------
;; ### Operation:
(and #t #t)
(or #t #f)
(not #f)
(eq? #t #t)

;; ## Boolean:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Number:

;; ------------------------------------------------------------
;; ### Literal:
123
#x123
#o123
#b1010
12.3
1/23

;; ------------------------------------------------------------
;; ### Type predicate:
(number? 1)
(complex? 1)
(real? 1)
(rational? 1)
(integer? 1)
(exact-integer? 1)
(inexact? 0.323)
(zero? 0.0)
(positive? 1)
(negative? -1)
(odd? 1)
(even? 2)

;; ------------------------------------------------------------
;; ### Operation:
(+ 2)
(- 2)
(+ 2 3 4)
(- 2 3 4)
(* 2 3 4)
(/ 2 3 4)
(* 12.3 3)
(/ 12.3 3)
(quotient  2 3)
(remainder 2 3)
(abs -1.23)
(max 2 3 4)
(min 2 3 4)
(floor 23.4)
(ceiling 23.4)
(round 23.4)
(sqrt 3)
(expt 2 3)
(log 3)
(log10 100)
(sin 2.3)
(cos 2.3)
(tan 2.3)
(asin 2.3)
(acos 2.3)
(atan 2.3)
(exp 1)
(logand #b1010 #b1000)
(logior #b1010 #b1000)
(logxor #b1010 #b1000)
(lognot #b1010)
(logbit? 1 #b1010)
(integer-length 15) ; 4
(integer-length 16) ; 5
(random 10)
(random 1.0)


;; ------------------------------------------------------------
;; ### Comparison:
(= 2 3)
(< 2 3)
(> 3 2)
(<= 2 3)
(>= 3 2)


;; ------------------------------------------------------------
;; ## String:

(string? "foo")
(string #\f #\o #\o)
(make-string 4 #\ )
(string-length "foo")
(substring "foo" 0 2)
(string-ref "foo" 0)
(string-append "foo" "bar")
(string-concatenate '("foo" "bar"))
(string-join '("foo" "bar") "-")

(string->list "foo")
(list->string '(#\f #\o #\o))
(string-upcase "foo")
(string-downcase "FOO")
(string-titlecase "foo")
(string-split "foo-bar" #\-)
(string-tokenize "foo-bar" char-set:letter)
(string-trim-both "foo ")
(string-contains "bar" "a")


;; ## String:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Regexp:

(string-match "[0-9]+" "foo123")
(make-regexp "[0-9]+")
(regexp-exec (make-regexp "[0-9]+")
             "foo123")
(regexp? (make-regexp "[0-9]+"))
(match:substring (string-match "[0-9]+" "foo123"))
(match:start (string-match "[0-9]+" "foo123"))
(match:end (string-match "[0-9]+" "foo123"))
(match:prefix (string-match "[0-9]+" "foo123"))
(match:string (string-match "[0-9]+" "foo123"))
(map match:substring (list-matches "[0-9]" "foo123"))

;; ## Regexp:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; ## List:

(list? '())
(list? '(1 2 3))
(null? '())
(pair? '(1))
'(1 . 2)
(cons 1 2)
(cons 1 '(2))
'(1 2 3)
(list 1 2 3)
(make-list 3 #f)
(car '(1 2 3))
(cdr '(1 2 3))
(car '(1 . 2))
(cdr '(1 . 2))
(length '(1 2 3))
(list-ref '(1 2 3) 2)
(list-tail '(1 2 3) 2)
(define l (list 1 2 3))
(list-set! l 0 4)
(cons 4 '(1 2 3))
(append '(4) '(1 2 3))
(reverse '(1 2 3))
(map 1+ '(1 2 3))
(fold + 0 '(1 2 3))
(fold       cons '() '(1 2 3))
(fold-right cons '() '(1 2 3))
(filter odd? '(1 2 3))
(delete 1 '(1 2 3 1))
(remove odd? '(1 2 3 1))
(sort '(1 2 3 1) >)
(member 2 '(1 2 3 1)) ; equal?
(memq 2 '(1 2 3 1))   ; eq?
(memv 2 '(1 2 3 1))   ; eqv?
(find even? '(1 2 3 1))
(find odd? '(1 2 3 1))
(find-tail (lambda (i)
             (> i 1))
           '(1 2 3 1))
(first  '(1 2 3 1))
(second '(1 2 3 1))
(third  '(1 2 3 1))
(last   '(1 2 3))
(last-pair '(1 2 3))
(take '(1 2 3) 1)
(take-right '(1 2 3) 1)
(drop '(1 2 3) 1)
(drop-right '(1 2 3) 1)
(define p (list 1 2 3))
(set-car! p 4)
p
(define p (list 1 2 3))
(set-cdr! p '(4))
p

;; ## List:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Vector:

(vector? #())
(vector? #(1 2 3))
(make-vector 3 #f)
(vector 1 2 3)
(make-vector 3 #f)
(vector-length #(1 2 3))
(vector-ref #(1 2 3) 2)
(define v (vector 1 2 3))
(vector-set! v 0 4)
(vector-append #(4) #(1 2 3))

;; ## Vector:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Hash:

(define h (make-hash-table))
(hash-table? h)
(hash-set! h 'foo 'bar)
(hash-ref h 'foo)
(hash-set! h 'jii 'haa)
(hash-ref h 'jii)
(hash-remove! h 'jii)
(hash-ref h 'jii)

;; ## Hash:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Record:

(define-record-type address
  (make-address name street house)
  address?
  (name     address-name)
  (street   address-street  set-address-street!)
  (house    address-house   set-address-house!)
  )

(define a (make-address "foo" "bar" 12))
(address-street a)
(set-address-street! a "dii")

;; ## Record:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Conversion:

(number->string 23)
(string->number "23")
(number->string #x23)
(string->number "23" 16)


;; ## Conversion:
;; ------------------------------------------------------------


;; # Datatypes:
;; ------------------------------------------------------------




;; ------------------------------------------------------------
;; # Class:

;; ------------------------------------------------------------
;; ## Guile class:

;; NOTE: Comprehensive GOOPS examples in vlogmod.scm.

;; counter-1 class (object)
;;
;;            name      inheritance list
(define-class <counter-1> ()
  ;; Slot list.
  (value #:init-keyword #:value         ; Slot called "value", with
                                        ; initialization keyword
                                        ; "#:value".

         #:init-form 0)                 ; Initial value when not
                                        ; explicitly set.
  (step  #:init-keyword #:step
         #:init-form 1))


;; Increment method for <counter> with optional step-size argument.
(define-method (incr (this <counter-1>) . rest)
  (let ((step (if (pair? rest)
                  (car rest)
                  (slot-ref this 'step))))
    ;; "slot-set!" is used to give slots new values.
    ;;
    ;;         obj  slot (as symbol)
    ;;                        slot value reference
    (slot-set! this 'value (+ (slot-ref this 'value)
                              step))))

(define-method (init (this <counter-1>))
  (slot-set! this 'value 0))

(define-method (get-value (this <counter-1>))
  (slot-ref this 'value))

;; Create counter.
;;
;;         make class     keyword value (for "value").
(define c (make <counter-1> #:value 1))

;; Display the complete object.
(describe c)

;; Get initial value of counter.
(get-value c)

;; Increment with default increment.
(incr c)

;; Increment with 2.
(incr c)

(get-value c)

;; Reset counter.
(init c)


;; ## Guile class:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Custom class:

(define-this-class <counter-2> ()
  (value 0)
  (step 1))

(define-this-method <counter-2> (incr . rest)
  (let ((step (if (pair? rest)
                  (car rest)
                  (this-ref step))))
    (this-set! value (+ (slot-ref this 'value)
                        step))))

(define-this-method <counter-2> (init)
  (this-set! value 0))

(define-this-method <counter-2> (get-value)
  (this-ref value))


;; Create counter.
;;
;;         make class     keyword value (for "value").
(define c (make <counter-2> #:value 1))

;; Display the complete object.
(describe c)

;; Get initial value of counter.
(get-value c)

;; Increment with default increment.
(incr c)

;; Increment with 2.
(incr c)

(get-value c)

;; Init counter.
(init c)

;; ## Custom class:
;; ------------------------------------------------------------

;; # Class:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; # Pattern matching:

(let ((l '(hello ("you"))))
  (match l
    (('hello (who))
     who)))

(let ((rest '(12 3 (foo bar dii))))
  (match rest
    (((? number? x) (? number? y))
     (values x y #f))
    (((? number? x) (? number? y) (opts ...))
     (values x y opts))
    (((opts ...))
     (values #f #f opts))
    (else (values #f #f #f))))

;; # Pattern matching:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; # Control flow:

;; ------------------------------------------------------------
;; ## Structure:

(begin
  1
  2
  3)

(if #t
    #t
    #f)

(when #f ; Returns unspecified
  #t)

(cond
 ([= 1 1]
  #t)
 (else #f))

(case 'foo
  ((foo bar) 'bar)
  (else 'dii))

(let 10-times-2 ((i 0)  ; Named-let (recursion)
                 (sum 0))
  (if (< i 10)
      (10-times-2 (1+ i)
                  (+ sum 2))
      sum))

;; 10-times-2 as "do"
;;
;;    var init  step
(do ((sum 0     (+ sum 2)) ;
     (i   0     (1+ i)))
    ((= i 10) sum)         ; exit condition and result value
  #t)                      ; body



;; ## Structure:
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; ## Multiple values:

(define-values (a b) (values 1 3))
(call-with-values (lambda () (values 1 3))
  (lambda (a b) (+ a b)))


;; ## Multiple values:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Continuation:

(call/cc (lambda (cc)
           (let find-number-1 ((tail '(3 1 2)))
             (if (pair? tail)
                 (if (= (car tail) 1)
                     (cc (car tail))
                     (find-number-1 (cdr tail)))))))

;; ## Continuation:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Prompt:

(use-modules (ice-9 control))
(% (let find-number-1 ((tail '(3 1 2)))
     (if (pair? tail)
         (if (= (car tail) 1)
             (abort (lambda (k) (car tail)))
             (find-number-1 (cdr tail))))))

;; ## Prompt:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; ## Exception:

(define exn-handler (lambda (exn) #t))
(with-exception-handler exn-handler
  (lambda ()
    (raise-exception &error)
    (+ 1 2))
  #:unwind? #t)

;; ## Exception:
;; ------------------------------------------------------------


;; # Control flow:
;; ------------------------------------------------------------




;; ------------------------------------------------------------
;; # Macros:

;; define-syntax-rule macro:
;;
;;     (create-string-list foo bar dii)
;;     ->
;;     ("foo" "bar" "dii")
(define-syntax-rule (create-string-list id ...)
  (map symbol->string (syntax->datum #'(id ...))))

;; define-syntax macro with syntax-case:
;;
;;     (create-string-list foo bar dii)
;;     ->
;;     ("foo" "bar" "dii")
(define-syntax create-string-list
  (lambda (x)
    (syntax-case x ()
      ((_ id ...)
       #'(map symbol->string (syntax->datum #'(id ...)))))))

;; define-syntax macro using procedular function:
;;
;;     (create-string-list foo bar dii)
;;     ->
;;     ("foo" "bar" "dii")
(define-syntax create-string-list
  (lambda (x)
    #`(map symbol->string (quote #,(datum->syntax x (cdr (syntax->datum x)))))))

;; Anaphoric if macro with syntax-case:
;;
;;     (aif (1+ i)
;;          i
;;          #f)
(define-syntax aif
  (lambda (x)
    (syntax-case x ()
      ((_ test then else)
       (with-syntax ((it (datum->syntax x 'it))) ; Create new variable "it"
         #'(let ((it test))
             (if it then else)))))))

;; Anaphoric if macro as procedular:
;;
;;     (aif (1+ i)
;;          i
;;          #f)
(define-syntax aif
  (lambda (x)
    (let ((lst (map (lambda (i) (datum->syntax x i)) (syntax->datum x))))
      (with-syntax ((it (datum->syntax x 'it))) ; Create new variable "it"
        #`(let ((it #,(cadr lst)))
            (if it #,(caddr lst) #,(cadddr lst)))))))


;; # Macros:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; # Input and Output:

(use-modules ((ice-9 rdelim) #:select (read-line)))

;; ------------------------------------------------------------
;; ## Input and Output to port:

;; Explicit open and close (not usually needed).
(let ((port (open-output-file "foo.txt")))
  (display "example\n" port)
  (close-port port))

;; Explicit open and close (not usually needed).
(let* ((port (open-input-file "foo.txt"))
       (line (read-line port)))
  (close-port port)
  line)

;; Output to default port.
(let ((port (open-output-file "foo.txt")))
  (with-output-to-port port
    (lambda ()
      (display "example\n")))
  (close-port port))

;; Typical use with user proc.
(call-with-output-file "foo.txt"
  (lambda (port)
    (display "example\n" port)))

;; Write list of "lines" to file adding newline after each line.
(define lines '("foo" "bar" "hii"))
(call-with-output-file "foo.txt"
  (lambda (port)
    (for-each (lambda (line)
                (display line port)
                (newline port))
              lines)))

;; Read list of "lines" from file removing newline from each line.
(call-with-input-file "foo.txt"
  (lambda (port)
    (let read-all ()
      (let ((line (read-line port)))
        (if (eof-object? line) '()
            (cons line (read-all)))))))

;; Write to default port.
(with-output-to-file "foo.txt"
  (lambda ()
    (for-each (lambda (line)
                (display line)
                (newline))
              lines)))

;; Read from default port.
(with-input-from-file "foo.txt"
  (lambda ()
    (let read-all ()
      (let ((line (read-line)))
        (if (eof-object? line) '()
            (cons line (read-all)))))))


;; ## Input and Output to port:
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; ## Input and Output to string:

(call-with-output-string (lambda (port) (display "example\n" port)))
(call-with-input-string "example\n" (lambda (port) (read-line port)))

(with-output-to-string (lambda () (display "example\n")))
(with-input-from-string "example\n" (lambda () (read-line)))

;; ## Input and Output to string:
;; ------------------------------------------------------------


;; # Input and Output:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; # Module:

(define-module (foo)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:export
  (
   foo-func
   ))

(define (foo-func)
  (with-input-from-string "foo\n" (lambda () (read-line))))

(use-modules (foo))

(define-module (bar)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:export
  (
   bar-func
   ))

(define (bar-func)
  (with-input-from-string "bar\n" (lambda () (read-line))))

(use-modules (bar))

;; Guile internals example: module reflection.

(define pr (resolve-module '(tuile pr)))
;; Print all module bindings, i.e. obarray keys.
(pd (hash-map->list (lambda (k v) k) (module-obarray dbg)))


;; # Module:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; # Eval:

(define (eval-with-x prog a b)
  (let (
        ;; Variable "a" inserted into a mutable list.
        (at-a (eval (list 'let (list (list 'x a)) prog) (interaction-environment)))
        ;; Variable "b" inserted into quasiquoted list.
        (at-b (eval `(let ((x ,b)) ,prog) (interaction-environment))))
    (- at-b at-a)))

(pr (eval-with-x '(expt 2 x) 3 5))

;; # Eval:
;; ------------------------------------------------------------
