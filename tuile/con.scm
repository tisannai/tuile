;; CON: configuration format interpreter (for Scheme).

;; Configuration processing stages:
;;
;; * Read in "con" file and tokenize text (with Gulex).
;;
;; * Parser detects grammar, checks that valid tokens are seen and
;;   create an AST.
;;
;; * Elaboration is performed through AST and it creates variable
;;   scopes for definitions and references. Variable are stored to
;;   slots.
;;
;; * Elaboration knows type of each atom. Primitive functions have
;;   fixed input type(s) to output type mapping, i.e. we can check all
;;   types at elaboration stage. We do type inference. Types are not
;;   checked at evaluation stage
;;
;; * Functions have predefined number of arguments. We check at
;;   elaboration time that function is taking the correct amount of
;;   arguments.
;;


(define-module (tuile con)
  #:use-module ((rnrs records syntactic) #:select (define-record-type))
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
  #:use-module ((ice-9 textual-ports) #:select (get-string-all))
  #:use-module ((ice-9 eval-string) #:select (eval-string))
  #:use-module ((ice-9 popen) #:select (open-input-pipe close-pipe))
  #:use-module ((srfi srfi-1) #:select (first second third fourth drop drop-right last fold))
  #:use-module (tuile pr)
  #:use-module (tuile gulex)
  #:use-module (tuile dbg)
  #:use-module ((tuile con-core) #:prefix core.)
  #:export
  (
   con-file
   con-text
   ))



;; ------------------------------------------------------------
;; ERRORS:

(define (con-internal-error msg)
  (pr "con INTERNAL ERROR: " msg)
  (exit 1))

(define (con-error-info tok msg)
  (pr "con ERROR: In file \"" (token-file tok) "\", on line: " (token-line tok) "\n    " msg)
  1)

(define (con-error tok msg)
  (pr "con ERROR: In file \"" (token-file tok) "\", on line: " (token-line tok) "\n    " msg)
  (exit 1))

(define (con-exit-from-file file msg)
  (pr "con EXIT: File \"" file "\":\n    " msg)
  (exit 1))

;; ERRORS:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; SCOPE:

(define-record-type scope
  (fields level               ; Scope level (top is 0).
          varhsh              ; Hash of variables (elaboration phase).
          (mutable count)     ; Count of registered variables.
          (mutable vars)      ; Locations for variable values.
          next                ; Link to next higher scope.
          ))


(define (scope-create . parent)
  (if (pair? parent)
      (let ((p (car parent)))
        (make-scope (1+ (scope-level p))
                    (make-hash-table)
                    0
                    #f
                    p))
      (make-scope 0
                  (make-hash-table)
                  ;; Fake an existing self-call with 1.
                  1
                  #f
                  #f)))

;; Add variable to scope (variable is varloc/no-elab at this stage).
;;
;; Return: <var index>
;;
(define (scope-elab-register-varloc scope varloc token)
  (if (hash-get-handle (scope-varhsh scope) (varloc-id varloc))
      (con-error token (ss "Variable name already used: \"" (varloc-id varloc) "\""))
      (let ((index (scope-count scope)))
        (hash-set! (scope-varhsh scope) (varloc-id varloc) (cons index varloc))
        (scope-count-set! scope (1+ (scope-count scope)))
        index)))

;; Return variable level-index or #f (for failure).
(define (scope-elab-get-var-index scope varname)
  ;; Start with top scope and travel deeper in scope hierarchy until
  ;; none available.
  (let loop ((scope scope)
             (level 0))
    (if (hash-get-handle (scope-varhsh scope) varname)
        (cons level (car (hash-ref (scope-varhsh scope) varname)))
        (if (scope-next scope)
            (loop (scope-next scope)
                  (1+ level))
            #f))))

;; Return variable value at index.
(define (scope-get-var-at-index scope index)
  (vector-ref (scope-vars scope) index))

;; Set variable value at index.
(define (scope-set-var-at-index scope index value)
  (vector-set! (scope-vars scope) index value))

;; Convert list of vars to varvec, in the scope.
(define (scope-finalize scope)
  (when (not (scope-vars scope))
    ;; Just create an empty vector for variable slots. They will be
    ;; filled when variable definitions are evaluted.
    (scope-vars-set! scope (make-vector (scope-count scope)))))


;; SCOPE:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; VARIABLES:

;; Variable location.
(define-record-type varloc
  (fields (mutable level)               ; Scope level.
          (mutable index)               ; Index to varvec.
          id                            ; Var id (string), before elab.
          (mutable scope)               ; Scope of var.
          ))


;; Create an (almost) empty varloc with just id (tag).
(define (varloc-create id)
  (make-varloc #f #f id #f))


;; Fill in the missing fields after "varloc-create".
(define (varloc-finalize varloc scope level index)
  (varloc-scope-set! varloc scope)
  (varloc-level-set! varloc level)
  (varloc-index-set! varloc index))


;; Find the scope level specified in varloc.
(define (varloc-find-scope-level varloc)
  (let loop ((scope (varloc-scope varloc))
             (level (varloc-level varloc)))
    (if (= level 0)
        scope
        (loop (scope-next scope)
              (1- level)))))


;; Get variable value (within scope env) by varloc.
(define (varloc-scope-get varloc)
  (scope-get-var-at-index (varloc-find-scope-level varloc)
                          (varloc-index varloc)))


;; Set variable value (within scope env) by varloc.
(define (varloc-scope-set varloc value)
  (scope-set-var-at-index (varloc-find-scope-level varloc)
                          (varloc-index varloc)
                          value))


;; Find stack entry by level and index. Go down in stack by
;; levels. Each level corresponds to one stack frame.
;;
;;   Example:
;;
;;       varloc: level = 1, index = 2
;;
;;
;;                                 level
;;
;;           top ->  0 1 2 3       0
;;
;;                   0 1 2 3 4     1
;;                       ^
;;                   0 1 2 3       2
;;
;;                   ...
;;
(define (varloc-find-stack-level stack varloc)
  (let loop ((stack stack)
             (level (varloc-level varloc)))
    (if (= level 0)
        (car stack)
        (loop (cdr stack)
              (1- level)))))


;; Get variable value through stack.
(define (varloc-get stack varloc)
  (vector-ref (varloc-find-stack-level stack varloc)
              (varloc-index varloc)))


;; Set variable value through stack.
(define (varloc-set stack varloc value)
  (vector-set! (varloc-find-stack-level stack varloc)
               (varloc-index varloc)
               value))

;; VARIABLES:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; PARSING:

(define gulex-token-table
  (if #t
      (list
       '(";.*"                           COMMENT)
       '("\\("                           LEFTPAREN)
       '("\\)"                           RIGHTPAREN)
       '("#t"                            TRUE)
       '("#f"                            FALSE)
       '("#nil"                          NULL)
       '("\"(\\\\.|[^\"])*\""            STRING)
       '("[0-9]+\\.[0-9]+"               FLOAT)
       '("[0-9]+"                        INTEGER)
       '("inc"                           KEY-INC)
       '("def"                           KEY-DEF)
       '("fun"                           KEY-FUN)
       '("ceq"                           KEY-CEQ)
       '("cgt"                           KEY-CGT)
       '("clt"                           KEY-CLT)
       '("cge"                           KEY-CGE)
       '("cle"                           KEY-CLE)
       '("sel"                           KEY-SEL)
       '("arr"                           KEY-ARR)
       '("dic"                           KEY-DIC)
       '("num"                           KEY-NUM)
       '("str"                           KEY-STR)
       '("mul"                           KEY-MUL)
       '("add"                           KEY-ADD)
       '("sub"                           KEY-SUB)
       '("neg"                           KEY-NEG)
       '("len"                           KEY-LEN)
       '("ref"                           KEY-REF)
       '("set"                           KEY-SET)
       '("gen"                           KEY-GEN)
       '("run"                           KEY-RUN)
       '("cat"                           KEY-CAT)
       '("glu"                           KEY-GLU)
       '("div"                           KEY-DIV)
       '("rip"                           KEY-RIP)
       '("fix"                           KEY-FIX)
       '("env"                           KEY-ENV)
       '("egg"                           KEY-EGG)
       '("[a-zA-Z_][a-zA-Z0-9_-]*"       ID)
       '("[ \t\n]+"                      WHITESPACE)
       '("."                             UNKNOWN)
       )
      (list
       '("\"(\\\\.|[^\"])*\""            STRING))))


(define con-lexer (gulex-create-lexer-interp gulex-token-table))


;; astnode:
;;
;;     (<node-type> <syntax-value> <token>)
;;
(define (make-astnode type value token)
  (list type value token))

;; Node type.
(define (astnode-type node)
  (first node))

;; Node type.
(define (astnode-type-set! node type)
  (set-car! node type))

;; Node values.
(define (astnode-value node)
  (second node))

;; Node token, except #f for internal nodes (i.e. intermediate
;; values).
(define (astnode-token node)
  (third node))


;; Function definition.
(define-record-type fundef
  (fields params                        ; Function parameters.
          body                          ; Function body.
          (mutable scope)               ; Scope for function.
          ))


;; Check if fundef is tail-recursive. Return the atom that is the
;; tail-recursive fundef or #f, in none found.
;;
;; There are only two control structures in "con": "fun" and
;; "sel". We only have to check for "fun" and "sel" structures. The
;; tail-call can be in either TRUE or FALSE branch for "sel", and it
;; can only be in the last expression of "fun" body for "fun".
;;
(define (find-recursive-calls atom funname)

  (let loop ((atom atom))

    (case (astnode-type atom)

      ((if-stmt)
       (append (loop (second (astnode-value atom)))
               (loop (third (astnode-value atom)))))

      ((user-call)
       (if (let ((funref (car (astnode-value atom))))
             (and (= (varloc-level funref) 0)
                  (= (varloc-index funref) 0)
                  (string=? (varloc-id funref)
                            funname)))
           (list atom)
           '()))

      (else '()))))


(define (parse-con ts)

  ;; NOTE:
  ;;
  ;; "parse-con" argument "ts" is used by sub-function
  ;; closures. However, "ts" is set to a new value when an "inc"
  ;; directive is encountered. "ts" is hence mutated, but the closures
  ;; will capture/provide the mutated value as expected.
  ;;

  (define (flush-idle-tokens)
    (let loop ()
      (case (cur-type)
        ((COMMENT)
         (token-stream-get ts)
         (loop))
        ((WHITESPACE)
         (token-stream-get ts)
         (loop))
        (else (cur-token)))))

  (define (get)
    (token-stream-get ts)
    (flush-idle-tokens))

  (define (cur-token) (token-stream-token ts))

  (define (cur-type) (token-type (token-stream-token ts)))

  (define (cur-value) (token-value (token-stream-token ts)))

  (define (expect type)
    (if (eq? (cur-type) type)
        (let ((ret (cur-token)))
          (get)
          ret)
        (con-error (token-stream-token ts) (ss "Expected: " type ", got: " (cur-type)))))

  (define (use)
    (let ((ret (cur-token)))
      (get)
      ret))

  (define (eof?)
    (eq? (cur-type) 'eof))

  (define (make-astnode-atom type value)
    (let ((ret (make-astnode type value (cur-token))))
      (use)
      ret))


  (define (parse-atom-pair)
    (expect 'LEFTPAREN)
    (let* ((p1 (parse-atom))
           (p2 (parse-atom)))
      (expect 'RIGHTPAREN)
      (cons p1 p2)))


  (define (parse-datum)

    ;; Parse all non-inc syntax.
    (define (parse-datum-non-inc key-tok)

      (case (token-type key-tok)

        ((KEY-DEF)
         ;;     (def <id> <value>)
         (let* ((id (token-value (expect 'ID)))
                (value (parse-atom)))
           (make-astnode 'vardef (cons (varloc-create id) value) key-tok)))

        ((KEY-FUN)
         ;;          params    body
         ;;     (fun (a b)     (add a b))
         (expect 'LEFTPAREN)
         ;; NOTE: Create function parameters the same way
         ;; as variable definitions are created. This
         ;; renders scope creation uniform between variable
         ;; declarations and function arguments.
         (let* ((params (let loop ((ret '()))
                          (case (cur-type)
                            ((ID) (loop (cons (varloc-create (token-value (use))) ret)))
                            ((RIGHTPAREN) (use) (reverse ret))
                            (else
                             (con-error (cur-token) (ss "Invalid function argument type: "
                                                         (cur-type)
                                                         " (\""
                                                         (cur-value)
                                                         "\") ..."))))))
                (body (parse-atoms)))
           (make-astnode 'fundef (make-fundef params body #f) key-tok)))

        ((KEY-CEQ KEY-CGT KEY-CLT KEY-CGE KEY-CLE)
         (make-astnode 'compare (list (token-type key-tok) (parse-atom) (parse-atom)) key-tok))

        ((KEY-SEL)
         ;;          cond     true     false
         ;;     (sel opt      12       (cat 12 (arr "dii" "duu") ":foobar:" (env "HOME")))
         (let* ((condition (parse-atom))
                (true-branch (parse-atom))
                (false-branch (parse-atom)))
           (make-astnode 'if-stmt (list condition true-branch false-branch) key-tok)))

        ((KEY-ARR)
         ;;     (arr 1 2 "foo" 12.3)
         (make-astnode 'array (parse-atoms) key-tok))

        ((KEY-DIC)
         ;;     (dic (1 2) ("foo" foo))
         (make-astnode 'dictionary (let loop ((ret '()))
                                     (if (eq? (cur-type) 'LEFTPAREN)
                                         (loop (cons (parse-atom-pair) ret))
                                         (reverse ret)))
                       key-tok))

        ((KEY-NUM KEY-STR)
         ;;     (num "123")
         (make-astnode 'prim-call (cons (token-type key-tok) (parse-atom)) key-tok))

        ((KEY-MUL KEY-ADD KEY-SUB)
         ;;     (mul 2 3)
         (make-astnode 'arith-op-multi (cons (token-type key-tok) (parse-atoms)) key-tok))

        ((KEY-NEG)
         (make-astnode 'arith-op-single (cons (token-type key-tok) (parse-atom)) key-tok))

        ((KEY-LEN)
         ;;     (len (arr 1 2 3)
         (make-astnode 'prim-call (cons (token-type key-tok) (parse-atom)) key-tok))

        ((KEY-REF)
         ;;     (ref (dic (1 2) ("foo" foo)) "foo")
         ;;     (ref (arr 1 2 3) 1)
         ;;     (ref (arr 1 2 3) 1 2)
         (make-astnode 'obj-reference (parse-atoms) key-tok))

        ((KEY-SET)
         ;;     (set (dic (1 2) ("foo" foo)) "foo" 12)
         (make-astnode 'obj-assign (list (parse-atom) (parse-atom) (parse-atom)) key-tok))

        ((KEY-GEN)
         ;;     Mapped to iota, and have args from 1 to 3.
         ;;     (gen 10)
         (make-astnode 'generate (parse-atoms) key-tok))

        ((KEY-RUN)
         ;;     (run (fun (x) (mul x 3)) (gen 10))
         (make-astnode 'mapping (cons (parse-atom) (parse-atom)) key-tok))

        ((KEY-CAT KEY-GLU)
         ;;     (cat 12 (arr "dii" "duu") ":foobar:" (env "HOME"))
         (make-astnode 'prim-call (cons (token-type key-tok) (parse-atoms)) key-tok))

        ((KEY-DIV)
         ;;     (div "foo/bar" "/")
         (make-astnode 'prim-call (cons (token-type key-tok) (list (parse-atom) (parse-atom))) key-tok))

        ((KEY-RIP)
         ;;     (rip "foobar" 0 2)
         (make-astnode 'prim-call (cons (token-type key-tok) (list (parse-atom) (parse-atom) (parse-atom))) key-tok))

        ((KEY-FIX KEY-ENV KEY-EGG)
         ;;     (fix (egg "echo foobar"))
         ;;     (env "HOME")
         (make-astnode 'prim-call (cons (token-type key-tok) (parse-atom)) key-tok))

        ((ID)
         ;;     (my-fun 1 2)
         (make-astnode 'user-call
                       (cons (varloc-create (token-value key-tok))
                             (parse-atoms))
                       key-tok))

        (else
         (con-error key-tok "Unknown s-expr head"))))


    (expect 'LEFTPAREN)

    (let ((ret (let ((key-tok (use)))

                 (if (eq? (token-type key-tok) 'KEY-INC)

                     ;;     (inc "example-inc.scm")
                     (let* ((inc-ret (make-astnode 'include-file (parse-atom) key-tok))
                            (filename (second (astnode-value inc-ret))))

                       ;; "inc" datum is terminated immediately. We
                       ;; look for RIGHTPAREN and call it a day.
                       ;;
                       ;; If the included file exists, we push the
                       ;; file to the token-stream top, and from the
                       ;; "con" perspective everything continues,
                       ;; business as usual.

                       (expect 'RIGHTPAREN)
                       (if (file-exists? filename)
                           (begin
                             (token-stream-push-file ts filename)
                             inc-ret
                             #f)
                           (con-error key-tok (ss "File not found: \"" filename "\""))))

                     ;; non-inc:
                     (let ((ret (parse-datum-non-inc key-tok)))
                       (expect 'RIGHTPAREN)
                       ret)))))
      ret))


  (define (parse-atom)

    (define (process-string text)
      (let loop ((chars (drop-right (drop (string->list text) 1) 1))
                 (ret '()))
        (if (pair? chars)
            (case (car chars)
              ((#\\) (let ((ch (cadr chars)))
                       (loop (cddr chars)
                             (cons (case ch
                                     ((#\n) #\newline)
                                     ((#\t) #\tab)
                                     (else ch))
                                   ret))))
              (else (loop (cdr chars)
                          (cons (car chars) ret))))
            (list->string (reverse ret)))))

    (case (cur-type)
      ((INTEGER) (make-astnode-atom 'integer (string->number (cur-value))))
      ((FLOAT) (make-astnode-atom 'float (string->number (cur-value))))
      ((TRUE) (make-astnode-atom 'boolean #t))
      ((FALSE) (make-astnode-atom 'boolean #f))
      ((NULL) (make-astnode-atom 'null #nil))
      ((ID) (make-astnode-atom 'varref (varloc-create (cur-value))))
      ((STRING) (make-astnode-atom 'string (process-string (cur-value))))
      ((LEFTPAREN) (parse-datum))))


  (define (parse-atoms)
    (let loop ((ret '()))
      (if (or (eq? (cur-type) 'eof)
              (eq? (cur-type) 'RIGHTPAREN))
          (reverse ret)
          (loop (let ((atom (parse-atom)))
                  (if atom (cons atom ret) ret))))))

  (flush-idle-tokens)
  (parse-atoms))


;; Convert keyword symbol to string: KEY-ADD -> "add".
(define (keywordsym->string keysym)
  (string-downcase (substring (symbol->string keysym) 4)))


;; PARSING:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; ELABORATION:

;; Elaborate atoms in/to scopes and return the number of errors
;; encountered (through the ast-tree).
(define (elab-con atoms scope)

  ;; Check argument count of a function (captured to the token).
  ;; Return number of errors.
  (define (check-arg-count token fun-name args spec . details)

    (define (msg spec)
      (ss spec ", for \"" fun-name "\""))

    (define (ensure value msg)
      (if value
          0
          (con-error-info token (ss "Incorrect number of arguments, expected: " msg))))

    (cond
     ((symbol? spec)
      (case spec
        ((one-or-more) (ensure (>= (length args) 1)
                               (msg "one or more")))

        ((range) (let ((min (first details))
                       (max (second details))
                       (cnt (length args)))
                   (ensure (and (<= min cnt)
                                (>= max cnt))
                           (msg (ss "range within " min "-" max)))))))))

  (define (elab-atoms-with-scope atoms scope funname)
    (fold (lambda (atom accu)
            (+ (elab-atom-with-scope atom scope funname)
               accu))
          0
          atoms))

  ;; TODO: In order to handle out-of-order variable definitions, make
  ;; first pass to create vardefs and only after that handle possible
  ;; varrefs.
  (define (elab-atom-with-scope atom scope funname)

    (define (elab-atom atom)
      (elab-atom-with-scope atom scope #f))

    (define (elab-atoms atoms)
      (elab-atoms-with-scope atoms scope #f))

    (case (astnode-type atom)

      ((vardef)
       ;; vardef: (<varloc> <value>)
       ;; Elaborate the value part.
       (let* ((varloc (car (astnode-value atom)))
              (index (scope-elab-register-varloc scope varloc (astnode-token atom))))
         (varloc-finalize varloc scope 0 index)
         (elab-atom-with-scope (cdr (astnode-value atom)) scope (varloc-id varloc))))

      ((varref)
       (let* ((varref (astnode-value atom))
              (level-index (scope-elab-get-var-index scope (varloc-id varref))))
         (if level-index
             (begin
               (varloc-finalize varref scope (car level-index) (cdr level-index))
               0)
             (con-error-info (astnode-token atom) (ss "Variable does not exist: \"" (varloc-id varref) "\"")))))

      ((fundef)
       (let ((fun-scope (scope-create scope))
             (fundef (astnode-value atom)))

         ;; Create slot (reservation) for the function itself, to the
         ;; function scope. Self-calling functions are stored here.
         (scope-elab-register-varloc fun-scope (varloc-create funname) (astnode-token atom))

         ;; Add function arguments to function scope.
         (for-each (lambda (varref) (scope-elab-register-varloc fun-scope varref (astnode-token atom)))
                   (fundef-params fundef))

         (let ((error-count (elab-atoms-with-scope (fundef-body fundef) fun-scope #f)))
           (scope-finalize fun-scope)
           (fundef-scope-set! fundef fun-scope)

           ;; Check if we have a tail call to self.
           (let* ((last-call (last (fundef-body fundef)))
                  (recursive-calls (find-recursive-calls last-call funname)))
             (when (pair? recursive-calls)
               ;; Marked user-call as tail-call and also to be checked in tcheck.
               (for-each (lambda (call)
                           (astnode-type-set! call 'user-call-tail-to-check))
                         recursive-calls)))

           error-count)))

      ((compare)
       ;; NOTE: Parsed argument count check.
       (+ (elab-atom (second (astnode-value atom)))
          (elab-atom (third (astnode-value atom)))))

      ((if-stmt)
       (let ((val (astnode-value atom)))
         (+  (elab-atom (first val))
             (elab-atom (second val))
             (elab-atom (third val)))))

      ((array)
       (elab-atoms (astnode-value atom)))

      ((dictionary)
       ;; "dictionary" requires a "custom" loop for elab-error
       ;; counting, since the value contains list of pairs.
       (let loop ((atom-pairs (astnode-value atom))
                  (error-count 0))
         (if (pair? atom-pairs)
             (loop (cdr atom-pairs)
                   (+ error-count (+ (elab-atom (caar atom-pairs))
                                     (elab-atom (cdar atom-pairs)))))
             error-count)))

      ((arith-op-multi)
       (+ (check-arg-count (astnode-token atom)
                           (keywordsym->string (car (astnode-value atom)))
                           (cdr (astnode-value atom))
                           'one-or-more)
          (elab-atoms (cdr (astnode-value atom)))))

      ((arith-op-single) (elab-atom (cdr (astnode-value atom))))

      ((obj-reference)
       (elab-atoms (astnode-value atom)))

      ((obj-assign)
       ;; NOTE: Parsed argument count check.
       (elab-atoms (astnode-value atom)))

      ((generate)
       (+ (check-arg-count (astnode-token atom)
                           "generate"
                           (astnode-value atom)
                           'range 1 3)
          (elab-atoms (astnode-value atom))))

      ((mapping)
       ;; NOTE: Parsed argument count check.
       (+ (elab-atom (car (astnode-value atom)))
          (elab-atom (cdr (astnode-value atom)))))

      ((prim-call)
       (let* ((prim (car (astnode-value atom)))
              (args (cdr (astnode-value atom)))
              (do-many (lambda (args)
                         (+ (check-arg-count (astnode-token atom)
                                             (keywordsym->string prim)
                                             args
                                             'one-or-more)
                            (elab-atoms args)))))
         (case prim
           ((KEY-NUM) (elab-atom args))
           ((KEY-STR) (elab-atom args))
           ((KEY-LEN) (elab-atom args))
           ((KEY-CAT) (do-many args))
           ((KEY-GLU) (do-many args))
           ((KEY-DIV) (do-many args))
           ((KEY-RIP) (do-many args))
           ;; NOTE: Parsed argument count check.
           ((KEY-FIX) (elab-atom args))
           ((KEY-ENV) (elab-atom args))
           ((KEY-EGG) (elab-atom args))
           )))

      ((user-call)

       ;; Fundef's should be referenced from scopes, since
       ;; they are static as are the functions themselves.
       ;;
       ;; NOT REALLY: Variables can then be referenced from stack, but
       ;; "free" variables should be in the free variable vector!!!

       (let* ((varref (car (astnode-value atom)))
              (level-index (scope-elab-get-var-index scope (varloc-id varref))))
         (varloc-finalize varref scope (car level-index) (cdr level-index))
         (elab-atoms (cdr (astnode-value atom)))))

      (else 0)))

  (elab-atoms-with-scope atoms scope #f))

;; ELABORATION:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; TYPE CHECK:

;; Check types and return error count.
(define (tcheck-con atoms do-check)

  (define (make-tcheck errors type . rest)
    (cons errors (cons type rest)))

  (define tcheck-count first)

  (define tcheck-type second)

  (define (tcheck? atom)
    (integer? (car atom)))

  ;; Return check failure count (1/0).
  (define (check-atom-type tcheck type token tag)
    (let ((valid? (case type
                    ((ignore) #t)
                    ((numeric integer) (case (tcheck-type tcheck)
                                         ((numeric integer float ignore) #t)
                                         (else #f)))
                    (else (eq? (tcheck-type tcheck) type)))))
      (if valid?
          (tcheck-count tcheck)
          (begin
            (con-error-info token (ss "Invalid argument types, for: \"" tag "\", expected: " type ", got: " (tcheck-type tcheck)))
            ;;(make-tcheck (1+ (tcheck-count tcheck)) 'ignore)
            (1+ (tcheck-count tcheck))
            ))))


  ;; Return check failure count.
  (define (check-atom-types tchecks type token tag)
    ;; Each listed type must match.
    (let loop ((tail tchecks)
               (types (if (list? type)
                          type
                          (make-list (length tchecks) type)))
               (error-count 0))
      (if (pair? tail)
          (loop (cdr tail)
                (cdr types)
                (+ error-count (check-atom-type (car tail)
                                                (car types)
                                                token
                                                tag)))
          ;;(make-tcheck error-count 'ignore)
          error-count
          )))

  ;; Return tcheck (i.e. (<count> . <type)) for array.
  (define (check-array atoms token tag)
    (let* ((base (tcheck-atom (car atoms)))
           (base-type (tcheck-type base)))
      (if (> (tcheck-count base) 0)
          ;; Type-error in first item.
          (make-tcheck 1 'ignore)
          ;; Check that all types are the same as first in the array.
          (make-tcheck (check-atom-types (map tcheck-atom (cdr atoms))
                                         base-type
                                         token
                                         tag)
                       base-type))))

  (define (check-count count lst)
    (if (= count (length lst))
        0
        1))

  (define (user-call fundef-atom args)
    (let ((fundef (astnode-value fundef-atom)))

      ;; Store self to index 0.
      (scope-set-var-at-index (fundef-scope fundef)
                              0
                              fundef-atom)

      (let loop ((args args)
                 (index 1))
        (when (pair? args)
          (scope-set-var-at-index (fundef-scope fundef)
                                  index
                                  (tcheck-atom (car args)))
          (loop (cdr args)
                (1+ index))))

      ;; Evaluate each expression in body and return value of last.
      (let loop ((body (fundef-body fundef))
                 (ret #f))
        (if (pair? body)
            (loop (cdr body)
                  (tcheck-atom (car body)))
            ret))))


  (define (tcheck-atom atom)

    (define (make-tcheck-self atom) (make-tcheck 0 (astnode-type atom)))
    (define (make-tcheck-ignore) (make-tcheck 0 'ignore))

    (if (tcheck? atom)

        atom

        (case (astnode-type atom)

          ((integer float boolean string null) (make-tcheck-self atom))

          ;; We can't store tcheck results to scope since the real
          ;; thing (fundef) might be needed.
          ((vardef)
           (let ((value (cdr (astnode-value atom))))
             (varloc-scope-set (car (astnode-value atom)) value)
             (let ((tcheck-value (tcheck-atom value)))
               (make-tcheck (car tcheck-value) 'ignore))))

          ((varref)
           (tcheck-atom (varloc-scope-get (astnode-value atom))))

          ((fundef)
           (make-tcheck-self atom))

          ((compare)
           (let* ((tcheck-index-1 (tcheck-atom (second (astnode-value atom))))
                  (tcheck-index-2 (tcheck-atom (third (astnode-value atom))))
                  (tcheck-type-1 (check-atom-type tcheck-index-1
                                                  'numeric
                                                  (astnode-token atom)
                                                  (keywordsym->string (first (astnode-value atom)))))
                  (tcheck-type-2 (check-atom-type tcheck-index-2
                                                  'numeric
                                                  (astnode-token atom)
                                                  (keywordsym->string (first (astnode-value atom))))))
             (if (and (eq? (second tcheck-index-1)
                           (second tcheck-index-2))
                      (= (+ tcheck-type-1 tcheck-type-2) 0))
                 (make-tcheck 0 'ignore)
                 (make-tcheck 1 'ignore))))

          ((if-stmt)
           (check-array (list (second (astnode-value atom))
                              (third (astnode-value atom)))
                        (astnode-token atom)
                        "sel"))

          ((array)
           (if (> (length (astnode-value atom)) 0)
               (let ((tcheck-elems (check-array (astnode-value atom)
                                                (astnode-token atom)
                                                "arr")))
                 (list (first tcheck-elems)
                       'array
                       (second tcheck-elems)))
               (make-tcheck (con-error-info (astnode-token atom) (ss "Array must have at least one element"))
                            'ignore)))

          ((dictionary)
           ;; NOTE: It is a runtime error if there is no value for the
           ;; key.
           (if (> (length (astnode-value atom)) 0)
               (let ((tcheck-elems (check-array (map cdr (astnode-value atom))
                                                (astnode-token atom)
                                                "dic")))
                 (list (first tcheck-elems)
                       'dictionary
                       (second tcheck-elems)))
               (make-tcheck (con-error-info (astnode-token atom) (ss "Dictionary must have at least one element"))
                            'ignore)))

          ((arith-op-multi)
           (let ((prim (car (astnode-value atom)))
                 (args (cdr (astnode-value atom))))
             (case prim
               ((KEY-MUL KEY-ADD KEY-SUB)
                (make-tcheck (check-atom-types (map tcheck-atom args)
                                               'numeric
                                               (astnode-token atom)
                                               (keywordsym->string prim))
                             'numeric)))))

          ((arith-op-single)
           (let ((prim (car (astnode-value atom)))
                 (args (cdr (astnode-value atom))))
             (case prim
               ((KEY-NEG)
                (make-tcheck (check-atom-type (tcheck-atom args)
                                              'numeric
                                              (astnode-token atom)
                                              (keywordsym->string prim))
                             'numeric)))))

          ;;     (ref (dic (1 2) ("foo" foo)) "foo")
          ;;     (ref (arr 1 2 3) 1)
          ;;     (ref (arr 1 2 3) 1 2)
          ((obj-reference)
           (let ((tcheck-container (tcheck-atom (first (astnode-value atom))))
                 (val (astnode-value atom)))
             (case (second tcheck-container)
               ((array)
                (case (length val)
                  ((2) (let ((tcheck-index (check-atom-type (tcheck-atom (second (astnode-value atom)))
                                                            'integer
                                                            (astnode-token atom)
                                                            "arr")))
                         (astnode-type-set! atom 'array-index-ref)
                         (if (= tcheck-index 0)
                             (list (first tcheck-container) (third tcheck-container))
                             (make-tcheck tcheck-index 'ignore))))
                  ((3) (let ((tcheck-index-1 (check-atom-type (tcheck-atom (second (astnode-value atom)))
                                                              'integer
                                                              (astnode-token atom)
                                                              "arr"))
                             (tcheck-index-2 (check-atom-type (tcheck-atom (third (astnode-value atom)))
                                                              'integer
                                                              (astnode-token atom)
                                                              "arr")))
                         (astnode-type-set! atom 'array-range-ref)
                         (if (= (+ tcheck-index-1 tcheck-index-2) 0)
                             (list (first tcheck-container) (third tcheck-container))
                             (make-tcheck (+ tcheck-index-1 tcheck-index-2) 'ignore))))
                  (else (make-tcheck (con-error-info (astnode-token atom) (ss "Incorrect number of arguments, expected: 1-2"))
                                     'ignore))))
               ((dictionary)
                (astnode-type-set! atom 'dictionary-ref)
                (list (first tcheck-container) (third tcheck-container))))))

          ((prim-call)
           (let ((prim (car (astnode-value atom)))
                 (args (cdr (astnode-value atom))))
             (case prim
               ((KEY-NUM)
                (make-tcheck (check-atom-type (tcheck-atom args)
                                              'string
                                              (astnode-token atom)
                                              (keywordsym->string prim))
                             'string))
               ((KEY-STR)
                (make-tcheck 0 'string))
               ((KEY-LEN)
                (make-tcheck (check-atom-type (tcheck-atom args)
                                              'array
                                              (astnode-token atom)
                                              (keywordsym->string prim))
                             'integer))
               ;;     (cat "foo" (arr "bar" 1 2 3) "/" "jii.haa")
               ((KEY-CAT)
                (make-tcheck 0 'string))
               ;;     (glu "@" "foo" (arr "bar" 1 2 3) "/" "jii.haa")
               ((KEY-GLU)
                (make-tcheck (check-atom-type (tcheck-atom (car args))
                                              'string
                                              (astnode-token atom)
                                              (keywordsym->string prim))
                             'string))
               ;;     (div "/foo//bar" "/")
               ((KEY-DIV)
                (if (= (length args) 2)
                    (make-tcheck (check-atom-types (map tcheck-atom args)
                                                   'string
                                                   (astnode-token atom)
                                                   (keywordsym->string prim))
                                 'string)
                    (make-tcheck (con-error-info (astnode-token atom) (ss "Incorrect number of arguments, expected: 1"))
                                 'ignore)))
               ;;     (rip "/foo//bar" 0 2)
               ((KEY-RIP)
                (if (= (length args) 3)
                    (let ((tcheck-str (check-atom-type (tcheck-atom (first args))
                                                       'string
                                                       (astnode-token atom)
                                                       (keywordsym->string prim)))
                          (tcheck-index-1 (check-atom-type (tcheck-atom (second args))
                                                           'integer
                                                           (astnode-token atom)
                                                           (keywordsym->string prim)))
                          (tcheck-index-2 (check-atom-type (tcheck-atom (third args))
                                                           'integer
                                                           (astnode-token atom)
                                                           (keywordsym->string prim))))
                      (if (= (+ tcheck-str tcheck-index-1 tcheck-index-2) 0)
                          (make-tcheck 0 'string)
                          (make-tcheck 1 'ignore)))
                    (make-tcheck (con-error-info (astnode-token atom) (ss "Incorrect number of arguments, expected: 3"))
                                 'ignore)))
               ((KEY-FIX KEY-ENV KEY-EGG)
                (make-tcheck (check-atom-type (tcheck-atom args)
                                              'string
                                              (astnode-token atom)
                                              (keywordsym->string prim))
                             'string)))))

          ((obj-assign)
           ;;          |<dic>                  |<ref>    |<val>
           ;;     (set (dic (1 2) ("foo" foo)) "foo"     12)
           (let* ((tcheck-container (tcheck-atom (first (astnode-value atom))))
                  (set-check (check-atom-type tcheck-container
                                              'dictionary
                                              (astnode-token atom)
                                              "set"))
                  ;; Make sure dic values are same type as the
                  ;; assigned value.
                  (val-check (check-atom-type (tcheck-atom (third (astnode-value atom)))
                                              (third tcheck-container)
                                              (astnode-token atom)
                                              "set"))
                  (err-count (+ set-check val-check)))
             (if (= err-count 0)
                 (begin
                   ;; (pde tcheck-container)
                   (list (first tcheck-container) 'dictionary (third tcheck-container)))
                 (make-tcheck err-count 'ignore))))

          ((generate)
           (make-tcheck (check-atom-types (map tcheck-atom (astnode-value atom))
                                          'integer
                                          (astnode-token atom)
                                          "gen")
                        'array
                        'integer))

          ;;     (run (fun (x) (mul x 3)) (gen 10))
          ((mapping)
           ;; TODO: Check that fun is not tail-recursive.
           (let* ((fun-check (check-atom-type (tcheck-atom (car (astnode-value atom)))
                                              'fundef
                                              (astnode-token atom)
                                              "run"))
                  (tcheck-gen (tcheck-atom (cdr (astnode-value atom))))
                  (gen-check-container (check-atom-type (list (car tcheck-gen) (second tcheck-gen))
                                                        'array
                                                        (astnode-token atom)
                                                        "run"))
                  (gen-check-value (check-atom-type (list (car tcheck-gen) (third tcheck-gen))
                                                    'integer
                                                    (astnode-token atom)
                                                    "run"))
                  (err-count (+ fun-check gen-check-container gen-check-value)))
             (if (= err-count 0)
                 (make-tcheck err-count 'array 'integer)
                 (make-tcheck err-count 'ignore))))

          ((user-call)
           ;; TODO: Store call stack info, so that invalid arguments
           ;; can be reported to the call site.
           ;;
           ;;     (my-fun 1 2)
           (let* ((fundef-atom (varloc-scope-get (car (astnode-value atom))))
                  (args (cdr (astnode-value atom))))
             (user-call fundef-atom args)))

          ;; Non-checked tail-call.
          ((user-call-tail-to-check)
           ;; Mark user-call checked!
           (astnode-type-set! atom 'user-call-tail)
           (let* ((fundef-atom (varloc-scope-get (car (astnode-value atom))))
                  (args (cdr (astnode-value atom))))
             (user-call fundef-atom args)))

          ;; Checked tail-call.
          ((user-call-tail)
           ;; Nothing to check anymore.
           (make-tcheck 0 'ignore))

          (else (con-internal-error (ss "Unknown primitive: \"" (astnode-type atom) "\""))))))

  (if do-check
      (fold (lambda (tcheck accu)
              (+ accu (tcheck-count tcheck)))
            0
            (map tcheck-atom atoms))
      0))


;; TYPE CHECK:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; EVALUATION:

(define (eval-con atoms top-scope)

  (define stack '())

  (define (push-stack scope)
    (set! stack (cons (make-vector (scope-count scope))
                      stack)))

  (define (pop-stack)
    (set! stack (cdr stack)))

  (define (eval-atom atom)

    ;;      fundef args
    ;;      v      v ...
    ;;     (my-fun 1 2)
    (define (user-call fundef args)
      ;; Copy function arguments to function scope locations,
      ;; i.e. pass function parameters.

      (push-stack (fundef-scope fundef))

      ;; Store self to stack at index 0.
      (vector-set! (car stack) 0 fundef)

      ;; Store function arguments to stack (top), starting at index 1,
      ;; since "self" is at 0.
      (let loop ((args args)
                 (index 1))
        (when (pair? args)
          (vector-set! (car stack) index (car args))
          (loop (cdr args)
                (1+ index))))

      ;; Evaluate each expression in body and return value of last.
      (let ((ret (let loop ((body (fundef-body fundef))
                            (ret #f))
                   (if (pair? body)
                       (loop (cdr body)
                             (eval-atom (car body)))
                       ret))))
        (pop-stack)
        ret))


    (define (user-call-tail fundef args)
      ;; Store function arguments to stack (top), starting at index 1,
      ;; since "self" is at 0. Don't push or pop the stack.
      (let loop ((args args)
                 (index 1))
        (when (pair? args)
          (vector-set! (car stack) index (car args))
          (loop (cdr args)
                (1+ index))))

      ;; Evaluate each expression in body and return value of last.
      (let ((ret (let loop ((body (fundef-body fundef))
                            (ret #f))
                   (if (pair? body)
                       (loop (cdr body)
                             (eval-atom (car body)))
                       ret))))
        ret))

    (case (astnode-type atom)

      ((integer float boolean string null) (astnode-value atom))

      ((vardef)
       (let ((value (eval-atom (cdr (astnode-value atom)))))
         (varloc-set stack (car (astnode-value atom)) value))
       *unspecified*)

      ((varref)
       (varloc-get stack (astnode-value atom)))

      ((fundef) (astnode-value atom))

      ((array) (map eval-atom (astnode-value atom)))

      ((dictionary) (map (lambda (pair)
                           (cons (eval-atom (car pair))
                                 (eval-atom (cdr pair))))
                         (astnode-value atom)))

      ((array-index-ref)
       (let ((arr (eval-atom (first (astnode-value atom))))
             (idx (eval-atom (second (astnode-value atom)))))
         (if (< idx (length arr))
             (list-ref arr idx)
             (con-error (astnode-token atom) "Array index overrun."))))

      ((array-range-ref)
       (let ((arr (eval-atom (first (astnode-value atom))))
             (idx1 (eval-atom (second (astnode-value atom))))
             (idx2 (eval-atom (third (astnode-value atom)))))
         (if (and (< idx1 idx2)
                  (<= idx2 (length arr)))
             (list-head (list-tail arr idx1) (- idx2 idx1))
             (con-error (astnode-token atom) "Invalid array range."))))

      ((dictionary-ref)
       (let ((ret (assoc-ref (eval-atom (first (astnode-value atom)))
                             (eval-atom (second (astnode-value atom))))))
         (if ret
             ret
             (con-error (astnode-token atom) "Dictionary item not found."))))

      ((compare)
       (let ((logic-op (lambda (op) (op (eval-atom (second (astnode-value atom)))
                                        (eval-atom (third (astnode-value atom)))))))
         (case (first (astnode-value atom))
           ((KEY-CEQ) (logic-op equal?))
           ((KEY-CGT) (logic-op >))
           ((KEY-CLT) (logic-op <))
           ((KEY-CGE) (logic-op >=))
           ((KEY-CLE) (logic-op <=)))))

      ((if-stmt)
       (if (eval-atom (first (astnode-value atom)))
           (eval-atom (second (astnode-value atom)))
           (eval-atom (third (astnode-value atom)))))

      ((prim-call)
       (let ((prim (car (astnode-value atom)))
             (args (cdr (astnode-value atom))))
         (case prim
           ((KEY-NUM) (string->number (eval-atom args)))
           ((KEY-STR) (object->string (eval-atom args)))
           ((KEY-LEN) (length (eval-atom args)))
           ((KEY-CAT) (apply core.cat (map eval-atom args)))
           ((KEY-GLU) (apply core.glu (map eval-atom args)))
           ((KEY-DIV) (apply core.div (map eval-atom args)))
           ((KEY-RIP) (apply core.rip (map eval-atom args)))
           ((KEY-FIX) (core.fix (eval-atom args)))
           ((KEY-ENV) (core.env (eval-atom args)))
           ((KEY-EGG) (core.egg (eval-atom args)))
           )))

      ((arith-op-multi)
       (let ((prim (car (astnode-value atom)))
             (args (cdr (astnode-value atom))))
         (case prim
           ((KEY-MUL) (apply * (map eval-atom args)))
           ((KEY-ADD) (apply + (map eval-atom args)))
           ((KEY-SUB) (apply - (map eval-atom args)))
           )))

      ((arith-op-single)
       (let ((prim (car (astnode-value atom)))
             (args (cdr (astnode-value atom))))
         (case prim
           ((KEY-NEG) (- (eval-atom args))))))

      ((user-call)
       ;;     (my-fun 1 2)
       (let* ((fundef (astnode-value (varloc-scope-get (car (astnode-value atom)))))
              (args (map eval-atom (cdr (astnode-value atom)))))
         (user-call fundef args)))

      ((user-call-tail)
       ;;     (my-fun 1 2)
       (let* ((fundef (vector-ref (car stack) 0))
              (args (map eval-atom (cdr (astnode-value atom)))))
         (user-call-tail fundef args)))

      ((obj-assign)
       (let ((ret (assoc-set! (eval-atom (first (astnode-value atom)))
                              (eval-atom (second (astnode-value atom)))
                              (eval-atom (third (astnode-value atom))))))
         (if ret
             ret
             (con-error (astnode-token atom) "Dictionary item not found."))))

      ((generate) (apply iota (map eval-atom (astnode-value atom))))

      ((mapping)
       (let* ((fundef (eval-atom (car (astnode-value atom))))
              (args (eval-atom (cdr (astnode-value atom)))))
         (map (lambda (i) (user-call fundef (list i))) args)))

      (else (con-internal-error (ss "Unknown primitive: \"" (astnode-type atom) "\"")))))

  ;; Prepare stack.
  (push-stack top-scope)

  ;; Evaluate all top level atoms.
  (map eval-atom atoms))

;; EVALUATION:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; ENTRY:

(define (con-do token-stream)
  (let* ((file (token-stream-file token-stream))
         (ast (parse-con token-stream))
         (top-scope (scope-create))
         (elab-error-count (elab-con ast
                                      top-scope)))
    (if (= elab-error-count 0)
        (begin
          (scope-finalize top-scope)
          (let ((tcheck-error-count (tcheck-con ast #t)))
            (if (= tcheck-error-count 0)
                (eval-con ast top-scope)
                (con-exit-from-file file (ss "Exiting after " tcheck-error-count " errors ...")))))
        (con-exit-from-file file (ss "Exiting after " elab-error-count " errors ...")))))


(define (con-file file)
  (con-do (token-stream-open file con-lexer)))


(define (con-text text)
  (con-do (token-stream-open-with-text text con-lexer)))

;; ENTRY:
;; ------------------------------------------------------------
