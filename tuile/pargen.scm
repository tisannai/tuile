(define-module (tuile pargen)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module (tuile basic)
  #:use-module (tuile pr)
  #:use-module (tuile issues)
  #:export
  (
   pargen-write-parser-module
   pargen-write-lalr-module
   ))


;; Pargen generates an lalr based parser. An example grammar is in the
;; end of the file.
;;
;; This library should be used from a simple executable. The
;; executable looks something like this (for parse-vlog.scm grammar):
;;
;;     (use-modules (tuile pargen))
;;     (define grammar-vlog #f)
;;     (primitive-load "rapid/grammar-vlog.scm")
;;     (pargen-write-parser-module "rapid/parse-vlog.scm"
;;                                 '(rapid parse-vlog)
;;                                 '()
;;                                 "parse-vlog"
;;                                 5
;;                                 grammar-vlog)
;;
;; Parsing is performed for a file:
;;
;;     (use-modules ((rapid parse-vlog) #:prefix #{vlog:}#))
;;     (define ast-vlog (vlog:parse "my-vlog.v"))
;;


;; opt - options
;; tok - define token
;; seq - sequence
;; sel - select from chars
;; zoo - zero or one
;; zom - zero or more
;; oom - one  or more
;; ran - range
;; any - any char
;; inv - not


;; Format user-style grammar rules to explicit/elaborated parser
;; rules.
;;
;; user-parser -> elab-parser
;;
;; FROM:
;;     ((start block*)
;;      (block LPAR
;;             BLOCK
;;             ID
;;             vardef*
;;             RPAR))
;;
;; TO:
;;     ((start . (zom (rule block)))
;;      (block . (seq (term LPAR)
;;                    (term BLOCK)
;;                    (term ID)
;;                    (zom (rule vardef))
;;                    (term RPAR))))
;;

(define (pargen-import-user-parser pardef)

  (define (import-symbol token)

    (define (chars->symbol chars)
      ;; term or rule
      (if (char-set-contains? char-set:upper-case (car chars))
          (list 'term (string->symbol (list->string chars)))
          (list 'rule (string->symbol (list->string chars)))))

    (define (->symbol-except-last-char chars)
      (chars->symbol (list-head chars (1- (length chars)))))

    (define (->symbol chars)
      (chars->symbol chars))

    (let* ((token-str (symbol->string token))
           (chars (string->list token-str))
           (first-char (car chars))
           (last-char (car (last-pair chars))))
      (cond
       ((char=? first-char #\-) (list 'empty 'empty))
       ((char=? last-char #\?) (list 'zoo (->symbol-except-last-char chars)))
       ((char=? last-char #\*) (list 'zom (->symbol-except-last-char chars)))
       ((char=? last-char #\+) (list 'oom (->symbol-except-last-char chars)))
       (else (->symbol chars)))))

  (define (import-production production)
    (cons (first production)
          (match (second production)
            ( ((seq ...) ...) (cons 'opt (map (lambda (seq) (cons 'seq (map import-symbol seq))) seq)) )
            ( (opt ...)   (cons 'opt (map import-symbol opt)) )
            (else
             (if (= (length (cdr production)) 1)
                 (import-symbol (second production))
                 (cons 'seq (map import-symbol (cdr production))))))))

  (map import-production pardef))


;;     Terminal     = term
;;     Non-terminal = rule
;;     Subject      = term | rule | prural
(define (pargen-output-parser-module parser-id expect grammar)

  (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
         (lexer-value (map (lambda (ld) (cons (second ld)
                                              (or (eq? (third ld) 'value)
                                                  (eq? (third ld) 'typeval)))) lexer-def))
         ;; (tokens (map second lexer-def))
         (parser-def (car (assoc-ref grammar 'parser)))
         (pargen-defs (pargen-import-user-parser parser-def)))

    (define (token-output tokdef)
      (case (third tokdef)
        ((comment space error) *unspecified*)
        (else (second tokdef))))

    (define (pargen-defs->lalr-defs pargen-defs)

      (define (id-prural rule)
        ;; (pde rule)
        (case (first rule)
          ((term) (string->symbol (ss (string-downcase (symbol->string (second rule))) "s")))
          ((zoo) (id-prural-zoo rule))
          ((zom) (id-prural-zom rule))
          ((oom) (id-prural-oom rule))))

      (define (id-prural-zoo rule)
        (symbol-append (second (second rule)) '-zoo))

      (define (id-prural-zom rule)
        (symbol-append (second (second rule)) '-zom))

      (define (id-prural-oom rule)
        (symbol-append (second (second rule)) '-oom))

      (define (id-rest rule)
        (case (first rule)
          ((term) (string->symbol (ss (string-downcase (symbol->string (second rule))) "-rest")))
          (else (symbol-append (second rule) '-rest))))

      (define (id-base subject)
        (case (first subject)
          ((term rule) (second subject))
;;           ((empty) '())
          ;; (else (id-prural (second subject)))
          (else (id-prural subject))
          ))

      (define id-term second)
      (define id-rule second)

      (define (is-term? subject)
        (eq? (car subject) 'term))

      (define (seq-symbol subject)
        (case (first subject)
          ((rule term) (id-rule subject))
;;           ((empty) '())
          ;; (else (id-prural (second subject)))
          (else (id-prural subject))
          ))

      (define (term-has-value? term)
        (assoc-ref lexer-value (second term)))

      (define (seq-return-list subjects)
        (let lp ((subjects subjects)
                 (i 1)
                 (ret '()))
          (if (pair? subjects)
              (let* ((subject (car subjects)))
                ;; (pd subject)
                (lp (cdr subjects)
                    (1+ i)
                    (if (or (not (is-term? subject))
                            (term-has-value? subject))
                        (cons `(cons (quote ,(seq-symbol subject)) ,(string->symbol (si "$#{i}")))
                              ret)
                        ret)))
              (reverse ret))))

      ;; Prural productions are the implicit definitions from grammar
      ;; definition to lalr.
      ;;
      ;;         .-- This is dropped away and only tail is processed (in create).
      ;;        /
      ;;       /    |---------------|
      ;;     (start zom (rule block))
      ;;
      (define (create-prural-productions defs)

        (define ready (list))

        ;;     (zom (rule block))
        (define (create defs ret)

          ;; Add if prural does not already exist.
          ;;
          ;;     (zom (rule topdef))
          ;;
          (define (add-if def productions)
            ;; (pd (list "add-if: " def))
            (if (not (assoc-ref ready (id-prural def)))
                (begin
                  (set! ready (assoc-set! ready (id-prural def) #t))
                  (create (cdr defs)
                          (append (reverse productions) ret)))
                (create (cdr defs) ret)))

          (if (pair? defs)


              (let* ((def (car defs))
                     (type (first def)))

                (case type

                  ;;     (zoo (rule block))
                  ;;       =>
                  ;;     (blocks   ()             : '()
                  ;;               block : $1))
                  ((zoo) (let ((rule (second def)))
                           ;; (pd def)
                           (add-if def
                                   `( (,(id-prural def) () : (quote ())
                                       (,(id-base rule)) : (list $1))) )))

                  ;;     (zom (rule block))
                  ;;       =>
                  ;;     (blocks   ()             : '()
                  ;;               (block blocks) : (cons $1 $2))
                  ((zom) (let ((rule (second def)))
                           ;; (pd def)
                           (add-if def
                                   `( (,(id-prural def) () : (quote ())
                                       (,(id-base rule) ,(id-prural def)) : (cons $1 $2))) )))

                  ;;     (oom (rule block))
                  ;;       =>
                  ;;     (blocks       (block
                  ;;                    block-rest)    : (cons $1 $2)
                  ;;
                  ;;     (block-rest   ()              : '()
                  ;;                   (block)         : (list $1))
                  ((oom) (let ((rule (second def)))
                           (add-if def
                                   `( (,(id-prural def) (,(id-base rule) ,(id-rest rule)) : (cons $1 $2))
                                      (,(id-rest rule) () : (quote ())
                                       (,(id-base rule) ,(id-rest rule)) : (cons $1 $2))) )))

                  ((opt seq) (create (cdr defs)
                                     (append (reverse (create (cddr def)
                                                              '()))
                                             ret)))

                  (else (create (cdr defs) ret))))

              (reverse ret)))

        ;; NOTE: Drop ids from productions (using cdr in map).
        ;; (ppre (car defs))
        (create (map cdr defs) '()))

      ;; Single productions are the explicit definition from grammar
      ;; definition to lalr.
      ;;
      ;;     (start zom (rule block))
      ;;
      (define (create-single-productions defs)

        (define (create-opt-production opt)
          (case (car opt)
            ((empty) '(() : '()))
            (else `((,(id-base opt)) : $1))))

        (let lp ((defs defs)
                 (ret '()))

          (if (pair? defs)

              (let* ((def (car defs))
                     (key (first def))
                     (type (second def)))

                (lp (cdr defs)

                    (case type

                      ;;     (vardef opt (rule clock) (rule reset))
                      ;;       =>
                      ;;     (vardef       (clock)         : $1
                      ;;                   (reset)         : $1)
                      ((opt)
                       ;; (cons `(,key ,@(append-map (lambda (opt) `((,(id-base opt)) : $1 )) (cddr def))) ret))
                       ;; Special support for *empty*.
                       (cons `(,key ,@(append-map create-opt-production (cddr def))) ret))

                      ;;     (clock seq (term LPAR) (term CLOCK) (term ID) (term RPAR))
                      ;;       =>
                      ;;     (clock (LPAR CLOCK ID RPAR) : (cons 'clock (list (cons 'id $3))))
                      ((seq)
                       ;; Optimize the case where sequence produces
                       ;; only one token value. Since it is the only
                       ;; value, it does not have to be labeled
                       ;; separately.
                       (cons `(,key ,(map id-base (cddr def))
                                    :
                                    (cons (quote ,key) ,(let ((seqlst (seq-return-list (cddr def))))
                                                          (if (= (length seqlst) 1)
                                                              (third (first seqlst))
                                                              `(list ,@seqlst)))))
                             ret))

                      ((term rule) (cons `(,key (,(third def)) : $1) ret))

                      ;;       =>
                      ;;     (topstuff (block-zom) : $1)
                      ((zom) (cons `(,key (,(id-prural (cdr def))) : $1) ret))

                      (else ret))))

              (reverse ret))))

      ;;       (pde (create-prural-productions pargen-defs))

      ;; First production is always a single production. All
      ;; productions may include prural productions (also the first).
      (let ((start-def (car pargen-defs))
            (rest-defs (cdr pargen-defs)))
        (append (create-single-productions (list start-def))
                (create-prural-productions pargen-defs)
                (create-single-productions rest-defs))))

    #;
    `(define ,(string->symbol (ss name "-parser"))
    (lalr-parser
    (expect: ,expect)
    ,tokens
    ,@(pargen-defs->lalr-defs pargen-defs)))

    `(define ,parser-id
       (lalr-parser
        ;; NOTE: The "#{}#" syntax is for some reason required in
        ;; order to avoid "#:expect" to appear for Guile.
        (#{expect:}# ,expect)
        ;; Uncomment for parsing table debugging.
        (#{out-table:}# "pargen-tables.txt")
        ,(list-specified (map token-output lexer-def))
        ,@(pargen-defs->lalr-defs pargen-defs)))

    ;;(pargen-defs->lalr-defs pargen-defs)

    ))

;; (pretty-print (pargen-output-parser-module "foobar" 5 grammar))

(define (pargen-output-module-header module-id user-modules)
  (define standard-modules '((system base lalr)
                             (tuile basic)
                             (tuile pr)
                             (tuile gulex)
                             (tuile issues)
                             (srfi srfi-1)))
  (define all-modules (append-map (lambda (module) (list '#:use-module module))
                                  (append standard-modules user-modules)))
  `(define-module ,module-id
     ,@all-modules
     #:export (parse)))


;; (define (pargen-output-lexer-table grammar)
;;   (let* ((lexer-def (car (assoc-ref grammar 'lexer))))
;;     `(define lexer
;;        (quote (,@(map (lambda (def) (list (first def) (second def))) lexer-def))))))


(define (pargen-output-parse-error-fn)
  `(define (parse-error message . args)
     (let ((exn-message
            (apply string-append
                   (cons message
                         (if (and (pair? args)
                                  (lexical-token? (car args)))
                             (let ((token (car args)))
                               (cons (or (ss "\"" (lexical-token-value token) "\"\n")
                                         (ss (lexical-token-category token) "\n"))
                                     (let ((source (lexical-token-source token)))
                                       (if (source-location? source)
                                           (let ((file (source-location-input source))
                                                 (line (source-location-line source))
                                                 (column (source-location-column source)))
                                             (when (and (number? line) (number? column))
                                               (list (si "    \"#{file}\":#{line}:#{(1+ column)}"))))
                                           '()))))
                             args)))))
       (issue-error exn-message))))


(define (pargen-output-lexer-fn grammar)

  (define (pargen-output-lexer-table grammar)
    (let* ((lexer-def (car (assoc-ref grammar 'lexer))))
      `(define lexer
         (quote (,@(map (lambda (def) (list (first def) (second def))) lexer-def))))))

  (define (token-output tokdef)
    (case (third tokdef)
      ((punct) `((,(second tokdef)) (return-and-next ret 'value)))
      ((comment) `((,(second tokdef)) (loop (token-stream-get ts))))
      ((keyword) `((,(second tokdef)) (return-and-next ret 'type)))
      ((operator) `((,(second tokdef)) (return-and-next ret 'type)))
      ((space) `((,(second tokdef)) (loop (token-stream-get ts))))
      ((value) `((,(second tokdef)) (return-and-next ret 'value)))
      ((typeval) `((,(second tokdef)) (return-and-next ret 'typeval)))
      ((error) *unspecified*)
      (else (issue-fatal "Unknown token klass in lexer table"))))

  (define parser-lexer-table (pargen-output-lexer-table grammar))

  (define parser-ts `(define ts (if (file-exists? filename)
                                    (token-stream-open filename
                                                       (gulex-create-lexer-fsm lexer))
                                    (issue-fatal (si "File not found: \"#{filename}\"")))))

  (let* ((lexer-def (car (assoc-ref grammar 'lexer))))
    ;;(pde (unspecified? (token-output (car (last-pair lexer-def)))))
    `(define (make-lexer)
       ,parser-lexer-table
       ,parser-ts
       (define (gulex-tok->lalr-tok tok type)
         (make-lexical-token (token-type tok)
                             (make-source-location (token-file tok)
                                                   (token-line tok)
                                                   (token-column tok)
                                                   -1
                                                   -1)
                             (case type
                               ((type) (token-type tok))
                               ((value) (token-value tok))
                               ((typeval) (cons (token-type tok)
                                                (token-value tok))))))
       (cons ts
             (lambda ()
               (define (return-and-next ret type)
                 (token-stream-get ts)
                 (gulex-tok->lalr-tok ret type))
               (let loop ((ret (token-stream-token ts)))
                 (case (token-type ret)
                   ((eof) '*eoi*)
                   ,@(list-specified (map token-output lexer-def))
                   (else
                    (parse-error (token-type ret) "Unknown character")))))))))


(define (pargen-write-parser-module parser-file
                                    module-id
                                    user-modules
                                    parser-name
                                    parser-expect
                                    grammar)

  (define parser-id (string->symbol (ss parser-name "-parser")))

  (define parser-module-header (pargen-output-module-header module-id user-modules))
;;   (define parser-lexer-table (pargen-output-lexer-table grammar))
  (define parser-error-fn (pargen-output-parse-error-fn))
;;   (define parser-ts `(define ts (if (file-exists? filename)
;;                                     (token-stream-open filename
;;                                                        (gulex-create-lexer-fsm lexer))
;;                                     (issue-fatal (si "File not found: \"#{filename}\"")))))
  (define parser-lexer (pargen-output-lexer-fn grammar))
  (define parser (pargen-output-parser-module parser-id parser-expect grammar))
  (define parser-call `(let* ((ts-and-lexer (make-lexer))
                              (result (,parser-id (cdr ts-and-lexer) parse-error)))
                         (token-stream-close (car ts-and-lexer))
                         result))

  ;;   (define out write)
  (define out pretty-print)

  (print-set! quote-keywordish-symbols #t)

  (call-with-output-file parser-file
    (lambda (port)
      (out parser-module-header port)
;;       (out `(define (parse filename)
;;               ,@(list parser-lexer-table
;;                       parser-error-fn
;;                       parser-ts
;;                       parser-lexer
;;                       parser
;;                       parser-call))
;;            port)
      (out `(define (parse filename)
              ,@(list parser-error-fn
                      parser-lexer
                      parser
                      parser-call))
           port)
      )))



(define grammar `((lexer (("\\("                    LPAR           punct)
                          ("\\)"                    RPAR           punct)

                          (";.*"                    COMMENTLINE    comment)

                          ;; Keywords (5-letter):
                          ("block"                  BLOCK          keyword)
                          ("clock"                  CLOCK          keyword)
                          ("reset"                  RESET          keyword)

                          ("if"                     IF             keyword)
                          ("when"                   WHEN           keyword)
                          ("cond"                   COND           keyword)
                          ("case"                   CASE           keyword)
                          ("else"                   ELSE           keyword)
                          ("begin"                  BEGIN          keyword)

                          ("[a-zA-Z_][a-zA-Z0-9_]*" ID             value)
                          ("[ \t\n]+"               SPACE          space)
                          ("."                      UNKNOWN        error)))

                  (parser ((start block*)
                           (block LPAR
                                  BLOCK
                                  ID
                                  LPAR
                                  vardef*
                                  RPAR
                                  RPAR)
                           (vardef (clock
                                    reset))
                           (clock LPAR CLOCK ID RPAR)
                           (reset LPAR RESET ID RPAR)))))

(when #f

  (pargen-write-parser-module "foopar.scm"
                              '(foopar)
                              '()
                              "foopar"
                              5
                              grammar))
