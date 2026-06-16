(define-module (tuile pargen)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module (tuile pr)
  #:use-module (tuile basic)
  #:use-module (tuile utils)
  #:use-module (tuile hash)
  #:use-module (tuile fmt)
  #:use-module (tuile issues)
  #:export
  (
   pargen-write-parser-module
   pargen-write-lalr-module
   pargen-write-c-parser))


;; Pargen generates an lalr based parser. An example grammar is in the
;; end of the file.
;;
;; Pargen generates the parser from either abstract grammar or lalr
;; grammar.
;;
;; This library should be used from a simple executable.
;;
;; The executable looks something like this (for parse-vlog.scm
;; grammar):
;;
;; Abstract grammar:
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
;; OR
;;
;; Lalr grammar:
;;
;;     (use-modules (tuile pargen))
;;     (define grammar-vlog #f)
;;     (primitive-load "rapid/grammar-vlog.scm")
;;     (pargen-write-lalr-module "rapid/parse-vlog.scm"
;;                                 '(rapid parse-vlog)
;;                                 '()
;;                                 "parse-vlog"
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
;; one - one item
;; oer - item or error


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
(define (org-pargen-import-user-parser pardef)

  (define (import-symbol token)

    (define (chars->symbol chars sep)
      ;; term or rule
      (if (char-set-contains? char-set:upper-case (car chars))
          (list 'term (string->symbol (list->string chars)))
          (list-as-clean 'rule (string->symbol (list->string chars)) sep)))

    (define (->symbol-except-last-char chars sep)
      (chars->symbol (list-head chars (1- (length chars))) sep))

    (define (->symbol-on-error chars sep)
      (chars->symbol (append (list-head chars (1- (length chars)))
                             (string->list "-on-error"))
                     sep))

    (define (->symbol chars)
      (chars->symbol chars #f))

    (let* ((token-name (if (pair? token) (lr0 token) token))
           (token-sep (if (pair? token) (lr1 token) #f))
           (token-str (symbol->string token-name))
           (chars (string->list token-str))
           (first-char (car chars))
           (last-char (car (last-pair chars))))
      (cond
       ((char=? first-char #\-) (list 'empty 'empty))
       ((char=? last-char #\?) (cons 'zoo (->symbol-except-last-char chars token-sep)))
       ((char=? last-char #\*) (cons 'zom (->symbol-except-last-char chars token-sep)))
       ((char=? last-char #\+) (cons 'oom (->symbol-except-last-char chars token-sep)))
       ;; ((char=? last-char #\!) (cons 'oer (->symbol-on-error chars token-sep)))
       ((char=? last-char #\!) (cons 'oer (->symbol-except-last-char chars token-sep)))
       (else (->symbol chars)))))


  (define (import-production production)

    (define (import-rule-name tag)
      (let* ((name (symbol->string tag))
             (head (string-drop-right name 1))
             (tail (string-take-right name 1)))
        (if (string=? tail "!")
            (string->symbol (string-append head "-or-error"))
            tag)))

    (cons
     (import-rule-name (lr0 production))
     ;; (lr0 production)
     (match (lr1 production)
       ( ((seq ...) ...) (cons 'opt (map (lambda (seq) (cons 'seq (map import-symbol seq))) seq)) )
       ( (opt ...)   (cons 'opt (map import-symbol opt)) )
       (else
        (if (= (length (cdr production)) 1)
            (import-symbol (lr1 production))
            (cons 'seq (map import-symbol (cdr production))))))))

  (map import-production pardef))


(define (pargen-import-user-parser pardef)

  (define (expand-symbol sym)
    (define (head->sym str) (string->symbol (string-drop-right str 1)))
    (define (str->sym str) (string->symbol str))
    (let* ((str (symbol->string sym))
           (len (string-length str)))
      (case (string-ref str (1- len))
        ((#\-) (list 'empty 'empty))
        ((#\!) (list 'oer (head->sym str)))
        ((#\?) (list 'zoo (head->sym str)))
        ((#\*) (list 'zom (head->sym str)))
        ((#\+) (list 'oom (head->sym str)))
        (else (cond
               ((char-set-contains? char-set:upper-case (string-ref str 0))
                (list 'term (str->sym str)))
               (else
                (list 'rule (str->sym str))))))))

  (define (import-product product)
    (let ((spec (expand-symbol (if (pair? product) (lr0 product) product)))
          (sep (if (pair? product)
                   (lr1 product)
                   #f)))
      (if sep
          (append spec (list sep))
          spec)))

  (define (import-rule production)

    (let ((rule-sym (expand-symbol (lr0 production))))
      (if (eq? (lr0 rule-sym) 'oer)
          (cons (lr1 rule-sym)
                (cons 'oer (map import-product (lr1 production))))
          (cons (lr1 rule-sym)
                (match (lr1 production)
                  ( ((seq ...) ...) (cons 'opt (map (lambda (seq) (cons 'seq (map import-product seq))) seq)) )
                  ( (opt ...)   (cons 'opt (map import-product opt)) )
                  (else
                   (if (= (length (cdr production)) 1)
                       (cons 'one (list (import-product (lr1 production))))
                       (cons 'seq (map import-product (cdr production))))))))))

  (map import-rule pardef))


;; Terminology:
;;
;;
;;         rule name
;;        /    ,rule type            ,rule product
;;      (port opt (rule port-input) (rule port-output) (rule port-error))
;;      |---------------------- rule -----------------------------------|
;;
;; Complete example of input:
;;
;;     ((start one (rule module))
;;      (module seq
;;              (term KW-MODULE)
;;              (term IDENTIFIER)
;;              (term SEP-LEFTPAREN)
;;              (oom port SEP-COMMA)
;;              (term SEP-RIGHTPAREN)
;;              (term SEP-SEMICOLON)
;;              (zom module-item)
;;              (term KW-ENDMODULE))
;;      (port opt (rule port-input) (rule port-output) (rule port-error))
;;      (port-input seq (term KW-INPUT) (term IDENTIFIER))
;;      (port-output seq (term KW-OUTPUT) (term IDENTIFIER))
;;      (port-error one (rule error))
;;      (module-item opt (oer assign-continuous))
;;      (assign-continuous oer
;;                         (rule assign-continuous)
;;                         (rule assign-continuous-error))
;;      (assign-continuous seq
;;                         (term KW-ASSIGN)
;;                         (term IDENTIFIER)
;;                         (term OP-ASSIGN)
;;                         (term IDENTIFIER)
;;                         (term SEP-SEMICOLON))
;;      (assign-continuous-error seq (rule error) (term SEP-SEMICOLON)))
;;
;; Complete example of output, the rules part:
;;
;;     ((start one (rule module))
;;      (module seq
;;              (term KW-MODULE)
;;              (term IDENTIFIER)
;;              (term SEP-LEFTPAREN)
;;              (rule port-oom)
;;              (term SEP-RIGHTPAREN)
;;              (term SEP-SEMICOLON)
;;              (rule module-item-zom)
;;              (term KW-ENDMODULE))
;;      (port-oom opt (rule port) (seq (rule port) (term SEP-COMMA) (rule port-oom)))
;;      (module-item-zom opt
;;                       (rule module-item)
;;                       (seq (rule module-item) (rule module-item-zom)))
;;      (port opt (rule port-input) (rule port-output) (rule port-error))
;;      (port-input seq (term KW-INPUT) (term IDENTIFIER))
;;      (port-output seq (term KW-OUTPUT) (term IDENTIFIER))
;;      (port-error one (rule error))
;;      (module-item opt (rule assign-continuous-or-error))
;;      (assign-continuous-or-error opt
;;                                  (rule assign-continuous)
;;                                  (rule assign-continuous-error))
;;      (assign-continuous seq
;;                         (term KW-ASSIGN)
;;                         (term IDENTIFIER)
;;                         (term OP-ASSIGN)
;;                         (term IDENTIFIER)
;;                         (term SEP-SEMICOLON))
;;      (assign-continuous-error seq (rule error) (term SEP-SEMICOLON)))
;;
(define (pargen-expand-user-parser pardef user-actions user-types)

  ;; Return products of rule.
  (define (get-products rule)
    (case (lr1 rule)
      ((seq opt oer one) (cddr rule))
      (else (list (cdr rule)))))

  ;; Return:
  ;;     revised product (itself)
  ;;     derived rule, if any
  ;;     derived rule action, if any
  ;;     derived rule c-type, if any
  (define (expand-product product parent rule-type)

    (define (get-c-type item)
      (cons (lr1 item) parent))

    (case (lr0 product)

      ((term) (list product #f #f #f))

      ((rule) (list product
                    #f
                    #f
                    (if (eq? rule-type 'oer)
                        (list (lr1 product) parent)
                        #f)))

      ((oom)
       ;;     (oom port SEP-COMMA)
       (let* ((separator (lr-if product 2))
              (org-tag (lr1 product))
              (new-tag (symbol-append org-tag '-oom)))
         (list
          ;;     (rule port-oom)
          `(rule ,new-tag)
          ;;     (port-oom opt
          ;;               (rule port)
          ;;               (seq (rule port) (term SEP-COMMA) (rule port-oom)))
          `(,new-tag opt
                     (rule ,org-tag)
                     (seq ,@(list-as-clean `(rule ,org-tag)
                                           (if separator
                                               `(term ,separator)
                                               #f)
                                           `(rule ,new-tag))))
          ;;     (port-oom opt (pass $1)
          ;;                   (link $1 $3))
          `(,new-tag opt ((pass $1)
                          ,(if separator
                               `(link $1 $3)
                               `(link $1 $2))))
          ;;     (port-oom port)
          `(,new-tag ,org-tag))))

      ((zom)
       ;;     (zom module-item)
       (let* ((separator (lr-if product 2))
              (org-tag (lr1 product))
              (new-tag (symbol-append org-tag '-zom)))
         (list
          ;;     (rule module-item-zom)
          `(rule ,new-tag)
          ;;     (module-item-zom opt
          ;;                      (rule module-item)
          ;;                      (seq (rule module-item) (rule module-item-zom)))
          `(,new-tag opt
                     ;; (rule ,org-tag)
                     (empty empty)
                     (seq ,@(list-as-clean `(rule ,org-tag)
                                           (if separator
                                               `(term ,separator)
                                               #f)
                                           `(rule ,new-tag))))
          ;;     (module-item-zom opt (pass $1)
          ;;                          (link-if $1 $2))
          `(,new-tag opt ((empty)
                          ,(if separator
                               '(link-if $1 $3)
                               '(link-if $1 $2))))
          ;;     (module-item-zom module-item)
          `(,new-tag ,org-tag))))

      ((oer)
       ;;     (oer assign-continuous)
       (let* ((org-tag (lr1 product))
              (new-tag (symbol-append org-tag '-or-error)))
         (list
          ;;     (rule assign-continuous-or-error)
          `(rule ,new-tag)
          #f
          ;;     (assign-continuous-or-error (skip))
          `(,new-tag (errok))
          ;;     (assign-continuous-or-error assign-continuous)
          `(,new-tag ,org-tag))))

      ))


  ;;      (module seq
  ;;              (term KW-MODULE)
  ;;              (term IDENTIFIER)
  ;;              (term SEP-LEFTPAREN)
  ;;              (oom port SEP-COMMA)
  ;;              (term SEP-RIGHTPAREN)
  ;;              (term SEP-SEMICOLON)
  ;;              (zom rule module-item)
  ;;              (term KW-ENDMODULE))
  (define (expand rules user-types)

    (define (cons-if item lst) (if item (cons item lst) lst))
    ;; (define (append-if item lst) (if item (cons item lst) lst))

    (define (derive-c-type item c-types)
      (if (assoc-ref c-types item)
          #f
          (if (eq? item 'start)
              ;; Don't add start to types.
              #f
              (list item item))))

    (define (contains-error? products)
      (filter (lambda (item) (and (pair? (cdr item))
                                  (eq? (lr1 item) 'error)))
              products))

    (let lp-rule ((rules rules)
                  (all-rules '())
                  (actions '())
                  (c-types (reverse user-types)))
      (lp-if rule
             (case (lr1 rule)
               ((seq opt oer)
                ;;      (module seq
                ;;              ^
                (let lp-product ((products (get-products rule))
                                 (revised-products '())
                                 (new-rules '())
                                 (new-actions '())
                                 (gen-c-types '()))
                  (if (pair? products)
                      (let* ((product (car products))
                             (expansion (expand-product product (lr0 rule) (lr1 rule))))
                        (lp-product (cdr products)
                                    (cons (lr0 expansion) revised-products)
                                    (cons-if (lr1 expansion) new-rules)
                                    (cons-if (lr2 expansion) new-actions)
                                    (cons-if (lr3 expansion) gen-c-types)))
                      (let ((new-name (if (eq? (lr1 rule) 'oer)
                                          (symbol-append (lr0 rule) '-on-error)
                                          (lr0 rule))))
                        (lp-rule (cdr rules)
                                 (append new-rules
                                         (list (append (if (eq? (lr1 rule) 'oer)
                                                           (list new-name 'opt)
                                                           (list-head rule 2))
                                                      (reverse revised-products)))
                                         all-rules)
                                 (cons-if (case (lr1 rule)
                                            ((opt oer) (list new-name 'opt
                                                             (map (lambda (product)
                                                                    (list 'pass '$1))
                                                                  (get-products rule))))
                                            (else (cond
                                                   ((contains-error? (get-products rule))
                                                    (if (not (assoc-ref actions (lr0 rule)))
                                                        (list (lr0 rule) '(errok))
                                                        #f))
                                                   (else #f))))
                                          (append new-actions
                                                  actions))
                                 (let ((cur-types (append gen-c-types c-types)))
                                   (cons-if (derive-c-type (lr0 rule) cur-types) cur-types)))))))
               (else (lp-rule (cdr rules)
                              (cons rule all-rules)
                              actions
                              (cons-if (derive-c-type (lr0 rule) c-types) c-types))))
             (list (reverse all-rules)
                   (reverse actions)
                   (reverse c-types)))))

  (define (derive-parents rules)

    (define (rule-name product)
      (match product
        ('error #f)
        (('term term-name) #f)
        (('rule rule-name) rule-name)
        (('seq parts ...) #f)
        (('empty pair) #f)
        (else (error (ss "no match " (ds product))))
        ))

    (let ((parents (let ((parents (make-hash-table)))
                     (let lp ((rules rules))
                       (lp-if rule
                              (begin
                                (case (lr1 rule)
                                  ((seq opt one)
                                   (let ((rule-names (list-clean (map rule-name (get-products rule)))))
                                     (for-each (lambda (name)
                                                 (hash-set! parents name (lr0 rule)))
                                               rule-names)))
                                  (else (error "Unknown rule type...")))
                                (lp (cdr rules)))
                              (hash->alist parents))))))

      parents))

  (define (sort-actions rules actions)
    (let lp ((rules rules)
             (ret '()))
      (lp-if rule
             (lp (cdr rules)
                 (cons (assoc (car rule) actions) ret))
             (reverse ret))))


  (let* ((res (expand pardef user-types))
         (all-rules (lr0 res))
         (derived-actions (lr1 res))
         (all-c-types (lr2 res))
         (parents (derive-parents all-rules))
         (sorted-actions (sort-actions all-rules (append user-actions derived-actions))))
    (list (cons 'parser all-rules)
          (cons 'action sorted-actions)
          (cons 'types  all-c-types)    ; TODO: add more
          (cons 'parents parents)
          )))




;;     Terminal     = term
;;     Non-terminal = rule
;;     Subject      = term | rule | prural
(define (pargen-output-parser-module parser-id expect grammar)

  (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
         (lexer-value (map (lambda (ld) (cons (lr1 ld)
                                              (or (eq? (lr2 ld) 'value)
                                                  (eq? (lr2 ld) 'typeval))))
                           lexer-def))
         ;; (tokens (map lr1 lexer-def))
         (parser-def (car (assoc-ref grammar 'parser)))
         (pargen-defs (pargen-import-user-parser parser-def)))

    (define (token-output tokdef)
      (case (lr2 tokdef)
        ((comment space error) *unspecified*)
        (else (lr1 tokdef))))

    (define (pargen-defs->lalr-defs pargen-defs)

      (define (id-prural rule)
        ;; (pde rule)
        (case (lr0 rule)
          ((term) (string->symbol (ss (string-downcase (symbol->string (lr1 rule))) "s")))
          ((zoo) (id-prural-zoo rule))
          ((zom) (id-prural-zom rule))
          ((oom) (id-prural-oom rule))))

      (define (id-prural-zoo rule)
        (symbol-append (lr1 (lr1 rule)) '-zoo))

      (define (id-prural-zom rule)
        (symbol-append (lr1 (lr1 rule)) '-zom))

      (define (id-prural-oom rule)
        (symbol-append (lr1 (lr1 rule)) '-oom))

      (define (id-rest rule)
        (case (lr0 rule)
          ((term) (string->symbol (ss (string-downcase (symbol->string (lr1 rule))) "-rest")))
          (else (symbol-append (lr1 rule) '-rest))))

      (define (id-base subject)
        (case (lr0 subject)
          ((term rule) (lr1 subject))
;;           ((empty) '())
          ;; (else (id-prural (lr1 subject)))
          (else (id-prural subject))
          ))

      (define id-term lr1)
      (define id-rule lr1)

      (define (is-term? subject)
        (eq? (car subject) 'term))

      (define (seq-symbol subject)
        (case (lr0 subject)
          ((rule term) (id-rule subject))
;;           ((empty) '())
          ;; (else (id-prural (lr1 subject)))
          (else (id-prural subject))
          ))

      (define (term-has-value? term)
        (assoc-ref lexer-value (lr1 term)))

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
                     (type (lr0 def)))

                (case type

                  ;;     (zoo (rule block))
                  ;;       =>
                  ;;     (blocks   ()             : '()
                  ;;               block : $1))
                  ((zoo) (let ((rule (lr1 def)))
                           ;; (pd def)
                           (add-if def
                                   `( (,(id-prural def) () : (quote ())
                                       (,(id-base rule)) : (list $1))) )))

                  ;;     (zom (rule block))
                  ;;       =>
                  ;;     (blocks   ()             : '()
                  ;;               (block blocks) : (cons $1 $2))
                  ((zom) (let ((rule (lr1 def)))
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
                  ((oom) (let ((rule (lr1 def)))
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
                     (key (lr0 def))
                     (type (lr1 def)))

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
                                                              (lr2 (lr0 seqlst))
                                                              `(list ,@seqlst)))))
                             ret))

                      ((term rule) (cons `(,key (,(lr2 def)) : $1) ret))

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
        (expect* ,expect)
        ;; Uncomment for parsing table debugging.
        (out-table* "pargen-tables.txt")
        ,(list-specified (map token-output lexer-def))
        ,@(pargen-defs->lalr-defs pargen-defs)))

    ;;(pargen-defs->lalr-defs pargen-defs)

    ))


(define (pargen-output-lalr-module parser-id grammar)

  (define (token-output tokdef)
    (case (lr2 tokdef)
      ((comment space error) *unspecified*)
      (else (lr1 tokdef))))

  (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
         (lexer-value (map (lambda (ld) (cons (lr1 ld)
                                              (or (eq? (lr2 ld) 'value)
                                                  (eq? (lr2 ld) 'typeval))))
                           lexer-def))
         (parser-def (assoc-ref grammar 'parser))
         (parser-opts (map (lambda (item)
                             (list (string->symbol
                                    (string-append
                                     (symbol->string (lr0 item))
                                     "*"
                                     ;; ":"
                                     ))
                                   (lr1 item)))
                           (lr0 parser-def)))
         (parser-tokens (list-specified (map token-output lexer-def)))
         (parser-rules (lr1 parser-def)))

    `(define ,parser-id
       (lalr-parser
        ,@parser-opts
        ,parser-tokens
        ,@parser-rules))))


;; (pretty-print (pargen-output-parser-module "foobar" 5 grammar))

(define (pargen-output-module-header module-id user-modules)
;;   (define standard-modules '((system base lalr)
;;                              (tuile basic)
;;                              (tuile pr)
;;                              (tuile gulex)
;;                              (tuile issues)
;;                              (srfi srfi-1)))
  (define standard-modules '((tuile lalr)
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
;;        (quote (,@(map (lambda (def) (list (lr0 def) (lr1 def))) lexer-def))))))


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
         (quote (,@(map (lambda (def) (list (lr0 def) (lr1 def))) lexer-def))))))

  (define (token-output tokdef)
    (case (lr2 tokdef)
      ((punct) `((,(lr1 tokdef)) (return-and-next ret 'value)))
      ((comment) `((,(lr1 tokdef)) (loop (token-stream-get ts))))
      ((keyword) `((,(lr1 tokdef)) (return-and-next ret 'type)))
      ((operator) `((,(lr1 tokdef)) (return-and-next ret 'type)))
      ((space) `((,(lr1 tokdef)) (loop (token-stream-get ts))))
      ((value) `((,(lr1 tokdef)) (return-and-next ret 'value)))
      ((typeval) `((,(lr1 tokdef)) (return-and-next ret 'typeval)))
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
       ;; (token-stream-show-token #t)
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
  (define parser-error-fn (pargen-output-parse-error-fn))
  (define parser-lexer (pargen-output-lexer-fn grammar))
  (define parser (pargen-output-parser-module parser-id parser-expect grammar))
  (define parser-call `(let* ((ts-and-lexer (make-lexer))
                              (result (,parser-id (cdr ts-and-lexer) parse-error)))
                         (token-stream-close (car ts-and-lexer))
                         result))

  (define out pretty-print)

  ;; (print-set! quote-keywordish-symbols #t)

  (call-with-output-file parser-file
    (lambda (port)
      (out parser-module-header port)
      (out `(define (parse filename)
              ,@(list parser-error-fn
                      parser-lexer
                      parser
                      parser-call))
           port)
      )))


(define (pargen-write-lalr-module parser-file
                                  module-id
                                  user-modules
                                  parser-name
                                  grammar)

  (define parser-id (string->symbol (ss parser-name "-parser")))

  (define parser-module-header (pargen-output-module-header module-id user-modules))
  (define parser-error-fn (pargen-output-parse-error-fn))
  (define parser-lexer (pargen-output-lexer-fn grammar))
  (define parser (pargen-output-lalr-module parser-id grammar))
  (define parser-call `(let* ((ts-and-lexer (make-lexer))
                              (result (,parser-id (cdr ts-and-lexer) parse-error)))
                         (token-stream-close (car ts-and-lexer))
                         result))

  (define out pretty-print)

  ;; (print-set! quote-keywordish-symbols #t)

  (call-with-output-file parser-file
    (lambda (port)
      (out parser-module-header port)
      (out `(define (parse filename)
              ,@(list parser-error-fn
                      parser-lexer
                      parser
                      parser-call))
           port)
      )))


(define (c-fy str)
  (string-replace-substring (symbol->string str) "-" "_"))


;; Parameter info:
;;
;;     parser-name     "parsve"
;;     lexpar-name     "lexpar"
;;     lexer-info      <see below>
;;     parse-info      <see below>
;;     grammar         <see below>
;;
;; lexer-info:
;;     (list (cons 'file "verilog.l")
;;           (cons 'headers '("<stdio.h>" "<sys/param.h>"))
;;           (cons 'return  "RET_TOKEN"))
;;
;; parse-info:
;;     (list (cons 'file "verilog.y")
;;           (cons 'headers '())
;;           (cons 'return  "RET_TOKEN"))
;;
(define (pargen-write-c-parser parser-name
                               lexpar-name
                               output-dir
                               lexer-info
                               parse-info
                               grammar)

  (define (gen-file-path filename)
    (ss output-dir "/" filename))

  (define (gen-list-of-used-types grammar-type-table)
    (uniquify identity (map lr1 grammar-type-table)))

  (define (get-product-type product types)
    (if (eq? (lr0 product) 'term)
        'token
        (car (assoc-ref types (lr1 product)))))

  ;;     (name $2)     $1
  ;;           ^       ^
  (define (get-cmd-arg-index-def arg-def)
    (if (list? arg-def) (lr1 arg-def) arg-def))

  ;;     (name $2)     $1
  ;;     ^             !
  (define (get-cmd-arg-def-name arg-def)
    (if (list? arg-def) (lr0 arg-def) #f))

  (define (cmd-arg-index-def->number index-def)
    (1- (string->number (substring (symbol->string index-def) 1))))

  (define (get-cmd-arg-index cmd-arg)
    ;; (ppr cmd-arg)
    (cmd-arg-index-def->number (get-cmd-arg-index-def cmd-arg)))


  (define (rule-name rule) (lr0 rule))

  (define (rule-products rule) (cddr rule))

  (define (rule-type rule) (lr1 rule))

  (define (rule-is-opt? rule) (eq? (rule-type rule) 'opt))


  (define (rule-c-type rule types)
    (let ((name (rule-name rule)))
      (car (assoc-ref types name))))


  (define (action-is-opt? action)
    (symbol? (lr1 action)))

  (define (action-cmd action)
    (if (action-is-opt? action)
        (lr0 (lr2 action))
        (lr0 (cdr action))))

  ;; (module (create (name $2) (port-list $4) (module-item-zom $7)))
  ;; (module-item opt ((create (item $1))))
  (define (action-cmd-list action)
    (if (action-is-opt? action)
        (lr2 action)
        (cdr action)))

  (define (action-has-create? action)
    (let ((cmd-list (action-cmd-list action)))
      (find (lambda (cmd) (eq? (lr0 cmd) 'create)) cmd-list)))

  ;;     (arg->c-type-and-name (lambda (cmd-arg)
  ;;                             (let* ((index (get-cmd-arg-index cmd-arg))
  ;;                                    (product (lr products index))
  ;;                                    (arg-type (get-product-type product types)))
  ;;                               (if (and is-opt? (not parent-match?))
  ;;                                   (cons "item" (get-cmd-arg-def-name cmd-arg))
  ;;                                   (cons (c-fy arg-type) (c-fy (get-cmd-arg-def-name cmd-arg)))))))
  (define (action-cmd-arg-c-type-and-name cmd-arg action rule types)
    (let* ((index (get-cmd-arg-index cmd-arg))
           (is-opt? (rule-is-opt? rule))
           (parent-match? (parent-type-matches-options? rule types))
           (product (lr (rule-products rule) index))
           (arg-type (get-product-type product types)))
      (if (and is-opt? (not parent-match?))
          (cons "item" (c-fy (get-cmd-arg-def-name cmd-arg)))
          (cons (c-fy arg-type) (c-fy (get-cmd-arg-def-name cmd-arg))))))


  ;;   (port opt
  ;;         (rule port-input)
  ;;         (rule port-output)
  ;;         (rule port-error))
  ;;
  ;;   (assign-continuous-on-error opt
  ;;                               (rule assign-continuous)
  ;;                               (rule assign-continuous-error))
  ;;
  (define (parent-type-matches-options? rule types)
    (let ((parent-type (get-product-type `(rule ,(lr0 rule)) types)))
      (and (eq? (lr1 rule) 'opt)
           (all (map (lambda (opt)
                       (let ((product-type (get-product-type opt types)))
                         (eq? parent-type product-type)))
                     (cddr rule))))))


  (define (make-c-struct-field-formatter indent type-column)
    (let ((ltn (1+ (apply max (map string-length type-column)))))
      (lambda (f1 . rest)
        (apply fmt (append `((ind ,indent) (lal ,ltn ,f1)) rest)))))

  (define (->string item)
    (if (string? item)
        item
        (object->string item)))

;;   (define (make-postfix-fn postfix)
;;     (lambda (item) (string-append (->string item) postfix)))
;;
;;   (define (make-infix-fn head tail)
;;     (lambda (item) (string-append (->string head) item (->string tail))))

  (define (make-postfix-fn postfix)
    (lambda (item) (string-append item postfix)))

  (define (make-infix-fn head tail)
    (lambda (item) (string-append head item tail)))

  (define (format-c-create-function fun-type fun-tag c-types c-names term)
    (let* ((type-and-fun (si "parse_#{(c-fy fun-type)}_t parse_#{(c-fy fun-tag)}_create( "))
           (type-and-fun-len (string-length type-and-fun))
           (fmt' (make-c-struct-field-formatter type-and-fun-len c-types))
           (lines (map fmt' c-types c-names))
           (head-fn (make-postfix-fn ","))
           (last-fn (make-postfix-fn term))
           (fix-lines-pre (map-with-special-last head-fn last-fn lines))
           (fix-lines (map (lambda (str) (string-replace-substring str "token_t" "token_s"))
                                fix-lines-pre)))
      (cons (si "#{type-and-fun}#{(string-trim (car fix-lines))}")
            (cdr fix-lines))))


  ;; These variables are shared between output-* functions.
  (define pargen-defs #f)
  (define expansions #f)
  (define list-of-types #f)
  (define list-of-type-names #f)
  (define active-tokens #f)

  (define (output-prepare! parser-name
                           lexpar-name
                           parse-info
                           grammar)
    (let ((lexer-def (car (assoc-ref grammar 'lexer)))
          (parse-def (car (assoc-ref grammar 'parser)))
          (ar assoc-ref))

      (set! pargen-defs (pargen-import-user-parser parse-def))
      (set! expansions (pargen-expand-user-parser pargen-defs
                                                  (car (assoc-ref grammar 'action))
                                                  (car (assoc-ref grammar 'types))))
      (set! list-of-types (gen-list-of-used-types (ar expansions 'types)))
      (set! list-of-type-names (map c-fy list-of-types))
      (set! active-tokens (let lp ((ldefs lexer-def)
                                   (ret '()))
                            (lp-if ldef
                                   (if (case (lr2 ldef)
                                         ((keyword value operator) #t)
                                         (else #f))
                                       (lp (cdr ldefs)
                                           (cons (c-fy (lr1 ldef)) ret))
                                       (lp (cdr ldefs) ret))
                                   (reverse (uniquify identity ret)))))))


  (define (output-lexer parser-name
                        lexer-info
                        grammar)

    (define (linfo key) (assoc-ref lexer-info key))

    (with-output-to-file (gen-file-path (linfo 'file))
      ;; with-output-to-port (current-output-port)
      (lambda ()

        (for-each (lambda (header)
                    (pr (si "#include #{header}")))
                  (linfo 'headers))

        (pr (si "#include \"#{parser-name}.h\""))
        (pr (si "#include \"parse.h\""))
        (pr (si ""))
        ;; "si" uses backslashes as well, hence we would need double
        ;; backslashes, if didn't skip it.
        ;;     (pr (si "#define YY_USER_ACTION \\\\"))
        (pr "#define YY_USER_ACTION \\")
        (pr "yylloc->first_column = yycolumn; \\")
        (pr "yylloc->last_column = yycolumn + yyleng - 1; \\")
        (pr "yycolumn += yyleng;")
        (pr (si ""))
        (pr (si "%}"))
        (pr (si ""))
        (pr (si "%option reentrant"))
        (pr (si "%option bison-bridge"))
        (pr (si "%option bison-locations"))
        (pr (si "%option noyywrap"))
        (pr (si "%option extra-type=\"#{lexpar-name}_t\""))
        (pr (si "%option fast"))
        (pr (si ""))
        (pr (si "%%"))
        (pr (si ""))

        ;; TODO: Make automatic regex quotation for patterns that need
        ;; extra quotation specifically for flex, but not for others.
        ;; This makes sharing the regex patterns easier between
        ;; generated outputs.

        (let* ((rules (car (assoc-ref grammar 'lexer)))
               (longest-regexp (apply max
                                      (map string-length (map lr0 rules))))
               (width (+ longest-regexp 2)))
          ;; ("\"module\""                   KW-MODULE      keyword)
          (for-each (lambda (rule)
                      (pr (fmt `(lal ,width ,(lr0 rule))
                               (case (lr2 rule)
                                 ((space comment) (si "{ /* Ignore #{(lr1 rule)} */ }"))
                                 (else (si "{ #{(linfo 'return)}( #{(c-fy (lr1 rule))} ); }"))))))
                    rules)
          #f)

        ;; (pr (si ".               { printf( \"Unknown character: %s\\n\", yyget_text( yyscanner ) ); }"))
        (pr (si ""))
        (pr (si "%%"))

        )))


  ;; Evaluate as elisp:
  ;; (defalias 'ti-default-macro (kmacro "TAB ( p r SPC ( s i SPC \" C-e \" ) ) C-f"))


  (define (output-parse parser-name
                        lexpar-name
                        parse-info
                        grammar)

    (define ar assoc-ref)

    (define (linfo key) (assoc-ref parse-info key))


    (define (format-rule rule actions types)

      (define (format-rule-name rule-name)
        (list (si "#{(c-fy rule-name)}:")))

      (define (format-product-item item)
        (let ((tag (lr1 item)))
          (if (eq? tag 'empty)
              "%empty"
              (c-fy tag))))

      (define (base-indent item) (fmt `(ind 4) item))

      (define (format-product-action rule-name rule-type products action)

        (define (option-cast? rule-name rule-type products cmd-args)
          ;; (define (arg->index arg) (1- (string->number (substring (symbol->string arg) 1))))
          (define (get-type-by-arg arg)
            (ar types (lr1 (lr products (cmd-arg-index-def->number arg)))))
          (and
           (eq? rule-type 'opt)
           (not (eq? (ar types rule-name)
                     (get-type-by-arg (lr0 cmd-args))))))

        (append (list "{")
                (map base-indent
                     ;;     (module (create (name $2) (port-list $4) (module-item-zom $7)))
                     (let* ((cmd (lr1 action))
                            (cmd-name (lr0 cmd))
                            (cmd-arg-defs (cdr cmd))
                            (cmd-args (map get-cmd-arg-index-def cmd-arg-defs)))
                       (case cmd-name
                         ((root) (list (si "#{lexpar-name}->root = $1;")))
                         ((create)
                          (if (option-cast? rule-name rule-type products cmd-args)
                              (list (fmt `(cat ,(si "$$ = parse_#{(c-fy rule-name)}_create( #{lexpar-name}")
                                               ,@(map (lambda (idx) (si ", (parse_item_t)#{idx}")) cmd-args)
                                               ,(si " );"))))
                              (list (fmt `(cat ,(si "$$ = parse_#{(c-fy rule-name)}_create( #{lexpar-name}")
                                               ,@(map (lambda (idx) (si ", #{idx}")) cmd-args)
                                               ,(si " );"))))))
                         ((pass) (list (si "$$ = #{(lr0 cmd-args)};")))
                         ((link) (let ((a1 (lr0 cmd-args))
                                       (a2 (lr1 cmd-args)))
                                   (list (si "#{a1}->next = #{a2};")
                                         (si "$$ = #{a1};"))))
                         ((link-if) (let ((a1 (lr0 cmd-args))
                                          (a2 (lr1 cmd-args)))
                                      (list (si "if ( #{a1} ) {")
                                            (si "    #{a1}->next = #{a2};")
                                            (si "    $$ = #{a1};")
                                            (si "} else {")
                                            (si "    $$ = NULL;")
                                            (si "}"))))
                         ((empty) (list (si "$$ = NULL;")))
                         ((clear) (list (si "yyclearin;")))
                         ((errok) (list (si "yyerrok;")
                                        (si "$$ = NULL;"))))))
                (list "}")))


      (define (format-options options)
        (cons (car options)
              (map (lambda (option) (cons (si "| #{(car option)}") (cdr option)))
                   (cdr options))))

      (define (format-seq rule-name rule-type products action)
        (define (ind line) (fmt '(ind 4) line))
        (append (list (fmt `(gap 1 ,@(map format-product-item products))))
                (format-product-action rule-name rule-type products action)))

      (define (format-products rule-name rule-type products action)
        ;; (ppr (list "format-products: " rule-name rule-type products action))
        (case rule-type
          ((seq one) (map base-indent (format-seq rule-name rule-type products action)))
          ((opt)
           ;; (ppre (list "format-products: " rule-name rule-type products action))
           (let ((options (map (lambda (product action)
                                 (case (lr0 product)
                                   ((seq) (format-seq rule-name rule-type (cdr product) (list #f action)))
                                   (else (format-seq rule-name rule-type (list product) (list #f action)))))
                               products (lr2 action))))
             (map base-indent (flatten-1 (format-options options)))))
          (else (error (ss "unknown format-products: " rule-type)))))


      (define (format-terminator)
        (list (si "    ;")))

      (let ((rule-name (lr0 rule))
            (rule-type (lr1 rule))
            (products (cddr rule)))
        (append (format-rule-name rule-name)
                (format-products rule-name rule-type products (assoc rule-name actions))
                (format-terminator))))


    (with-output-to-file (gen-file-path (linfo 'file))
      ;; with-output-to-port (current-output-port)
      (lambda ()
        (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
               (parse-def (car (assoc-ref grammar 'parser)))
               ;; Now in output-prepare!
               ;;                (pargen-defs (pargen-import-user-parser parse-def))
               ;;                (expansions (pargen-expand-user-parser pargen-defs
               ;;                                                       (car (assoc-ref grammar 'action))
               ;;                                                       (car (assoc-ref grammar 'types))))
               ;;                (list-of-types (gen-list-of-used-types (ar expansions 'types)))
               ;;                (active-tokens (let lp ((ldefs lexer-def)
               ;;                                        (ret '()))
               ;;                                 (lp-if ldef
               ;;                                        (if (case (lr2 ldef)
               ;;                                              ((keyword value operator) #t)
               ;;                                              (else #f))
               ;;                                            (lp (cdr ldefs)
               ;;                                                (cons (c-fy (lr1 ldef)) ret))
               ;;                                            (lp (cdr ldefs) ret))
               ;;                                        (reverse (uniquify identity ret)))))
               )

          ;; (ppre pargen-defs)
          ;; (ppre expansions)
          ;; (ppre list-of-types)
          ;; (ppr (ar expansions 'types))

          (for-each (lambda (header)
                      (pr (si "#include #{header}")))
                    (linfo 'headers))

          (pr (si "%{"))
          (pr (si "#include \"#{parser-name}.h\""))
          (pr (si "#include \"lexer.h\""))
          (pr (si ""))
          (pr (si "int  yylex( YYSTYPE* yylval_param, YYLTYPE* yylloc_param, void* yyscanner );"))
          (pr (si "void yyerror( YYLTYPE* yylloc_param, #{lexpar-name}_t #{lexpar-name}, void* scanner, const char* msg );"))
          (pr (si ""))
          (pr (si "%}"))
          (pr (si ""))
          (pr (si "%locations"))
          (pr (si "%define api.pure full"))
          (pr (si "%define parse.error detailed"))
          (pr (si "%lex-param {void *scanner}"))
          (pr (si "%parse-param {#{lexpar-name}_t #{lexpar-name}}"))
          (pr (si "%parse-param {void *scanner}"))
          (pr (si ""))

          ;; Union:
          (pr (si "%union {"))
          (let* ((c-types (cons "int" (map
                                       ;; (lambda (c-type) (si "parse_#{c-type}_t"))
                                       (make-infix-fn "parse_" "_t")
                                       (cons "token" list-of-type-names))))
                 (c-names (map (lambda (item) (si "#{item};"))
                               (append (list "ival" "token") list-of-type-names)))
                 (fmt' (make-c-struct-field-formatter 4 c-types)))
            (for-each (lambda (c-type c-name)
                        (pr (fmt' c-type c-name)))
                      c-types c-names))
          (pr (si "}"))

          ;; Tokens:
          (pr (si ""))
          (for-each (lambda (token) (pr (si "%token <parse_token> #{token}"))) active-tokens)
          (pr (si ""))

          ;; ((port-input port)
          ;;  (port-output port)
          ;;  (port-error port)
          ;;  (port-oom port)
          ;;  (module-item-zom module-item)
          ;;  (module module)
          ;;  (port port)
          ;;  (assign-continuous-or-error assign-continuous)
          ;;  (module-item module-item)
          ;;  (assign-continuous assign-continuous)
          ;;  (assign-continuous-error assign-continuous))

          ;; Rule types:
          ;;          (pr (si "%type <parse_module> module"))
          ;;          (pr (si "%type <parse_port> port_list port"))
          ;;          (pr (si "%type <parse_module_item> module_item_list module_item"))
          ;;          (pr (si "%type <parse_assign_continuous> assign_continuous"))
          (for-each (lambda (tdef)
                      (pr (si "%type <parse_#{(c-fy (lr1 tdef))}> #{(c-fy (lr0 tdef))}")))
                    (ar expansions 'types))

          (pr (si ""))

          (let ((out (lambda (prio)
                       (if (> (length prio) 2)
                           (fmt (lr0 prio) " " `(gap 1 ,@(map c-fy (cdr prio))))
                           (fmt (lr0 prio) " " (c-fy (lr1 prio)))))))
            (for-each (lambda (prio)
                        (pr (si "%#{(out prio)}")))
                      (car (ar grammar 'priority))))

          ;;          (pr (si "/* Precedence to handle expression ambiguity */"))
          ;;          (pr (si "%left SEP_SEMICOLON SEP_COLON"))
          ;;          (pr (si ""))
          ;;          (pr (si "%left OP_LOG_AND"))
          ;;          (pr (si "%left OP_BIT_ORI"))
          ;;          (pr (si "%left OP_BIT_XOR"))
          ;;          (pr (si "%left OP_BIT_AND"))
          ;;          (pr (si "%left OP_ADD OP_SUB"))

          (pr (si ""))
          (pr (si "%%"))
          (pr (si ""))

          ;; (format-rule rule actions)
          (let ((actions (ar expansions 'action))
                (types (ar expansions 'types)))
            (for-each (lambda (rule)
                        (pl (format-rule rule actions types))
                        (pr))
                      (ar expansions 'parser)))

          (pr (si "%%"))))))


  (define (output-lexpar-h parser-name
                           lexpar-name
                           parse-info
                           grammar)

    (define ar assoc-ref)

    (define (linfo key) (assoc-ref parse-info key))

    (with-output-to-file (gen-file-path (si "#{lexpar-name}.h"))
     ;; with-output-to-port (current-output-port)
     (lambda ()
       (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
              (parse-def (car (assoc-ref grammar 'parser))))


         (pr (si "#ifndef #{(string-upcase lexpar-name)}_H"))
         (pr (si "#define #{(string-upcase lexpar-name)}_H"))
         (pr (si ""))
         (pr (si "typedef void* yyscan_t;"))
         (pr (si ""))
         (pr (si "struct YYLTYPE;"))
         (pr (si "typedef struct YYLTYPE YYLTYPE;"))
         (pr (si ""))
         (pr (si "union YYSTYPE;"))
         (pr (si "typedef union YYSTYPE YYSTYPE;"))
         (pr (si ""))
         (pr (si ""))
         (pr (si "pl_struct( parse_token )"))
         (pr (si "{"))
         (pr (si "    int    token;"))
         (pr (si "    plsr_s value;"))
         (pr (si "    int    place;"))
         (pr (si "};"))
         (pr (si ""))

         ;;          (pr (si "pl_enum( parse_type ){ PARSE_NONE,        PARSE_MODULE,      PARSE_PORT_INPUT,"))
         ;;          (pr (si "PARSE_PORT_OUTPUT, PARSE_MODULE_ITEM, PARSE_ASSIGN_CONTINUOUS };"))

         (pr (si "pl_enum( parse_type ) {"))
         (pl (map-except-last (lambda (tag) (ss "    PARSE_" (string-upcase (c-fy tag))))
                              ;; (lambda (tag) (ss tag ","))
                              (make-postfix-fn ",")
                              (cdr (map car (ar expansions 'parser)))))
         (pr (si "};"))

         (pr (si ""))
         (pr (si ""))
         (pr (si "pl_struct( parse_item )"))
         (pr (si "{"))
         (pr (si "    parse_type_t type;"))
         (pr (si "};"))
         (pr (si ""))
         (pr (si ""))

         ;;          (pr (si "pl_struct_type( parse_module );"))
         ;;          (pr (si "pl_struct_type( parse_port );"))
         ;;          (pr (si "pl_struct_type( parse_module_item );"))
         ;;          (pr (si "pl_struct_type( parse_assign_continuous );"))

         (for-list (type list-of-type-names)
                   (pr (si "pl_struct_type( parse_#{type} );")))

         ;;          (pr (si "pl_struct_type( parse_module );"))

         (pr (si ""))
         (pr (si ""))

         ;;          (pr (si "pl_struct_body( parse_module )"))
         ;;          (pr (si "{"))
         ;;          (pr (si "parse_type_t        type;"))
         ;;          (pr (si "parse_token_t       name;"))
         ;;          (pr (si "parse_port_t        port_list;"))
         ;;          (pr (si "parse_module_item_t module_item_list;"))
         ;;          (pr (si "};"))

         ;; here
         ;;          (ppre list-of-types)

         ;;          ((module (seq (term KW-MODULE)
         ;;                        (term IDENTIFIER)
         ;;                        (term SEP-LEFTPAREN)
         ;;                        (rule port-oom)
         ;;                        (term SEP-RIGHTPAREN)
         ;;                        (term SEP-SEMICOLON)
         ;;                        (rule module-item-zom)
         ;;                        (term KW-ENDMODULE))
         ;;                   (create (name $2) (port-list $4) (module-item-zom $7))
         ;;                   ))

         ;; call for type: (get-product-type product types)

         ;; (ppre (ar expansions 'parser))

         ;; TODO: Create function that checks if parent and children
         ;; have the same type.

         (for-list
          (type list-of-types)
          ;; (ppr type)
          (let* ((original-rule (assoc type (ar expansions 'parser)))
                 (types (ar expansions 'types))
                 ;; rule : (port opt (rule port-input) (rule port-output) (rule port-error))
                 ;; (is-opt? (eq? (lr1 original-rule) 'opt))
                 (is-opt? (rule-is-opt? original-rule))
                 ;; (parent-match? (parent-type-matches-options? original-rule types))
                 (parent-match? (parent-type-matches-options? original-rule types))
                 (rule (if (and is-opt? parent-match?)
                           (assoc (lr1 (lr2 original-rule)) (ar expansions 'parser))
                           original-rule))
                 (products (if (eq? (lr1 rule) 'opt)
                               (list (lr2 rule))
                               (cddr rule)))
                 (rule-name (lr0 rule))
                 ;; action-def : (opt ((create (name $2))))
                 ;; action-def : ((create (name $2)))
                 (action (assoc rule-name (ar expansions 'action)))
                 ;;                  (action-def (cdr action))
                 ;;                  (cmd-arg-defs (if (and (symbol? (lr0 action-def))
                 ;;                                         (eq? (lr0 action-def) 'opt))
                 ;;                                    (cdr (lr0 (lr1 action-def)))
                 ;;                                    (cdr (lr0 action-def))))
                 ;;                  (cmd-arg-defs (action-cmd-list action))
                 ;; type : port
                 ;; products : ((term KW-INPUT) (term IDENTIFIER))
                 (c-types-and-names (map (lambda (cmd-arg)
                                           (action-cmd-arg-c-type-and-name cmd-arg action rule types))
                                         (cdar (action-cmd-list action))))
                 (c-types (map
                           ;; (lambda (c-type) (si "parse_#{c-type}_t"))
                           (make-infix-fn "parse_" "_t")
                           (append (list "type" (c-fy rule-name))
                                   (map car c-types-and-names))))
                 (c-names (map (make-postfix-fn ";")
                               (append (list "type" "next") (map cdr c-types-and-names))))
                 (fmt' (make-c-struct-field-formatter 4 c-types)))
            ;; (parent-type-matches-options? rule types)
            (pr (si "pl_struct_body( parse_#{(c-fy type)} )"))
            (pr (si "{"))
            (for-each (lambda (c-type c-name)
                        (pr (fmt' c-type c-name)))
                      c-types c-names)
            (pr (si "};"))
            (pr (si ""))))

         (for-list (action (ar expansions 'action))
                   (when (action-has-create? action)
                     ;; (ppr action)
                     ;;     (module (create (name $2) (port-list $4) (module-item-zom $7)))
                     (let* ((name (lr0 action))
                            (types (ar expansions 'types))
                            (rule (assoc name (ar expansions 'parser)))
                            (c-types-and-names (map (lambda (cmd-arg)
                                                      (action-cmd-arg-c-type-and-name cmd-arg action rule types))
                                                    (cdar (action-cmd-list action))))
                            (c-types (cons (si "#{lexpar-name}_t")
                                           (map (make-infix-fn "parse_" "_t")
                                                (map car c-types-and-names))))
                            (c-names (cons lexpar-name (map cdr c-types-and-names)))
                            (lines (format-c-create-function (rule-c-type rule types)
                                                             name
                                                             c-types
                                                             c-names
                                                             " );")))
                       (pl lines)
                       (pr "")))))

       (pr (si ""))
       (pr (si "#endif")))))


  (define (output-lexpar-c parser-name
                           lexpar-name
                           parse-info
                           grammar)

    (define ar assoc-ref)

    (define (linfo key) (assoc-ref parse-info key))

    ( ;; with-output-to-file (gen-file-path (si "#{lexpar-name}.h"))
     with-output-to-port (current-output-port)
     (lambda ()
       (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
              (parse-def (car (assoc-ref grammar 'parser))))


         (pr (si "#include \"#{parser-name}.h\""))
         (pr (si "#include \"parse.h\""))
         (pr (si "#include \"lexer.h\""))

         ;;     (pr (si "parse_module_t parse_module_create( lexpar_t            lexpar,"))
         ;;     (pr (si "parse_token_s       name,"))
         ;;     (pr (si "parse_port_t        port_list,"))
         ;;     (pr (si "parse_module_item_t module_item_list )"))
         ;;     (pr (si "{"))
         ;;     (pr (si "parse_module_t parse_module;"))
         ;;     (pr (si "parse_module = plam_get( lexpar->arena, sizeof( parse_module_s ) );"))
         ;;     (pr (si "parse_module->type = PARSE_MODULE;"))
         ;;     (pr (si "parse_module->name = plam_get( lexpar->arena, sizeof( parse_token_s ) );"))
         ;;     (pr (si "*parse_module->name = name;"))
         ;;     (pr (si "parse_module->port_list = port_list;"))
         ;;     (pr (si "parse_module->module_item_list = module_item_list;"))
         ;;     (pr (si "return parse_module;"))
         ;;     (pr (si "}"))

         (for-list (action (ar expansions 'action))
                   (when (action-has-create? action)
                     ;; (ppr action)
                     ;;     (module (create (name $2) (port-list $4) (module-item-zom $7)))
                     (let* ((name (lr0 action))
                            (types (ar expansions 'types))
                            (rule (assoc name (ar expansions 'parser)))
                            (c-type-of-rule (rule-c-type rule types))
                            (c-types-and-names (map (lambda (cmd-arg)
                                                      (action-cmd-arg-c-type-and-name cmd-arg action rule types))
                                                    (cdar (action-cmd-list action))))
                            (c-types (cons (si "#{lexpar-name}_t")
                                           (map (make-infix-fn "parse_" "_t")
                                                (map car c-types-and-names))))
                            (c-names (cons lexpar-name (map cdr c-types-and-names)))
                            (lines (format-c-create-function c-type-of-rule
                                                             name
                                                             c-types
                                                             c-names
                                                             " )"))
                            (obj-name (si "parse_#{(c-fy name)}"))
                            )
                       (pr "")
                       (pl lines)
                       (pr (si "{"))
                       (pr (si "    parse_#{(c-fy c-type-of-rule)}_t #{obj-name};"))
                       (pr (si "    #{obj-name} = plam_get( lexpar->arena, sizeof( parse_#{(c-fy c-type-of-rule)}_s ) );"))
                       (pr (si "    #{obj-name}->type = #{(string-upcase obj-name)};"))
                       (pr (si "    #{obj-name}->next = NULL;"))

                       (for-each (lambda (c-type-and-name)
                                   (let ((c-type (car c-type-and-name))
                                         (name (cdr c-type-and-name)))
                                     (if (string=? c-type "token")
                                         (begin
                                           (pr (si "    #{obj-name}->#{name} = plam_get( #{lexpar-name}->arena, sizeof( parse_token_s ) );"))
                                           (pr (si "    *#{obj-name}->#{name} = #{name};")))
                                         (pr (si "    #{obj-name}->#{name} = #{name};")))
                                     )
                                   )
                                 c-types-and-names)
                       (pr (si "    return #{obj-name};"))
                       (pr (si "}"))
                       )))))))


  (output-prepare! parser-name lexpar-name parse-info grammar)
  (output-lexer parser-name lexer-info grammar)
  (output-parse parser-name lexpar-name parse-info grammar)
  (output-lexpar-h parser-name lexpar-name parse-info grammar)
  (output-lexpar-c parser-name lexpar-name parse-info grammar)

  )



;; Token types:
;;   punct      Parsed separator
;;   comment    Comment
;;   keyword    Keyword string
;;   operator   Operator for operands
;;   space      Ignored separator
;;   value      Parsed value: <value>
;;   typeval    Parsed value: (cons <type> <value>)
;;   error      Invalid input text

(define grammar-block
  `((lexer (("\\("                    LPAR           punct)
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



(define grammar-vlog
  `((lexer (("[ \\t\\r\\n]+"                SPACE          space)
            ("//.*"                         LINE-COMMENT   comment)
            ("\"/*\"([^*]|\\*+[^*/])*\\*+\"/\"" BLOCK-COMMENT  comment)

            ("\"module\""                   KW-MODULE      keyword)
            ("\"endmodule\""                KW-ENDMODULE   keyword)
            ("\"input\""                    KW-INPUT       keyword)
            ("\"output\""                   KW-OUTPUT      keyword)
            ("\"wire\""                     KW-WIRE        keyword)
            ("\"reg\""                      KW-REG         keyword)
            ("\"always\""                   KW-ALWAYS      keyword)
            ("\"assign\""                   KW-ASSIGN      keyword)
            ("\"begin\""                    KW-BEGIN       keyword)
            ("\"end\""                      KW-END         keyword)

            ("[0-9]+\"'\"[hH][0-9a-fA-F_]+" VAL-NUMBER     value)
            ("[0-9]+\"'\"[oO][0-7_]+"       VAL-NUMBER     value)
            ("[0-9]+\"'\"[bB][01_xXzZ]+"    VAL-NUMBER     value)
            ("[0-9]+\"'\"[dD][0-9_]+"       VAL-NUMBER     value)
            ("[0-9]+"                       VAL-NUMBER     value)

            ("\"=\""                        OP-ASSIGN      operator)
            ("\"==\""                       OP-LOG-EQ      operator)
            ("\"<=\""                       OP-LOG-LE      operator)
            ("\">=\""                       OP-LOG-GE      operator)
            ("\"&&\""                       OP-LOG-AND     operator)
            ("\"||\""                       OP-LOG-ORI     operator)
            ("\"!\""                        OP-LOG-NEG     operator)

            ("\"+\""                        OP-ADD         operator)
            ("\"-\""                        OP-SUB         operator)

            ("\"&\""                        OP-BIT-AND     operator)
            ("\"|\""                        OP-BIT-ORI     operator)
            ("\"^\""                        OP-BIT-XOR     operator)

            ("\";\""                        SEP-SEMICOLON  operator)
            ("\":\""                        SEP-COLON      operator)
            ("\",\""                        SEP-COMMA      operator)
            ("\"(\""                        SEP-LEFTPAREN  operator)
            ("\")\""                        SEP-RIGHTPAREN operator)
            ("\"?\""                        SEP-QUESTION   operator)

            ("[a-zA-Z_][a-zA-Z0-9_$]*"      IDENTIFIER     value)
            ("\"`\"[a-zA-Z_]+"              DIRECTIVE      value)

            ("."                        UNKNOWN        error)))

    (parser ((start module)
             (module KW-MODULE
                     IDENTIFIER
                     SEP-LEFTPAREN
                     (port+ SEP-COMMA)
                     ;; port+
                     SEP-RIGHTPAREN
                     SEP-SEMICOLON
                     module-item*
                     KW-ENDMODULE)

             (port  (port-input
                     port-output
                     port-error))

             (port-input KW-INPUT IDENTIFIER)
             (port-output KW-OUTPUT IDENTIFIER)
             (port-error error)

             (module-item (assign-continuous!))

             (assign-continuous! (assign-continuous
                                  assign-continuous-error))

             (assign-continuous KW-ASSIGN
                                IDENTIFIER
                                OP-ASSIGN
                                IDENTIFIER
                                SEP-SEMICOLON)

             (assign-continuous-error error SEP-SEMICOLON)

             ))


    (action ((start (root))
             (module (create (name $2) (port-list $4) (module-item-zom $7)))
             ;; Generated:
             ;; (port-oom opt ((pass $1)
             ;;                (link $1 $3)))
             ;; Generated:
             ;; (port opt ((pass $1) (pass $1) (pass $1)))
             (port-input (create (name $2)))
             (port-output (create (name $2)))
             (port-error (clear) (errok))
             ;; Generated:
             ;; (module-item-zom opt ((empty)
             ;;                       (link-if $1 $2)))
             ;; Generate cast for this!
             (module-item opt ((create (item $1))))
             ;; Generated:
             ;; (assign-continuous-or-error (skip))
             (assign-continuous (create (lvalue $2) (rvalue $4)))
             ;; Generated:
             ;; (assign-continuous-error (errok))
             ))

    ;; Generated:
    (types ((port-input port)
            (port-output port)
            (port-error port)
            ;; Generated (default):
            ;; (module module)
            ;; Generated (expand):
            ;; (port-oom port)
            ;; Generated (default):
            ;; (port port)
            ;; Generated (expand):
            ;; (module-item-zom module-item)
            ;; Generated (default):
            ;; (module-item module-item)
            ;; Generated (expand):
            ;; (assign-continuous-or-error assign-continuous)
            ;; Generated (default):
            ;; (assign-continuous assign-continuous)
            ;; Generated (default):
            ;; (assign-continuous-error assign-continuous)
            ))

    ;; Precedence rules for operators (high->low)
    ;; + - ! ~ & ~& | ~| ^ ~^ ^~ (unary)
    ;;
    ;; **
    ;; * / %
    ;; + - (binary)
    ;; << >> <<< >>>
    ;; < <= > >=
    ;; == != === !==
    ;; & (binary)
    ;; ^ ^~ ~^ (binary)
    ;; | (binary)
    ;; &&
    ;; ||
    ;; ?: (conditional operator)
    ;; {} {{}}

    ;; Precedence rules for operators (low->high)
    (priority ((left OP-LOG-ORI)
               (left OP-LOG-AND)
               (left OP-BIT-ORI)
               (left OP-BIT-XOR)
               (left OP-BIT_AND)
               (left OP-SUB OP-ADD)
               (left SEP-COLON)
               (left SEP-SEMICOLON)
               ))))



(when #f

  (pargen-write-parser-module "foopar.scm"
                              '(foopar)
                              '()
                              "foopar"
                              5
                              grammar-block))


;; (pargen-write-c-parser "parsve"         ; parser-name
;;                        "lexpar"         ; lexpar-name
;;                        "."              ; output-dir
;;                        (list (cons 'file "verilog.l") ; lexer-info
;;                              (cons 'headers '())
;;                              (cons 'return  "RET_TOKEN"))
;;                        (list (cons 'file "verilog.y") ; parse-info
;;                              (cons 'headers '()))
;;                        grammar-vlog ; grammar
;;                        )
