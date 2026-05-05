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
                       (import-product (lr1 production))
                       (cons 'seq (map import-product (cdr production))))))))))

  (map import-rule pardef))


;; Expansions:
;;
;; ----
;;
;;     (...
;;      (oom port SEP-COMMA)
;;      ...)
;;
;;   =>
;;
;;     product replacement:
;;     (...
;;      (rule port-oom)
;;      ...)
;;
;;   AND
;;
;;     new rule:
;;     (...
;;      (port-oom opt
;;                (rule port)
;;                (seq (rule port) (term SEP-COMMA) (rule port-oom)))
;;      ...)
;;
;; ----
;;
;;     (...
;;      (zom module-item)
;;      ...)
;;
;;   =>
;;
;;     product replacement:
;;     (...
;;      (rule module-item-zom)
;;      ...)
;;
;;   AND
;;
;;     new rule:
;;     (...
;;      (module-item-zom opt
;;                       (empty empty)
;;                       (seq (rule module-item) (rule module-item-zom)))
;;      ...)
;;
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
;;     ((start rule module)
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
;;      (port-error rule error)
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
;;     ((start rule module)
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
;;      (port-error rule error)
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
      ((seq opt oer) (cddr rule))
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
       (let* ((separator (lr-if product 3))
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
                               '(link $1 $3)
                               '(link $1 $2))))
          ;;     (port-oom port)
          `(,new-tag ,org-tag))))

      ((zom)
       ;;     (zom module-item)
       (let* ((separator (lr-if product 3))
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
       (let* ((separator (lr-if product 3))
              (org-tag (lr1 product))
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
          (list item item)))

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
                                  ((seq opt)
                                   (let ((rule-names (list-clean (map rule-name (get-products rule)))))
                                     (for-each (lambda (name)
                                                 (hash-set! parents name (lr0 rule)))
                                               rule-names)))
                                  ((rule) (hash-set! parents (lr2 rule) (lr0 rule)))
                                  (else (error "Unknown rule type...")))
                                (lp (cdr rules)))
                              (hash->alist parents))))))

      parents))

  ;;   (define (derive-c-types rules base-types parents)
  ;;     #f
  ;;     )

  (define (sort-actions rules actions)
    (let lp ((rules rules)
             (ret '()))
      (lp-if rule
             (lp (cdr rules)
                 (cons (assoc (car rule) actions) ret))
             (reverse ret))))


  (let* ((res (expand pardef user-types))
         (all-rules (ppr (lr0 res)))
         (derived-actions (lr1 res))
         (all-c-types (lr2 res))
         ;; (c-types (append user-types expand-c-types))
         (parents (derive-parents all-rules))
         (sorted-actions (ppre (sort-actions all-rules (append user-actions derived-actions))))
         ;; (derived-c-types (derive-c-types rules c-types parents))
         ;; (all-c-types (append c-types))
         )
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
;;           (cons 'headers '("<stdio.h>" "<sys/param.h>"))
;;           (cons 'return  "RET_TOKEN"))
;;
(define (pargen-write-c-parser parser-name
                               lexpar-name
                               lexer-info
                               parse-info
                               grammar)

  (define (c-fy str)
    (string-replace-substring (symbol->string str) "-" "_"))

  (define (output-lexer parser-name
                        lexer-info
                        grammar)

    (define (linfo key) (assoc-ref lexer-info key))

    (;; with-output-to-file (linfo 'file)
     with-output-to-port (current-output-port)
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

    (define (linfo key) (assoc-ref lexer-info key))

    ;; The needed information for parse definitions:
    ;;
    ;; * List of rule names
    ;;
    ;; * Token names (c-fy)
    ;;
    ;; * Left association list of tokens
    ;;
    ;; * Rules with action info
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *
    ;;
    ;; *


    ( ;; with-output-to-file (linfo 'file)
     with-output-to-port (current-output-port)
     (lambda ()
       (let* ((lexer-def (car (assoc-ref grammar 'lexer)))
              (parse-def (car (assoc-ref grammar 'parser)))
              (pargen-defs (pargen-import-user-parser parse-def))
              (expansions (pargen-expand-user-parser pargen-defs
                                                     (car (assoc-ref grammar 'action))
                                                     (car (assoc-ref grammar 'types))))
              ;; (rules #f)
              )

         ;; (ppre pargen-defs)
         (ppre expansions)

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
         (pr (si "%union {"))
         (pr (si "int                       ival;"))

         ;;         (pr (si "parse_token_s             parse_token;"))
         ;;         (pr (si "parse_module_t            parse_module;"))
         ;;         (pr (si "parse_port_t              parse_port;"))
         ;;         (pr (si "parse_module_item_t       parse_module_item;"))
         ;;         (pr (si "parse_assign_continuous_t parse_assign_continuous;"))

         (pr (si "}"))
         (pr (si ""))
         (pr (si "%token <parse_token> KW_MODULE"))
         (pr (si "%token <parse_token> KW_ENDMODULE"))
         (pr (si "%token <parse_token> KW_INPUT"))
         (pr (si "%token <parse_token> KW_OUTPUT"))
         (pr (si "%token <parse_token> KW_WIRE"))
         (pr (si "%token <parse_token> KW_REG"))
         (pr (si "%token <parse_token> KW_ALWAYS"))
         (pr (si "%token <parse_token> KW_ASSIGN"))
         (pr (si "%token <parse_token> KW_BEGIN"))
         (pr (si "%token <parse_token> KW_END"))
         (pr (si ""))
         (pr (si "%token <parse_token> VAL_NUMBER"))
         (pr (si ""))
         (pr (si "%token <parse_token> OP_ASSIGN"))
         (pr (si "%token <parse_token> OP_LOG_EQ"))
         (pr (si "%token <parse_token> OP_LOG_LE"))
         (pr (si "%token <parse_token> OP_LOG_GE"))
         (pr (si "%token <parse_token> OP_LOG_AND"))
         (pr (si "%token <parse_token> OP_LOG_ORI"))
         (pr (si "%token <parse_token> OP_LOG_NEG"))
         (pr (si ""))
         (pr (si "%token <parse_token> OP_ADD"))
         (pr (si "%token <parse_token> OP_SUB"))
         (pr (si ""))
         (pr (si "%token <parse_token> OP_BIT_AND"))
         (pr (si "%token <parse_token> OP_BIT_ORI"))
         (pr (si "%token <parse_token> OP_BIT_XOR"))
         (pr (si ""))
         (pr (si "%token <parse_token> SEP_SEMICOLON"))
         (pr (si "%token <parse_token> SEP_COLON"))
         (pr (si "%token <parse_token> SEP_COMMA"))
         (pr (si "%token <parse_token> SEP_LEFTPAREN"))
         (pr (si "%token <parse_token> SEP_RIGHTPAREN"))
         (pr (si "%token <parse_token> SEP_QUESTION"))
         (pr (si ""))
         (pr (si "%token <parse_token> IDENTIFIER"))
         (pr (si "%token <parse_token> DIRECTIVE"))
         (pr (si ""))
         (pr (si "%type <parse_module> module"))
         (pr (si "%type <parse_port> port_list port"))
         (pr (si "%type <parse_module_item> module_item_list module_item"))
         (pr (si "%type <parse_assign_continuous> assign_continuous"))
         (pr (si ""))
         (pr (si "/* Precedence to handle expression ambiguity */"))
         (pr (si "%left SEP_SEMICOLON SEP_COLON"))
         (pr (si "%left OP_LOG_ORI"))
         (pr (si "%left OP_LOG_AND"))
         (pr (si "%left OP_BIT_ORI"))
         (pr (si "%left OP_BIT_XOR"))
         (pr (si "%left OP_BIT_AND"))
         (pr (si "%left OP_ADD OP_SUB"))
         (pr (si ""))
         (pr (si "%%"))
         (pr (si ""))


         ))))


  ;; (output-lexer parser-name lexer-info grammar)
  (output-parse parser-name lexpar-name parse-info grammar)

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
  `((lexer (("[ \\t\\r\\n]+"               SPACE          space)
            ("//.*"                     LINE-COMMENT   comment)
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
            ("[0-9]+"                   VAL-NUMBER     value)

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
             (module (create $2 $4 $7))
             ;; Generated:
             ;; (port-oom opt (pass $1)
             ;;               (link $1 $3))
             (port-input (create $2))
             (port-output (create $2))
             (port-error (clear) (errok))
             ;; Generated:
             ;; (module-item-oom opt (empty)
             ;;                      (link-if $1 $2))
             ;; Generate cast for this!
             (module-item (create $1))
             ;; Generated:
             ;; (assign-continuous-or-error (skip))
             (assign-continuous (create $2 $4))
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


    (priority ((left (SEP-SEMICOLON SEP-COLON OP-LOG-ORI OP-LOG-AND
                                    OP-BIT-ORI OP-BIT-XOR OP-BIT_AND
                                    OP-ADD OP-SUB))))))



(when #f

  (pargen-write-parser-module "foopar.scm"
                              '(foopar)
                              '()
                              "foopar"
                              5
                              grammar-block))


(pargen-write-c-parser "parsve"
                       "lexpar"
                       (list (cons 'file "verilog.l")
                             (cons 'headers '("<stdio.h>" "<sys/param.h>"))
                             (cons 'return  "RET_TOKEN"))
                       (list (cons 'file "verilog.y")
                             (cons 'headers '()))
                       grammar-vlog)
