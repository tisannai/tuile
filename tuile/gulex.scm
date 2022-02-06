(define-module (tuile gulex)
  #:use-module (tuile pr)
  ;;  #:use-module ((tuile utils) #:select (datum->string find-first define-mu-record))
  ;;  #:use-module ((tuile utils) #:select (datum->string find-first))
  #:use-module (srfi srfi-1)
  ;;  #:use-module (srfi srfi-9)
  ;;  #:use-module (srfi srfi-111)
  #:use-module ((ice-9 textual-ports) #:select (get-char))
  #:use-module (tuile compatible)
  #:export
  (
   char-stream
   char-stream-open
   char-stream-close
   char-stream-get
   char-stream-put
   char-stream-file
   char-stream-line
   char-stream-line-prev
   token-stream
   token-stream-open
   token-stream-open-with-lexer
   token-stream-close
   token-stream-token
   token-stream-get
   token-stream-put
   token-stream-name
   token-stream-line
   token-stream-line-prev
   gulex-parse-token-table
   token-table->lexer-ir
   lexer-table->lexer-ir
   gulex-create-lexer-interp
   gulex-create-lexer-fsm
   gulex-lexer-get-token
   gulex-show-token
   gulex-token-type
   gulex-token-value
   gulex-token-file
   gulex-token-line
   ))


;; For usage: see `gulex`, the Gulex CLI tool README.md.

;; Proper C block comment regex:
;;   /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/

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


;; ------------------------------------------------------------
;; Char stream:

(define-record-type char-stream
  (fields name                   ; Name of stream (filename for files)
          type                   ; Stream type (file, string)
          port                   ; Port of stream
          (mutable lineno)       ; Line number
          (mutable putback)      ; Putback buffer
          (mutable char)))       ; Current (prefetch) char


(define (char-stream-open name type)
  (let ((cs (make-char-stream name
                              type
                              (case type
                                ((file)   (open-input-file   name))
                                ((string) (open-input-string name))
                                (else
                                 (comp-error (ss "char-stream: Invalid stream type: " type))))
                              1
                              (list)
                              #f)))
    ;; Prefetch first char.
    (char-stream-get cs)
    cs))

(define (char-stream-lineno-update! cs proc)
  (char-stream-lineno-set! cs (proc (char-stream-lineno cs))))

(define (char-stream-close cs)
  (close-input-port (char-stream-port cs)))

(define (char-stream-get cs)
  (let ((ret (if (pair? (char-stream-putback cs))
                 (let ((ret (car (char-stream-putback cs))))
                   (char-stream-putback-set! cs (cdr (char-stream-putback cs)))
                   ret)
                 (get-char (char-stream-port cs)))))
    (when (and (not (eof-object? ret))
               (char=? ret #\newline))
      (char-stream-lineno-update! cs comp-1+))
    (char-stream-char-set! cs ret)
    ret))

;; Put back one or more chars. Note that chars must be in latest first
;; order.
(define (char-stream-put cs char-or-chars)
  (let* ((char-list (if (list? char-or-chars) char-or-chars (list char-or-chars)))
         (putbacks (cons (char-stream-char cs) char-list))
         (nl-count (fold + 0 (map (lambda (i) (if (char=? i #\newline) 1 0)) putbacks))))
    (char-stream-lineno-update! cs (lambda (v) (- v nl-count)))
    (char-stream-putback-set! cs (append (reverse putbacks) (char-stream-putback cs)))
    (char-stream-get cs)))

(define (char-stream-file cs)
  (if (eq? 'file (char-stream-type cs))
      (char-stream-name cs)
      #f))

(define (char-stream-line cs)
  (char-stream-lineno cs))

(define (char-stream-line-prev cs)
  (if (and (char-stream-char cs)
           (char=? #\newline (char-stream-char cs)))
      (char-stream-lineno cs)
      (comp-1+ (char-stream-lineno cs))))


;; ------------------------------------------------------------
;; Lexer definition parser:

;; Parse regexp and return the corresponding lexer IR.
(define (parse-regexp-entry regexp)

  (define cs (cons 0 (string->list regexp)))

  ;; Recursive regexp parser.
  (define (parse-regexp group? opt?)

    ;; Debug is expensive, hence do dbug with macro.
    (define-syntax dbug
      (lambda (x)
        (syntax-case x ()
          ((_ msg)
           #'#t
           ;; #'(pr "PARSE: " msg)
           ))))

    (define special (string->list "*+.[]"))
    (define setchar (string->list "-^"))
    (define is-in?  member)
    (define (normal? c)
      (and (not (is-in? c special))
           (not (is-in? c setchar))))

    (define (is? a b)
      (cond
       ((and (char? a)
             (char? b))
        (char=? a b))
       ((and (symbol? a)
             (symbol? b))
        (equal? a b))
       (else
        #f)))

    (define (idx)
      (car cs))

    (define (err msg)
      (display (ss "gulex ERROR: at index:" (idx)))
      (display (ss "  " msg))
      (newline)
      (exit 1))

    (define (nth idx)
      (if (< idx (length (cdr cs)))
          (list-ref (cdr cs) idx)
          'eof))

    (define (cur)
      (nth (car cs)))

    (define (cur-is? ch)
      (is? ch (cur)))

    (define (peek)
      (nth (comp-1+ (car cs))))

    (define (step)
      (set-car! cs (+ (car cs) 1)))

    (define (get)
      (let ((ret (cur)))
        (step)
        ret))

    (define (use c)
      (if (is? c (cur))
          (get)
          (err (ss "Expected: " c ", got: " (cur)))))

    (define (use-if check . msg)
      (if (check (cur))
          (get)
          (if (pair? msg)
              (err (ss "Expected: " (car msg) ", got: " (cur)))
              (err (ss "Expected: -, got: " (cur))))))


    ;; Parse selection body (content).
    (define (parse-sel-body)

      (if (is? #\^ (cur))

          ;; [^...]
          ;;  ^
          (begin
            (get)
            (list 'inv (parse-sel-body)))

          ;; [a-...]
          ;; [ab...]
          ;;  ^
          (cons 'sel
                (let loop ()

                  (cond

                   ;; [a-z...]
                   ;;   ^
                   ((is? #\- (peek))
                    (let ((t1 (get)))
                      (use #\-)
                      (cons (list 'ran t1 (use-if normal?))
                            (loop))))

                   ;; [abc]
                   ;;     ^
                   ((is? #\] (cur))
                    '())

                   ;; [ab...]
                   ;;  ^
                   (else
                    (when (is? #\\ (cur))
                      (get))
                    (cons (get)
                          (loop))))))))

    ;; Parse selection.
    ;; [...]
    (define (parse-sel)
      (use #\[)
      (let ((ret (parse-sel-body)))
        (use #\])
        ret))

    ;; "lookahead" is lookahead non-terminal.
    ;; "lexers" is a sequence of match atoms.
    (let loop ((lookahead #f)
               (lexers    '()))

      (cond

       ;; [a-z]+
       ;;       ^
       ((cur-is? 'eof)
        ;; Not in opt.
        (cons 'seq
              (if lookahead
                  (reverse (cons lookahead lexers))
                  (reverse lexers))))

       (else

        (dbug (ss "cur: " (cur)))

        (cond

         ;; (ab|cd)...
         ;; ^
         ((cur-is? #\()
          (dbug "Start group")
          (use #\()
          (let ((ret (parse-regexp #t opt?)))
            (use #\))
            (loop #f
                  (if lookahead
                      (cons ret (cons lookahead lexers))
                      (cons ret lexers)))))

         ;; (ab|cd)...
         ;;       ^
         ((cur-is? #\))
          (dbug "End group")
          (if opt?
              ;; Part of options.
              (if (not lookahead)
                  (err "Missing lookahead for option")
                  ;; (a|b|c|d)
                  ;;         ^
                  (cons 'seq (reverse (cons lookahead lexers))))
              ;; Not part of options.
              (cons 'seq
                    (if lookahead
                        (reverse (cons lookahead lexers))
                        (reverse lexers)))))

         ;; abc|d...
         ;; (ab|cd)...
         ;;    ^
         ((cur-is? #\|)
          (dbug "Option")
          (use #\|)
          (cond
           ((not opt?)
            ;; First part of non-group opt.
            (if (not lookahead)
                (err "Missing lookahead for option")
                (let loop-opt ((opts '()))
                  (let ((opt (parse-regexp group? #t)))
                    (if (or (cur-is? 'eof)
                            (cur-is? #\)))
                        ;; a|b|c|d    OR    (a|b|c|d)
                        ;;        ^                 ^
                        (cons 'opt (cons (cons 'seq (reverse (cons lookahead lexers)))
                                         (reverse (cons opt opts))))
                        ;; a|b|c|d    OR    (a|b|c|d)
                        ;;    ^                 ^
                        (begin
                          (use #\|)
                          (loop-opt (cons opt opts))))))))
           (else
            ;; Rest part of non-group opt.
            (if (not lookahead)
                (err "Missing lookahead for option")
                ;; a|b|c|d
                ;;    ^
                (cons 'seq (reverse (cons lookahead lexers)))))))

         ;; [a-...
         ;; ^
         ((cur-is? #\[)
          (loop (parse-sel)
                (if lookahead (cons lookahead lexers) lexers)))

         ;; [a-z]*
         ;;      ^
         ((cur-is? #\*)
          (dbug "ZOM")
          ;; TODO 210610_1209: Is it ok to comment out?
          ;;          (when (not lookahead)
          ;;            (err "Missing repeatable item"))
          (get)
          (loop #f
                (if lookahead
                    (cons (list 'zom lookahead)
                          lexers)
                    (cons (list 'zom (car lexers))
                          (cdr lexers)))))

         ;; [a-z]?
         ;;      ^
         ((cur-is? #\?)
          (dbug "ZOO")
          ;; TODO 210610_1209: Is it ok to comment out?
          ;;          (when (not lookahead)
          ;;            (err "Missing repeatable item"))
          (get)
          (loop #f
                (if lookahead
                    (cons (list 'zoo lookahead)
                          lexers)
                    (cons (list 'zoo (car lexers))
                          (cdr lexers)))))

         ;; [a-z]+
         ;;      ^
         ((cur-is? #\+)
          (dbug "OOM")
          ;; (dbug (comp-datum->string lexers))
          (get)
          (loop #f
                (if lookahead
                    (cons (list 'oom lookahead)
                          lexers)
                    (cons (list 'oom (car lexers))
                          (cdr lexers)))))

         ;; abc...
         ;; a.c...
         ;; a\[
         ;;  ^
         (else

          (dbug "Normal")

          (let* ((ch (get))
                 (tok (cond
                       ((is? #\. ch)
                        (list 'any))
                       ((is? #\\ ch)
                        (get))
                       (else
                        ch))))
            ;; abc...
            ;;  ^
            (loop tok
                  (if lookahead (cons lookahead lexers) lexers)))))))))

  (parse-regexp #f #f))


;; Return lexer-table: ((token lexer-def) ...)
(define (gulex-parse-token-table table)
  (map (lambda (e)
         (list (second e)
               (parse-regexp-entry (first e))))
       table))


;; Return lexer-ir: ((token lexer-def) ...)
(define (token-table->lexer-ir table)
  (lexer-table->lexer-ir (gulex-parse-token-table table)))


;; ------------------------------------------------------------
;; Lexer interpreter:

;; Recursive lexer interpreter.
;;
;; Args:
;;     token         Token to return for match.
;;     lexer         Lexer to apply.
;;     char-stream   Character stream handle.
;;
;; Return: (token|#f lexeme)
;;
(define (lex-interp token lexer char-stream)

  ;; Debug is expensive, hence do dbug with macro.
  (define-syntax dbug
    (lambda (x)
      (syntax-case x ()
        ((_ msg)
         #'#t
         ;; #'(pr "PARSE: " msg)
         ))))

  ;; Return value formatter.
  (define (return success? matched)
    (list success? matched))

  ;; Get next char from stream.
  (define (get)
    (char-stream-get char-stream))

  ;; Get N characters.
  (define (get-n n)
    (if (= 0 n)
        #f
        (let loop ((i n))
          (if (> i 1)
              (begin
                (get)
                (loop (- i 1)))
              (get)))))

  ;; Return current char.
  (define (cur)
    (char-stream-char char-stream))

  ;; Is EOF reached?
  (define (eof?)
    (eof-object? (char-stream-char char-stream)))

  ;; Put back m, but make the first current "char".
  ;; Return: empty list.
  (define (put-back m)
    (when (pair? m)
      (char-stream-put char-stream (reverse m)))
    '())

  ;; Check "ch" against lexer.
  ;;
  ;; Return #t on match.
  (define (lex-interp-step ch lexer)

    ;; Non-char.
    (case (car lexer)

      ;; (sel #\a #\b #\c)
      ((sel)
       ;; (pr "Sel char: " ch)
       (let loop ((lexer (cdr lexer)))
         (if (pair? lexer)

             (if (if (char? (car lexer))
                     (char=? (car lexer) ch)
                     (lex-interp-step ch (car lexer)))
                 #t
                 (loop (cdr lexer)))

             #f)))

      ;; (any)
      ((any)
       (not (char=? ch #\newline)))

      ;; (inv (sel #\a #\b #\c))
      ((inv)
       (not (lex-interp-step ch (second lexer))))

      ;; (ran #\a #\c)
      ((ran)
       ;; (pr "ran: " (second lexer) ", " (third lexer))
       (and (>= (char->integer ch)
                (char->integer (second lexer)))
            (<= (char->integer ch)
                (char->integer (third lexer)))))))

  ;; Create return value for single char match and get new char.
  (define (single-char-match token char)
    (dbug (ss "Single char match: " char))
    (get)
    (return token (list char)))

  ;; Create return value for single char mismatch.
  (define (single-char-mismatch token char)
    (dbug (ss "Single char mismatch: " char))
    (return #f '()))


  (if (not (eof?))

      (begin

        (dbug (ss "Start: token: " token ", lexer: " (comp-datum->string lexer) ", char: " (cur)))

        (cond

         ;; CHAR matching.
         ((not (list? lexer))
          (if (char=? lexer (cur))
              (single-char-match token (cur))
              (single-char-mismatch token (cur))))

         ;; (sel ...)
         ;; (any)
         ;; (inv ...)
         ;; (ran ...)
         ((or (eq? 'sel (car lexer))
              (eq? 'any (car lexer))
              (eq? 'inv (car lexer))
              (eq? 'ran (car lexer)))
          (dbug (ss "Matching: " (car lexer) ", to: " (cur)))
          (let ((ret (lex-interp-step (cur) lexer)))
            (if ret
                (single-char-match token (cur))
                (single-char-mismatch token (cur)))))

         ;; (seq ...)
         ((eq? 'seq (car lexer))

          (dbug (ss "Sequence ----: "))

          (let loop-seq ((lexer (cdr lexer))
                         (m '()))

            (if (pair? lexer)

                (if (eof?)

                    (begin
                      (dbug (ss "Sequence no match: EOF"))
                      (return #f (put-back m)))

                    ;; Continue.
                    (let ((ret (lex-interp token
                                           (car lexer)
                                           char-stream)))
                      (if (first ret)
                          (begin
                            (dbug (ss "Sequence adding: " token ", string: " (list->string m) ", cur: " (cur)))
                            (if (pair? m)
                                ;; Success, continue.
                                (loop-seq (cdr lexer)
                                          (append m (second ret)))
                                ;; TODO: We may get empty "m" from ZOO, and we should proceed???
                                (loop-seq (cdr lexer)
                                          (append m (second ret)))))
                          ;; Matching done (failure).
                          (begin
                            (dbug (ss "Sequence mismatch, with: " (cur)))
                            (return #f (put-back m))))))

                ;; Matching done (success).
                (begin
                  (dbug (ss "Sequence done: " token ", string: " (list->string m) ", cur: " (cur)))
                  (return token m)))))

         ;; (zoo (...))
         ((eq? 'zoo (car lexer))
          (let ((ret (lex-interp token
                                 (second lexer)
                                 char-stream)))
            (return token (second ret))))

         ;; (zom (...))
         ((eq? 'zom (car lexer))

          (dbug (ss "Zero-or-more ----: "))

          (let loop-zom ((m '()))

            (if (eof?)

                (return token m)

                (let ((ret (lex-interp token
                                       (second lexer)
                                       char-stream)))
                  (if (first ret)

                      (begin
                        (dbug (ss "Zero-or-more adding: " token ", string: \"" (list->string (second ret)) "\""))
                        ;; Success, continue.
                        (loop-zom (append m (second ret))))

                      ;; Matching done (success).
                      (return token m))))))

         ;; (oom (...))
         ((eq? 'oom (car lexer))

          (dbug (ss "One-or-more ----: "))

          (let ((return-one-or-more
                 (lambda (matched? m)
                   (if matched?
                       (begin
                         (dbug (ss "One-or-more match: " token ", string: " (list->string m) ", cur: " (cur)))
                         (return matched? m))
                       (begin
                         (dbug (ss "One-or-more mismatch: " token ", string: " (list->string m) ", cur: " (cur)))
                         (return #f (put-back m)))))))

            (let loop-oom ((matched? #f)
                           (m '()))

              (if (eof?)

                  (return-one-or-more matched? m)

                  (let ((ret (lex-interp token
                                         (second lexer)
                                         char-stream)))
                    (if (first ret)

                        (begin
                          (dbug (ss "One-or-more adding: " token ", string: \"" (list->string (second ret)) "\""))
                          ;; Success, continue.
                          (loop-oom token
                                    (append m (second ret))))

                        ;; Matching mismatch (success/failure).
                        (return-one-or-more matched? m)))))))

         ;; (opt ...)
         ((eq? 'opt (car lexer))

          (dbug (ss "Options ----: "))

          ;; Return the longest (or first if many longest), if any.
          (let ((opt-return (lambda (opts)
                              (if (pair? opts)
                                  (begin
                                    (dbug (ss "Option matches: " (comp-datum->string opts)))
                                    (let* ((best (apply max (map (lambda (ret)
                                                                   (length (second ret)))
                                                                 opts)))
                                           ;;                                           (ret (find-first (lambda (i)
                                           ;;                                                              (= (length (second i))
                                           ;;                                                                 best))
                                           ;;                                                            (reverse opts)))
                                           (ret (find (lambda (i)
                                                        (= (length (second i))
                                                           best))
                                                      (reverse opts)))
                                           )
                                      ;; Take the matched token away from char stream.
                                      (get-n (length (second ret)))
                                      ret))
                                  (return #f '())))))

            (let loop-opt ((lexer (cdr lexer))
                           (opts '()))

              (if (pair? lexer)

                  ;; Continue.
                  (let ((ret (lex-interp token
                                         (car lexer)
                                         char-stream)))
                    (cond

                     ((first ret)
                      (dbug (ss "Options match: " token))
                      ;;                    (return (first ret) (second ret))
                      ;; Put back the matched token and current (if not eof).
                      (put-back (second ret))
                      (loop-opt (cdr lexer)
                                (cons (return (first ret) (second ret))
                                      opts)))

                     ((eof?)
                      (dbug (ss "Options failure: no data"))
                      (opt-return opts)
                      )

                     (else
                      (dbug (ss "Options mismatch"))
                      (loop-opt (cdr lexer)
                                opts))))

                  ;; Matching done (failure).
                  (begin
                    (if (pair? opts)
                        (dbug (ss "Options return: some matches"))
                        (dbug (ss "Options return: no matches")))
                    (opt-return opts))))))

         ;; (tok ID (seq ...))
         ((eq? 'tok (car lexer))
          (dbug (ss "Token: "))
          (let ((ret (lex-interp (second lexer)
                                 (third lexer)
                                 char-stream)))
            (dbug (ss "Token, token: " (first ret)))
            ret))))

      ;; EOF return.
      (return 'eof '())))


(define (gulex-create-lexer-interp gulex-token-table)
  (let ((lexer-ir (token-table->lexer-ir gulex-token-table)))
    (lambda (char-stream)
      (lex-interp #f lexer-ir char-stream))))



;; ------------------------------------------------------------
;; Lexer fsm:

;; Lexer node for one char match step.
(define-record-type lnode
  (fields rule                          ; Match rule.
          label                         ; Node label.
          term                          ; Is term?
          (mutable next)))              ; Next node.


;; Is "ch" accepted by lnode?
(define (lnode-accept? lnode ch)

  (define (accept? ch lexer)

    (if (char? lexer)

        (char=? lexer ch)

        ;; Non-char.
        (case (car lexer)

          ;; (sel #\a #\b #\c)
          ((sel)
           (let loop ((lexer (cdr lexer)))
             (if (pair? lexer)
                 (if (if (char? (car lexer))
                         (char=? (car lexer) ch)
                         (accept? ch (car lexer)))
                     #t
                     (loop (cdr lexer)))

                 #f)))

          ;; (any)
          ((any)
           (not (char=? ch #\newline)))

          ;; (inv (sel #\a #\b #\c))
          ((inv)
           (not (accept? ch (second lexer))))

          ;; (ran #\a #\c)
          ((ran)
           ;; (pr "ran: " (second lexer) ", " (third lexer))
           (and (>= (char->integer ch)
                    (char->integer (second lexer)))
                (<= (char->integer ch)
                    (char->integer (third lexer))))))))

  (cond
   ((eof-object? ch)
    #f)
   ((lnode-rule lnode)
    (accept? ch (lnode-rule lnode)))
   (else
    #f)))


;; ------------------------------------------------------------
;; Test code:

(define (show-lnode lnode)
  (string-join (list (ss "label: " (lnode-label lnode))
                     (ss "    rule: " (comp-datum->string (lnode-rule lnode)))
                     (if (lnode-term lnode)
                         (ss "    <term>")
                         (ss "    next: " (comp-datum->string (lnode-next lnode)))))
               "\n"))

(define (show-lnode-list lnode-list)
  (string-join (map show-lnode lnode-list)
               "\n"))

;; Test code:
;; ------------------------------------------------------------


;; Convert lexer-ir to lnode-list
(define (lexer-ir->lnode-list ir)

  (define lnode-index 0)
  (define lnode-list (list))

  (define (lnode-get-label)
    (let ((label lnode-index))
      (set! lnode-index (comp-1+ lnode-index))
      label))

  (define (lnode-add rule term next)
    (let ((lnode (make-lnode rule
                             (lnode-get-label)
                             term
                             next)))
      (set! lnode-list (cons lnode lnode-list))
      lnode))

  (define (lnode-create-terminal)
    (let ((ret (lnode-add #f #t (list))))
      (lnode-label ret)))

  (define (lnode-create rule next)
    (lnode-add rule #f next))

  (define (get-label lnode-or-nodes)
    (if (list? lnode-or-nodes)
        (lnode-label (car lnode-or-nodes))
        (lnode-label lnode-or-nodes)))

  (define (lnode-create-sub ir next)
    (if (single? ir)
        (lnode-create ir (list next))
        (->net ir next)))

  (define (non-char-single? sub)
    (case (car sub)
      ((sel any inv ran) #t)
      (else #f)))

  (define (single? sub)
    (or (char? sub)
        (non-char-single? sub)))


  ;; (seq (oom (opt (seq #\a #\b) (seq #\c #\d))) #\a)
  (define (seq->net ir next)
    (reverse
     (let loop ((tail (reverse (cdr ir)))
                (prev next))
       (if (pair? tail)
           (let ((sub (lnode-create-sub (car tail) prev)))
             (cons sub (loop (cdr tail)
                             (lnode-label sub))))
           '()))))


  ;; (opt (seq #\a #\b) (seq #\c #\d))
  (define (opt->net ir next)
    (let ((opts
           (let loop ((tail (cdr ir)))
             (if (pair? tail)
                 (cons (get-label (lnode-create-sub (car tail)
                                                    next))
                           (loop (cdr tail)))
                 '()))))
      (lnode-create #f opts)))

  ;; (zoo #\a)
  (define (zoo->net ir next)
    (let* ((sub (lnode-create-sub (second ir) next))
           (opt (lnode-create #f
                              (list (get-label sub)
                                    next))))
      opt))

  ;; (zom |<-         argument          ->|)
  ;; (zom (opt (seq #\a #\b) (seq #\c #\d)))
  (define (zom->net ir next)
    (let* ((out (lnode-create #f (list next)))
           (sub (lnode-create-sub (second ir) (get-label out)))
           (in (lnode-create #f (list (get-label out)))))
      (lnode-next-set! out (cons (get-label sub)
                                 (lnode-next out)))
      in))


  ;; (oom |<-         argument          ->|)
  ;; (oom (opt (seq #\a #\b) (seq #\c #\d)))
  (define (oom->net ir next)
    (let* ((out (lnode-create #f (list next)))
           (sub (lnode-create-sub (second ir) (get-label out)))
           (in (lnode-create #f (list (get-label sub)))))
      (lnode-next-set! out (cons (get-label sub)
                                 (lnode-next out)))
      in))


  (define (->net ir next)
    (case (car ir)
      ((seq) (seq->net ir next))
      ((opt) (opt->net ir next))
      ((zoo) (zoo->net ir next))
      ((zom) (zom->net ir next))
      ((oom) (oom->net ir next))))


  ;; ------------------------------------------------------------
  ;; Test code:

  (define next #f)

  (define (reset)
    (set! lnode-index 0)
    (set! lnode-list (list))
    (set! next (lnode-create-terminal)))


  (define (show-lnode lnode)
    (pr "label: " (lnode-label lnode))
    (pr "    rule: " (comp-datum->string (lnode-rule lnode)))
    (if (lnode-term lnode)
        (pr "    <term>")
        (pr "    next: " (comp-datum->string (lnode-next lnode)))))

  (define (show-lnode-list)
    (for-each show-lnode lnode-list))

  (define (try ir)
    (pr "IR: " (comp-datum->string ir))
    (set! lnode-index 0)
    (set! lnode-list (list))
    (let ((res (let ((next (lnode-create-terminal)))
                 (->net ir next))))
      (show-lnode-list)))

  ;; Tested sequences:
  (when #f
    (try '(seq (sel #\a #\b)))
    (try '(seq #\a #\b))
    (try '(opt (seq #\a #\b) (seq #\c #\d)))
    (try '(zoo #\a))
    (try '(seq (zoo #\a)))
    (try '(seq (zoo #\a) #\b))
    (try '(seq (zoo #\a) (zoo #\b)))
    (try '(zom #\a))
    (try '(seq (zom #\a)))
    (try '(oom #\a))
    (try '(seq (oom #\a)))

    ;; Not working.
    (try '(seq (oom #\a)))
    )

  ;; Test code:
  ;; ------------------------------------------------------------

  (->net ir (lnode-create-terminal))
  lnode-list)


(define-record-type fsm
  (fields token                         ; Token
          nodes                         ; Node list
          start                         ; Start node.
          (mutable state)               ; FSM state.
          (mutable match-count)         ; Count of matches in set.
          (mutable valid-prev)          ; Previous valid match set.
          (mutable valid-cur)))         ; Current match set.


;; Return fsm.
;;
;; Perform "fsm-reset" before use.
;;
(define (lexer-ir->fsm token ir)

  ;; Create FSM.
  ;;
  ;; Order lnode-list using labels and store the ordered list as vector
  ;; for efficient lookup.
  ;;
  ;; Args:
  ;;     lnode-list Ordered graph of lnodes.
  ;;
  (define (fsm-create token lnode-list)
    (let ((start (lnode-label (car lnode-list))))
      (make-fsm token
                (list->vector
                 (comp-sort lnode-list
                            (lambda (a b)
                              (< (lnode-label a)
                                 (lnode-label b)))))
                start
                (list)
                0
                #f
                #f)))


  (let* ((lnode-list (lexer-ir->lnode-list ir)))
    (fsm-create token lnode-list)))


;; FSM based lexer.
;;
;; Args:
;;     fsm-set       Set of FSM lexers.
;;     char-stream   Character stream handle.
;;
;; Return: (token|#f lexeme)
;;
(define (lexer-fsm fsm-set char-stream)

  ;; Debug is expensive, hence do dbug with macro.
  (define-syntax dbug
    (lambda (x)
      (syntax-case x ()
        ((_ msg)
         #'#t
         ;; #'(pr "PARSE: " msg)
         ))))

  ;; Return value formatter.
  (define (return success? matched)
    (list success? matched))

  ;; Get next char from stream.
  (define (get)
    (char-stream-get char-stream))

  ;; Return current char.
  (define (cur)
    (char-stream-char char-stream))

  ;; Is EOF reached?
  (define (eof?)
    (eof-object? (char-stream-char char-stream)))

  ;; Reset fsm before starting matching with "fsm-step".
  (define (fsm-reset-set fsm-set)
    (for-each (lambda (fsm)
                (fsm-state-set! fsm (list (fsm-start fsm)))
                (fsm-match-count-set! fsm 0)
                (fsm-valid-prev-set! fsm #f)
                (fsm-valid-cur-set! fsm #f))
              fsm-set))

  ;; Copy cur state to prev before continuing matching with "fsm-step".
  (define (fsm-store-valid-set fsm-set)
    (for-each (lambda (fsm)
                (fsm-valid-prev-set! fsm (fsm-valid-cur fsm))
                (fsm-valid-cur-set! fsm #f))
              fsm-set))

  ;; Return lnode by index.
  (define (fsm-get-lnode fsm index)
    (vector-ref (fsm-nodes fsm) index))

  ;; Return states in fanout of state (current state).
  (define (fsm-fanout fsm from)
    (define (depth-first fsm from)
      ;; (pr "fsm-fanout from: " (ds from))
      (let loop ((tail from))
        (if (pair? tail)
            (begin
              ;; (pr "fsm-fanout cur: " (car tail))
              (let ((lnode (fsm-get-lnode fsm (car tail))))
                ;; (pr "fsm-fanout lnode: " (ds lnode))
                (if (or (lnode-rule lnode)
                        (null? (lnode-next lnode)))
                    (cons (car tail) (loop (cdr tail)))
                    (append (depth-first fsm (lnode-next lnode))
                            (loop (cdr tail)))
                    )))
            '())))
    (if (and (= (length from) 1)
             (lnode-rule (fsm-get-lnode fsm (car from))))
        from
        (depth-first fsm from)))

  ;; Return true if success.
  ;;
  ;; NOTE: No stepping is performed if step would fail.
  (define (fsm-step fsm ch)

    ;; Check if fsm accepts at "index" the "ch".
    (define (fsm-accept? fsm index ch)
      (if (lnode-accept? (fsm-get-lnode fsm index)
                         ch)
          index
          #f))


    (dbug (ss "FSM ***** START"))
    (dbug (ss "FSM nodes: \n" (show-lnode-list (vector->list (fsm-nodes fsm)))))
    (dbug (ss "FSM state: " (ds (fsm-state fsm))))
    (dbug (ss "FSM valid-prev: " (fsm-valid-prev fsm)))
    (dbug (ss "FSM valid-cur: " (fsm-valid-cur fsm)))
    (dbug (ss "FSM fanout: " (ds (fsm-fanout fsm (fsm-state fsm)))))

    (if (eof-object? ch)

        ;; EOF is never accepted.
        (begin
          (dbug (ss "FSM EOF MISMATCH"))
          (dbug (ss "FSM ***** END"))
          (fsm-valid-cur-set! fsm #f)
          #f)

        (let* (
               ;; Propagate current state as far as possible.
               (fanout (fsm-fanout fsm (fsm-state fsm)))
               ;; Collect all states that accept "ch".
               (accepting (filter identity
                                  (map (lambda (index)
                                         (fsm-accept? fsm index ch))
                                       fanout))))
          (if (pair? accepting)
              (begin
                (dbug (ss "FSM accepting: " (ds accepting)))
                (dbug (ss "FSM MATCH"))
                (dbug (ss "FSM ***** END"))
                (fsm-state-set!
                 fsm
                 ;; Step each accepting state one forward to get new
                 ;; state.
                 (apply append
                        (map (lambda (index)
                               (lnode-next (fsm-get-lnode fsm index)))
                             accepting)))
                (fsm-match-count-set! fsm (comp-1+ (fsm-match-count fsm)))
                (fsm-valid-cur-set! fsm #t)
                #t)
              (begin
                (dbug (ss "FSM MISMATCH"))
                (dbug (ss "FSM ***** END"))
                (fsm-valid-cur-set! fsm #f)
                #f)))))


  ;; Step the full-set forward and return failure or success with
  ;; accepting fsm(s).
  (define (fsm-step-set fsm-set ch)

    (define (->valid-set-cur fsm-set)
      (filter fsm-valid-cur fsm-set))

    (for-each (lambda (fsm)
                (fsm-step fsm ch))
              fsm-set)

    (let ((valid-set (->valid-set-cur fsm-set)))
      (if (null? valid-set)
          (cons 'failure '())
          (cons 'continue valid-set))))


  (define (fsm-terminated? fsm)
    (let ((fanout (fsm-fanout fsm (fsm-state fsm))))
      (find (lambda (i) (= i 0)) fanout)))


  ;; Find first fsm which has one of it's state as 0 in the propagated
  ;; fanout.
  (define (fsm-find-terminated fsm-set)
    (find fsm-terminated? fsm-set))



  ;; Reset all FSMs to initial state.
  (fsm-reset-set fsm-set)

  ;; Loop through char-stream.
  (if (not (eof?))

      (let loop ((chars '())
                 (one-pass-done #f)
                 (best-terminated-fsm #f)
                 (prev-set fsm-set))

        (fsm-store-valid-set prev-set)

        (begin
          (dbug (ss "------"))
          (dbug (ss "FSM: matching: " (cur)))

          (let ((next-res (fsm-step-set prev-set (cur))))

            (dbug (ss " -- SET RESULT"))
            (dbug (if (eq? (car next-res) 'continue)
                      (ss "FSM: next-res: " (car next-res)
                          ", valid-cur: " (ds (map fsm-valid-cur (cdr next-res)))
                          ", state: " (ds (map fsm-state (cdr next-res))))
                      (ss "next-res car: " (car next-res))))

            (cond

             ((eq? (car next-res) 'failure)
              ;; Matching terminated.
              (if one-pass-done
                  ;; Return previous set state (round) or from best-terminated.
                  (let ((terminated-from-prev-set (fsm-find-terminated prev-set)))

                    (if terminated-from-prev-set

                        ;; Last-standing FSM was matched to termination.
                        (return (fsm-token terminated-from-prev-set)
                                (reverse chars))

                        ;; Some earlier terminated FSM is used.
                        (begin
                          ;; Put back chars that are not
                          ;; matched. First put-back "cur" and then
                          ;; all that overflow "best-terminated-fsm".
                          (let ((leftover-count (- (length chars)
                                                   (car best-terminated-fsm))))
                            (char-stream-put char-stream (take chars leftover-count))
                            (return (fsm-token (cdr best-terminated-fsm))
                                    (reverse (drop chars leftover-count)))))))

                  (return #f '())))

             (else
              ;; Continue;
              (let ((ch (cur)))
                (get)
                (loop (cons ch chars)
                      #t
                      (let ((best (cadr next-res)))
                        (if (fsm-terminated? best)
                            (cons (fsm-match-count best) best)
                            best-terminated-fsm))
                      (cdr next-res))))))))
      ;; EOF return.
      (return 'eof '())))


(define (gulex-create-lexer-fsm gulex-token-table)
  (let* ((lexer-ir (token-table->lexer-ir gulex-token-table))
         (fsm-set (map (lambda (token-ir)
                         (lexer-ir->fsm (second token-ir) (third token-ir)))
                       (cdr lexer-ir))))
    (lambda (char-stream)
      (lexer-fsm fsm-set char-stream))))




;; ------------------------------------------------------------
;; Token stream:

;;(define-record-type token-stream
;;  (make-token-stream cs lexer token buf)
;;  token-stream?
;;  (cs    token-stream-cs)                            ; Char-stream.
;;  (lexer token-stream-lexer)                         ; Lexer (interp, fsm, ...).
;;  (token token-stream-token set-token-stream-token!) ; Current token.
;;  (buf   token-stream-buf set-token-stream-buf!))        ; Buffer for put-back.

(define-record-type token-stream
  (fields cs                            ; Char-stream.
          lexer                         ; Lexer (interp, fsm, ...).
          (mutable token)               ; Current token.
          (mutable buf)))               ; Buffer for put-back.

;; Open token stream for name (file/string) and type
;; (file/string). Use lexer for token extraction rules.
(define (token-stream-open name type lexer skip-token-prefetch)
  (let* ((cs (char-stream-open name type))
         (ts (make-token-stream cs lexer #f '())))
    ;; Prefetch first token.
    (when (not skip-token-prefetch)
      (token-stream-get ts))
    ts))

;; Open token stream for name (file/string) and type
;; (file/string). Use token-table for token extraction rules.
(define (token-stream-open-with-lexer name type lexer skip-token-prefetch)
  (token-stream-open name type lexer skip-token-prefetch))

;; Close token stream.
(define (token-stream-close ts)
  (char-stream-close (token-stream-cs ts)))

;; Get next token.
(define (token-stream-get ts)
  (let ((ret (if (pair? (token-stream-buf ts))
                 (let ((ret (car (token-stream-buf ts))))
                   (token-stream-buf-set! ts (cdr (token-stream-buf ts)))
                   ret)
                 (gulex-lexer-get-token (token-stream-lexer ts)
                                        (token-stream-cs ts)))))
    (token-stream-token-set! ts ret)
    ret))

;; Put back token.
(define (token-stream-put ts token)
  (token-stream-buf-set! ts (append (token-stream-buf ts) (list token))))

;; Get current char-stream line.
(define (token-stream-name ts)
  (char-stream-name (token-stream-cs ts)))

;; Get current char-stream line.
(define (token-stream-line ts)
  (char-stream-line (token-stream-cs ts)))

;; Get previous char-stream line.
(define (token-stream-line-prev ts)
  (char-stream-line-prev (token-stream-cs ts)))


;; ------------------------------------------------------------
;; Common:

;; Convert:
;;     ((<token> <lexer-def>) ...)
;; TO
;;     (opt (tok <token> <lexer-def>) ...)
(define (lexer-table->lexer-ir lexer-table)
  (cons 'opt (map (lambda (p)
                    (cons 'tok p))
                  lexer-table)))


(define gulex-show-token-active #f)

;; Turn on token display with
;;     (gulex-show-token)
;;     (gulex-show-token #t)
;; Turn off token display with
;;     (gulex-show-token #f)
(define (gulex-show-token . rest)
  (set! gulex-show-token-active (if (pair? rest)
                                    (car rest)
                                    #t)))

;; Return: (<token> <lexeme> <file> <line>)
(define (gulex-lexer-get-token lexer char-stream)
  (let* ((file (char-stream-file char-stream))
         (line (comp-1+ (char-stream-line char-stream)))
         (lex-ret (lexer char-stream))
         (ret (list (first lex-ret)
                    (list->string (second lex-ret))
                    file
                    line)))
    (when gulex-show-token-active
      (pr "GULEX: Token: " (comp-datum->string ret)))
    ret))



(define gulex-token-type  first)
(define gulex-token-value second)
(define gulex-token-file  third)
(define gulex-token-line  fourth)
