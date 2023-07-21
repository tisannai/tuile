;; Gulex - Efficient lexer library.

;; Gulex is a lexer library with layered structure. The layers (from
;; bottom up):
;;
;; * char-stream: Provide single characters for token layer.
;;
;; * token-stream: Provide tokens for parser layer.
;;
;; Gulex is lexer which has (currently) two incarnations: interpreter
;; and FSM. Interpreter is easier to debug and understand, and FSM is
;; more performant.
;;
;; Gulex is registered to the token-stream as the "token
;; creator". Gulex is provided with the Gulex Token Table, which
;; defines Regexp to Token mappings.
;;
;; For usage and further documentation: see `gulex`, the Gulex CLI tool README.md.

(define-module (tuile gulex)
  #:use-module (common base)
  #:use-module (tuile pr)
  #:use-module ((tuile utils) #:select (datum->string))
  #:use-module (srfi srfi-1)
  #:use-module ((ice-9 textual-ports) #:select (get-string-all))
;;   #:use-module (tuile compatible)
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:export
  (
   char-stream-open
   char-stream-open-with-text
   char-stream-close
   char-stream-at-eof?
   char-stream-is-eof?
   char-stream-char
   char-stream-get
   char-stream-put
   char-stream-file
   char-stream-line
   char-stream-column

   token-stream-open
   token-stream-open-with-text
   token-stream-close
   token-stream-use-stream
   token-stream-push-file
   token-stream-token
   token-stream-lexer-get
   token-stream-get
;;   token-stream-put
   token-stream-file
   token-stream-line
   token-stream-column
   token-stream-show-token

   token-type
   token-value
   token-file
   token-line
   token-column

   gulex-parse-token-table
   gulex-create-lexer-interp
   gulex-create-lexer-fsm
   gulex-token-table->lexer-ir

   ))


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
  (fields (mutable file)       ; Filename of stream.
          text                 ; String of file/text chars.
          (mutable index)      ; Current char index.
          (mutable lineno)     ; Line number info, pair: index, lineno
          (mutable column)     ; Column number of Line
          ))


(define (char-stream-open filename)
  (if (file-exists? filename)
      (make-char-stream filename
                        (call-with-input-file filename
                          (lambda (port) (get-string-all port)))
                        0
                        0
                        0)
      (error (ss "char-stream: File not found: \"" filename "\" ..."))))


(define (char-stream-open-with-text text)
  (make-char-stream "<unknown>"
                    text
                    0
                    0
                    0))

(define (char-stream-close cs)
  #t)

;; EOF marker (symbol, i.e. a non-char).
(define char-stream-eof-char 'eof)

(define (char-stream-at-eof? cs)
  (>= (char-stream-index cs)
      (string-length (char-stream-text cs))))

(define (char-stream-is-eof? sym)
  (and (symbol? sym) (eq? sym char-stream-eof-char)))

(define (char-stream-char cs)
  (if (char-stream-at-eof? cs)
      char-stream-eof-char
      (string-ref (char-stream-text cs)
                  (char-stream-index cs))))

(define (char-stream-change-linenum! cs change)
  (char-stream-lineno-set! cs (+ (char-stream-lineno cs) change)))

(define (char-stream-change-column! cs change)
  (char-stream-column-set! cs (+ (char-stream-column cs) change)))

(define (char-stream-change-index! cs change)
  (char-stream-index-set! cs (+ (char-stream-index cs) change)))

(define (char-stream-get cs)
  (let ((ret (char-stream-char cs)))
    (when ret
      (if (char=? ret #\newline)
          (begin
            (char-stream-change-linenum! cs 1)
            (char-stream-column-set! cs 0))
          (char-stream-change-column! cs 1))
      (char-stream-change-index! cs 1))
    ret))

;; Put back one or more chars. Note that chars must be in latest first
;; order.
(define (char-stream-put cs char-or-chars)
  (let* ((chars (if (list? char-or-chars) char-or-chars (list char-or-chars)))
         (nl-count (fold + 0 (map (lambda (i) (if (char=? i #\newline) 1 0)) chars))))
    (char-stream-change-index! cs (- (length chars)))
    (char-stream-change-linenum! cs (- nl-count))
    (char-stream-column-set! cs (let loop ((index (char-stream-index cs))
                                           (count 0))
                                  (if (or (= index 0)
                                          (char=? (string-ref (char-stream-text cs) (1- index))
                                                  #\newline))
                                      count
                                      (loop (1- index)
                                            (1+ count)))))
    (char-stream-char cs)))

(define (char-stream-line cs)
  (char-stream-lineno cs))




;; ------------------------------------------------------------
;; Token stream:

(define-record-type token-stream
  (fields (mutable cs)                  ; Char-stream (stack, top at head).
          lexer                         ; Lexer (interp, fsm, ...).
          (mutable token)               ; Current token.
          (mutable stack)               ; Char-stream and cur-token stack: ( (cs . token) ...).
          (mutable buf)))               ; Buffer for put-back.

;; Open token stream for filename or text (mutually exclusive). Use
;; lexer for token extraction rules.
(define (token-stream-common-open filename text lexer)
  (let* ((cs (if filename
                 (char-stream-open filename)
                 (char-stream-open-with-text text)))
         (ts (make-token-stream cs lexer #f '() '())))
    ;; Prefetch first token.
    (token-stream-get ts)
    ts))

;; Open token stream for filename.
(define (token-stream-open filename lexer)
  (token-stream-common-open filename #f lexer))

;; Open token stream for text.
(define (token-stream-open-with-text text lexer)
  (token-stream-common-open #f text lexer))

;; Close token stream.
(define (token-stream-close ts)
  (char-stream-close (token-stream-cs ts)))

;; Change char-stream.
(define (token-stream-use-stream ts cs)
  (char-stream-close (token-stream-cs ts))
  (token-stream-cs-set! ts cs)
  ;; Prefetch first token.
  (token-stream-get ts)
  ts)

(define (token-stream-push-file ts filename)
  ;; TODO: error if there is stuff in "buf".

  ;; Push current state to stack: (char-stream . token).
  (token-stream-stack-set! ts (cons (cons (token-stream-cs ts)
                                          (token-stream-token ts))
                                    (token-stream-stack ts)))

  ;; Set new file as current char stream.
  (token-stream-cs-set! ts (char-stream-open filename))
  (token-stream-get ts)
  ts)

;; Get token from lexer.
(define (token-stream-lexer-get ts)
  (let* ((char-stream (token-stream-cs ts))
         (lexer (token-stream-lexer ts))
         (file (char-stream-file char-stream))
         (line (1+ (char-stream-line char-stream)))
         (column (1+ (char-stream-column char-stream)))
         (lex-ret (lexer char-stream))
         (ret (list (first lex-ret)
                    (list->string (second lex-ret))
                    file
                    line
                    column)))
    (when token-stream-show-token-active
      (pr "Token Stream: " (datum->string ret)))
    ret))

;; Get next token (from lexer or from buf).
;;
;; Return:
;;
;;     (<token> <lexeme> <file> <line>)
;;
(define (token-stream-get ts)
  ;; Try to get token from current file, but if eof is encountered and
  ;; there are still char-streams on the stack, just pop the stack and
  ;; continue with the next char-stream of the stack.
  (let loop ((token (token-stream-lexer-get ts)))

    (if (char-stream-at-eof? (token-stream-cs ts))
        (if (pair? (token-stream-stack ts))
            ;; Stack non-empty, i.e. we continue with the older file.
            (let ((top (car (token-stream-stack ts))))
              (token-stream-stack-set! ts (cdr (token-stream-stack ts)))
              (token-stream-cs-set! ts (car top))
              (token-stream-token-set! ts (cdr top))
              (loop (token-stream-token ts)))
            (begin
              (token-stream-token-set! ts token)
              token))
        (begin
          (token-stream-token-set! ts token)
          token)))


  ;; Old version with token putback.
  ;;  (let ((ret (if (pair? (token-stream-buf ts))
  ;;                 (let ((ret (car (token-stream-buf ts))))
  ;;                   (token-stream-buf-set! ts (cdr (token-stream-buf ts)))
  ;;                   ret)
  ;;                 (token-stream-lexer-get ts))))
  ;;    (token-stream-token-set! ts ret)
  ;;    ret)

  )

;; Put back token.
;;
;; TODO: The current version of "put" does not work, since we have
;; current token lookahead and this is not accounted for here.
;;
;;(define (token-stream-put ts token)
;;  (token-stream-buf-set! ts (append (token-stream-buf ts) (list token))))

;; Get current char-stream line.
(define (token-stream-file ts)
  (char-stream-file (token-stream-cs ts)))

;; Get current char-stream line.
(define (token-stream-line ts)
  (char-stream-line (token-stream-cs ts)))

;; Get current char-stream column.
(define (token-stream-column ts)
  (char-stream-column (token-stream-cs ts)))

(define token-stream-show-token-active #f)

;; Turn on token display with
;;     (token-stream-show-token)
;;     (token-stream-show-token #t)
;;
;; Turn off token display with
;;     (token-stream-show-token #f)
;;
(define (token-stream-show-token . rest)
  (set! token-stream-show-token-active (if (pair? rest)
                                           (car rest)
                                           #t)))


;; ------------------------------------------------------------
;; Token attribute access:

;; Token: (<token> <lexeme> <file> <line> <column>)

(define token-type   first)
(define token-value  second)
(define token-file   third)
(define token-line   fourth)
(define token-column fifth)



;; ------------------------------------------------------------
;; Lexer definition parser:


;; Return lexer-table: ((token lexer-def) ...)
(define (gulex-parse-token-table table)

  ;; Parse regexp and return the corresponding lexer IR.
  (define (parse-regexp-entry regexp)

    (define cs (cons 0 (string->list regexp)))

    ;; Recursive regexp parser.
    (define (parse-regexp group? opt?)

      ;; Debug is expensive, hence do dbug with a macro.
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
        (nth (1+ (car cs))))

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
                     ;; OR
                     ;; [...-]
                     ;;     ^
                     ((is? #\- (peek))
                      (let ((t1 (get)))
                        (if (is? #\] (peek))
                            ;; Literal "-" in char range.
                            (cons t1 (cons (get) (loop)))
                            (begin
                              (use #\-)
                              (cons (list 'ran t1 (use-if normal?))
                                    (loop))))))

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
            ;; (dbug (datum->string lexers))
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


  (map (lambda (e)
         (list (second e)
               (parse-regexp-entry (first e))))
       table))


;; Convert Token Table to Lexer IR.
;;
;; Return lexer-ir: (opt (tok <token> <lexer-def>) ...)
(define (gulex-token-table->lexer-ir table)

  ;; Convert:
  ;;     ((<token> <lexer-def>) ...)
  ;; TO
  ;;     (opt (tok <token> <lexer-def>) ...)
  (define (lexer-table->lexer-ir lexer-table)
    (cons 'opt (map (lambda (p)
                      (cons 'tok p))
                    lexer-table)))

  (lexer-table->lexer-ir (gulex-parse-token-table table)))



;; ------------------------------------------------------------
;; Gulex Lexer interpreter:

(define (gulex-create-lexer-interp gulex-token-table)

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

    ;; Debug is expensive, hence do dbug with a macro.
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
                  (loop (1- i)))
                (get)))))

    ;; Return current char.
    (define (cur)
      (char-stream-char char-stream))

    ;; Is EOF reached?
    (define (eof?)
      (char-stream-at-eof? char-stream))

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

          (dbug (ss "Start: token: " token ", lexer: " (datum->string lexer) ", char: " (cur)))

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
                                      (dbug (ss "Option matches: " (datum->string opts)))
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


  (let ((lexer-ir (gulex-token-table->lexer-ir gulex-token-table)))
    (lambda (char-stream)
      (lex-interp #f lexer-ir char-stream))))


;; ------------------------------------------------------------
;; Gulex Lexer FSM:

;; Lexer node for one char match step. Lnodes are states of the Lexer
;; FSM. Labels run from higher to lower number and terminal state
;; label is 0.
;;
;; Normally nodes have a rule, which is used for matching, and one
;; next node. However, if there are options, there is no rule and
;; there are multiple next nodes.
;;
;; Lnodes create an FSM graph (DAG).
;;
;; Examples:
;;
;;     Char      : #<lnode rule: #\g label: 5 term: #f next: (4)>
;;     Select    : #<lnode rule: (sel (ran #\a #\b)) label: 2 term: #f next: (1)>
;;     Branch    : #<lnode rule: #f label: 3 term: #f next: (2 1)>
;;     Terminate : #<lnode rule: #f label: 0 term: #t next: ()>
;;
(define-record-type lnode
  (fields rule                          ; Match rule.
          label                         ; Node label (index number).
          (mutable next)))              ; Next node or nodes (empty for termination).

(define (lnode-terminal? lnode) (null? (lnode-next lnode)))


;; Convert lexer-ir to lnode-list.
(define (lexer-ir->lnode-list ir)

  (define lnode-index 0)

  (define lnode-list (list))


  (define (lnode-get-label)
    (let ((label lnode-index))
      (set! lnode-index (1+ lnode-index))
      label))

  (define (lnode-add rule next)
    (let ((lnode (make-lnode rule
                             (lnode-get-label)
                             next)))
      (set! lnode-list (cons lnode lnode-list))
      lnode))

  (define (lnode-create-terminal)
    (let ((ret (lnode-add #f (list))))
      (lnode-label ret)))

  (define (lnode-create rule next)
    (lnode-add rule next))

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
    (pr "    rule: " (datum->string (lnode-rule lnode)))
    (if (lnode-terminal? lnode)
        (pr "    <term>")
        (pr "    next: " (datum->string (lnode-next lnode)))))

  (define (show-lnode-list)
    (for-each show-lnode lnode-list))

  (define (try ir)
    (pr "IR: " (datum->string ir))
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


;; Create FSM.
;;
;; Order lnode-list using labels and store the ordered list as vector
;; for efficient lookup.
;;
;; Perform "fsm-reset" before use.
;;
;; Args:
;;     lnode-list Ordered graph of lnodes.
;;
(define (lexer-def->fsm lexer-def)

  (let* ((token (first lexer-def))
         (lnode-list (lexer-ir->lnode-list (second lexer-def)))
         (start (lnode-label (car lnode-list))))
    ;; Uncomment for lnode-listings.
    ;; (pretty-print lnode-list)
    (make-fsm token
              (list->vector
               (stable-sort lnode-list
                            (lambda (a b)
                              (< (lnode-label a)
                                 (lnode-label b)))))
              start
              (list)
              0
              #f
              #f)))


;; FSM based lexer.
;;
;; Args:
;;     fsm-set       Set of FSM lexers.
;;     char-stream   Character stream handle.
;;
;; Return: (token|#f lexeme)
;;
(define (lexer-fsm fsm-set char-stream)

  ;; Debug is expensive, hence do dbug with a macro.
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
    ;;    (eof-object? (char-stream-char char-stream))
    (char-stream-at-eof? char-stream))

  ;; Is char EOF marker?
  (define (eof-char? ch)
    (char-stream-is-eof? ch))

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
                        (lnode-terminal? lnode))
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
         ((char-stream-is-eof? ch)
          #f)
         ((lnode-rule lnode)
          (accept? ch (lnode-rule lnode)))
         (else
          #f)))

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

    (if (char-stream-is-eof? ch)

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
                (fsm-match-count-set! fsm (1+ (fsm-match-count fsm)))
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
  (let* ((lexer-ir (gulex-token-table->lexer-ir gulex-token-table))
         (fsm-set (map (lambda (lexer-table-def)
                         (lexer-def->fsm (cdr lexer-table-def)))
                       (cdr lexer-ir))))
    (lambda (char-stream)
      (lexer-fsm fsm-set char-stream))))
