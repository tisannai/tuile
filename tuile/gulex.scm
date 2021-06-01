(define-module (tuile gulex)
  #:use-module (tuile pr)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:export
  (
   char-stream
   char-stream-open
   char-stream-close
   char-stream-get
   char-stream-put
   char-stream-line
   char-stream-line-prev
   token-stream
   token-stream-open
   token-stream-close
   token-stream-get
   token-stream-put
   token-stream-line
   token-stream-line-prev
   parse-gulex-token-table
   lexer-table-to-lexer-top
   lex-interp-entry
   ))


;; gulex-token-table example:
;;
;;(define gulex-token-table
;;  (list
;;
;;;;   '("guile"                 GUILE)
;;;;   '("guilo"                 GUILO)
;;;;   '("[a-m]?"                LEFTCHAR)
;;;;   '("[a-m]+n"                LEFTCHAR)
;;   '("[a-m]*n"                LEFTCHAR)
;;   '("[a-z]+"                ID)
;;   '("[0-9]+"                NUM)
;;   '("[ \t]+"                SPACE)
;;   
;;;;   '("."                    UNKNOWN)
;;
;;;;   '("[a-z]+"                ID)
;;;;   '("[0-9]+"                NUM)
;;
;;;;   '("[a-z]"                 ID)        ; (sel (ran #\a #\z))
;;;;   '("[abc]"                 ID2)       ; (sel #\a #\b #\c)
;;;;   '("[^abc]"                ID3)       ; (inv (sel #\a #\b #\c))
;;;;   '("[0-9]+"                INT)       ; (oom (sel (ran #\0 #\9)))
;;;;   '("[0-9]?"                INT)       ; (zoo (sel (ran #\0 #\9)))
;;;;   '("[0-9]+.[0-9]+"         FLOAT)     ; (oom (sel (ran #\0 #\9)))
;;;;                                        ; (any)
;;;;                                        ; (oom (sel (ran #\0 #\9)))
;;;;   '("guile"                 GUILE)     ; #\g #\u #\i #\l #\e
;;;;   '("(ab|cd)"               PREFIX)    ; (opt (seq #\a #\b) (seq #\c #\d))
;;;;
;;;;   '("\\("                   LEFT-PAR)  ; #\(
;;;;   '("\\)"                   RIGHT-PAR) ; #\)
;;;;   '("//.*"                  COMMENT)   ; #\/ #\/ (zom (any))
;;;;   '("\n"                    NEWLINE)   ; #\newline
;;
;;   ))



;; Proper C block comment regex:
;;   /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/

;; sel - select from chars
;; zoo - zero or one
;; zom - zero or more
;; oom - one  or more
;; ran - range
;; any - any char
;; inv - not
;; opt - options
;; seq - sequence


;; ------------------------------------------------------------
;; Char stream:

(define-record-type char-stream
  (make-char-stream name type port char)
  char-stream?
  (name  char-stream-name)      ; Name of stream (filename for files)
  (type  char-stream-type)      ; Stream type (file, string)
  (port  char-stream-port)      ; Port of stream
  (char  char-stream-char set-char-stream-char!)) ; Last char

(define (char-stream-open name type)
  (let ((cs (make-char-stream name
                              type
                              (case type
                                ((file)   (open-input-file   name))
                                ((string) (open-input-string name))
                                (else
                                 (error (ss "char-stream: Invalid stream type: " type))))
                              #f)))
    (char-stream-get cs)
    cs))

(define (char-stream-close cs)
  (close-input-port (char-stream-port cs)))

(define (char-stream-get cs)
  (let ((ret (get-char (char-stream-port cs))))
    (set-char-stream-char! cs ret)
    ret))

(define (char-stream-put cs ch)
  (unget-char (char-stream-port cs) ch)
  (set-char-stream-char! cs #f))

(define (char-stream-line cs)
  (port-line (char-stream-port cs)))

(define (char-stream-line-prev cs)
  (if (and (char-stream-char cs)
           (char=? #\newline (char-stream-char cs)))
      (port-line (char-stream-port cs))
      (1+ (port-line (char-stream-port cs)))))


;; ------------------------------------------------------------
;; Lexer definition parser:

;; Parse regexp and return the corresponding lexer IR.
(define (parse-regexp-entry regexp)

  (define cs (cons 0 (string->list regexp)))

  (define (datum-to-string datum)
    (with-output-to-string (lambda () (write datum))))

  ;; Recursive regexp parser.
  (define (parse-regexp group? opt?)

    (define parse-debug #f)

    (define (dbug msg)
      (when parse-debug
        (pr "PARSE: " msg)))

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
          (when (not lookahead)
            (err "Missing repeatable item"))
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
          (when (not lookahead)
            (err "Missing repeatable item"))
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
          ;; (dbug (datum-to-string lexers))
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
(define (parse-gulex-token-table table)
  (map (lambda (e)
         (list (second e)
               (parse-regexp-entry (first e))))
       table))


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

  ;; Enable debug printing.
  (define lex-debug #f)

  ;; Debug message display.
  (define (dbug msg)
    (when lex-debug
      (pr "LEX: " msg)))

  ;; Convert datum to string representation.
  (define (datum-to-string datum)
    (with-output-to-string (lambda () (write datum))))

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

  ;; Put back m, but make the first current "char".
  ;; Return: empty list.
  (define (put-back m)
    (dbug (ss "put-back: " (list->string m)))
    (when (pair? m)
      (for-each (lambda (ch)
                  (char-stream-put char-stream ch))
                (reverse (cdr m)))
      (dbug (ss "put-back, char: " (car m)))
      (set-char-stream-char! char-stream (car m)))
    '())

  ;; Check "ch" agains lexer.
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

        (dbug (ss "Start: token: " token ", lexer: " (datum-to-string lexer) ", char: " (cur)))

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
                            (return #f (put-back (append m (list (cur)))))))))

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
                         (return #f (put-back (append m (list (cur))))))))))

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

          (let loop-opt ((lexer (cdr lexer))
                         (m '()))

            (if (pair? lexer)

                ;; Continue.
                (let ((ret (lex-interp token
                                       (car lexer)
                                       char-stream)))
                  (cond

                   ((first ret)
                    (dbug (ss "Options match: " token))
                    (return (first ret) (second ret)))

                   ((eof?)
                    (dbug (ss "Options failure: no data"))
                    (return #f (put-back m)))

                   (else
                    (dbug (ss "Options mismatch"))
                    (loop-opt (cdr lexer)
                              '()))))

                ;; Matching done (failure).
                (begin
                  (dbug (ss "Options failure: no matches"))
                  (return #f (put-back m))))))

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



;; ------------------------------------------------------------
;; Token stream:

(define-record-type token-stream
  (make-token-stream cs lexer buf)
  token-stream?
  (cs    token-stream-cs)                         ; Char-stream.
  (lexer token-stream-lexer)                      ; Lexer (top).
  (buf   token-stream-buf set-token-stream-buf!))        ; Buffer for put-back.

;; Open token stream for name (file/string) and type
;; (file/string). Use lexer-table for token extraction rules.
(define (token-stream-open name type lexer-table)
  (let ((cs (char-stream-open name type))
        (lexer (lexer-table-to-lexer-top lexer-table)))
    (make-token-stream cs lexer '())))

;; Close token stream.
(define (token-stream-close ts)
  (char-stream-close (token-stream-cs ts)))

;; Get next token.
(define (token-stream-get ts)
  (if (pair? (token-stream-buf ts))
      (let ((ret (car (token-stream-buf ts))))
        (set-token-stream-buf! ts (cdr (token-stream-buf ts)))
        ret)
      (lex-interp-entry (token-stream-lexer ts)
                        (token-stream-cs ts))))

(define (token-stream-put ts token)
  (set-token-stream-buf! ts (append (token-stream-buf ts) (list token))))

(define (token-stream-line ts)
  (char-stream-line (token-stream-cs ts)))

(define (token-stream-line-prev ts)
  (char-stream-line-prev (token-stream-cs ts)))


;; ------------------------------------------------------------
;; Common:

;; Convert:
;;     ((<token> <lexer-def>) ...)
;; TO
;;     (opt (tok <token> <lexer-def>) ...)
(define (lexer-table-to-lexer-top lexer-table)
  (cons 'opt (map (lambda (p)
                    (cons 'tok p))
                  lexer-table)))


(define (lex-interp-entry lexer-top char-stream)
  (let ((ret (lex-interp #f lexer-top char-stream)))
    (cons (first ret)
          (list->string (second ret)))))
