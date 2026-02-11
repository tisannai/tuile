(define-module (tuile edit)
  #:use-module ((tuile pr) #:select (ss))
  #:use-module (tuile utils)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile re)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 string-fun)

  #:replace
  (
   read
   from-to
   )

  #:export
  (
   edit
   save
   lines
   get
   ref
   line
   step
   firstline
   lastline
   linecount
   set
   match?
   match-re?
   sub
   sub-re
   update
   insert
   insert-step
   remove
   insertfile
   insertfile-step
   clear
   find
   find-re
   search
   search-re
   filename
   dirty
   edited?
   within?
   excursion
   mark
   markgo
   unmark
   edit-catch
   edit-raise
   edit-using
   ))


;; If opts is non-null, eval "then" else eval "else".
;;
;;     (if-opt then else)
;;
(define-syntax if-opt
  (lambda (x)
    (syntax-case x ()
      ((_ then else)
       (with-syntax ((opt (datum->syntax x 'opt))
                     (opts (datum->syntax x 'opts)))
         #'(if (pair? opts)
               (let ((opt (car opts)))
                 then)
               else))))))


;; Internal Edit State per edited/accessed file.
(define-record-type state
  (fields
   (mutable filename)               ; Edit file name.
   (mutable lines)                  ; File content as vector of lines.
   (mutable line)                   ; Current line number.
   (mutable mark)                   ; Default mark.
   (mutable marks)                  ; Named marks (hash).
   (mutable writable)               ; File is writable (not effective ATM).
   (mutable edited)                 ; Edited (dirty) flag.
   ))



;; ------------------------------------------------------------
;; Edit API:

;; Read file and return "edit" object.
(define (read filename)
  (make-state filename
              (read-file-content filename)
              0
              #f
              (make-hash-table)
              #f
              #f))


;; Read and edit file and return "edit" object.
(define (edit filename)
  (make-state filename
              (if (file-exists? filename)
                  (read-file-content filename)
                  #())
              0
              #f
              (make-hash-table)
              #t
              #f))


;; Save "edit" or named file.
(define (save s . opts)
  (if-opt
   (save-file s opt)
   (begin
     (save-file s (state-filename s))
     (mark-clean s))))


;; All lines as list.
(define (lines s)
  (vector->list (state-lines s)))


;; Get current line or range starting from current (inclusive).
;;     (get s)
;;     (get s 10)
(define (get s . opts)
  (if-opt
   (vector->list
    (vector-range (state-lines s)
                  (state-line s)
                  (1+ (abs-index s (+ (state-line s) (user-pos-to-index s opt))))))
   (current-line s)))


;; Reference any line or line range.
;;
;;     (ref s)           ; Current line.
;;     (ref s 10)        ; Line 10.
;;     (ref s 10 16)     ; Lines 10-16.
(define (ref s . args)
  (cond
   ((null? args) (current-line s))
   ((>= (length args) 2)
    (let-values (((a b) (user-normalize-indices s (lr0 args) (lr1 args))))
      (vector->list
       (vector-range (state-lines s)
                     a
                     (1+ b)))))
   ((and (number? (lr0 args))
         (> (lr0 args) (linecount s)))
    #f)
   (else (get-line s (user-pos-to-index s (lr0 args))))))


;; Get or set current line.
;;     (line s)          ; Return current line.
;;     (line s 10)       ; Set current line to 10.
(define (line s . opts)
  (if-opt
   (set-line! s (user-pos-to-index s opt))
   (1+ (state-line s))))


;; Step by 1 or by specified amount.
;;     (step s)          ; Step 1 forward.
;;     (step s 1)        ; Step 1 forward.
;;     (step s -1)       ; Step 1 backward.
(define (step s . opts)
  (set-line! s (abs-index s (+ (state-line s)
                               (if-opt opt 1)))))

;; Jump to first line.
(define (firstline s)
  (set-line! s 0))


;; Jump to last line.
(define (lastline s)
  (set-line! s (- (linecount s) 1)))


;; Return line count.
(define (linecount s)
  (vector-length (state-lines s)))


;; Set text for current line.
;;     (set s "foobar")
(define (set s text)
  (mark-dirty s)
  (set-text! s text))


;; Does current line include str?
(define (match? s str)
  (string-contains (current-line s) str))


;; Does current line match regex restr?
(define (match-re? s restr)
  (re-match? restr (current-line s)))


;; Substitute current line text with "from" "to".
(define (sub s from to)
  (when (match? s from)
    (mark-dirty s)
    (set-text! s (string-replace-substring
                  (current-line s)
                  from
                  to))))


;; Substitute current line text with regex "from" "to".
(define (sub-re s from to)
  (when (match-re? s from)
    (mark-dirty s)
    (set-text! s (re-gsub from (current-line s) to))))


;; Update current line content with "fn".
;;     (update s (lambda (line) (sub s "foo" "bar")))
(define (update s fn)
  (mark-dirty s)
  (set-text! s (apply fn (list (current-line s)))))


;; Insert lines and move to insertion position.
;;
;; Position: <num>, 'first, 'after, 'last, 'end.
;;
;; args:
;;   <none>     Insert empty line at current position.
;;   text       Insert text at current position.
;;   text pos   Insert text at given position and move to it.
(define (insert s . args)
  (mark-dirty s)
  (let-values (((index count) (insert-lines s args)))
    (set-line! s index)))


;; Insert lines and move to last inserted line.
;;
;; Position: <num>, 'first, 'after, 'last, 'end.
;;
;; args:
;;   <none>     Insert empty line at current position.
;;   text       Insert text at current position.
;;   text pos   Insert text at given position and move to it.
(define (insert-step s . args)
  (mark-dirty s)
  (let-values (((index count) (insert-lines s args)))
    (set-line! s (+ index count))))


;; Remove current line or number of lines.
;;    (remove s)            ; Remove current line and step 1.
;;    (remove s 10)         ; Remove 10 lines at current line.
(define (remove s . args)
  (mark-dirty s)
  (state-lines-set! s (vector-delete (state-lines s)
                                     (state-line s)
                                     (default-unsigned-count
                                       args
                                       1))))


;; Insert file to current or named position.
;;     (insertfile s myfile)
;;     (insertfile s myfile 10)
(define (insertfile s filename . opts)
  (insert s (read-file-content filename) (if-opt opt (state-line s))))


;; Insert file to current position and step.
;;     (insertfile-step s myfile)
;;     (insertfile-step s myfile 10)
(define (insertfile-step s filename . opts)
  (insert-step s (read-file-content filename)
               (if-opt opt (state-line s))))


;; Clear content and reset current line.
(define (clear s)
  (mark-dirty s)
  (state-lines-set! s #())
  (state-line-set! s 0))


;; Find literal string forwards or backwards. Fail with
;; expection (edit-find-error) if not found.
;;     (find s "foo")           ; Find forward.
;;     (find s "foo" #f)        ; Find backward.
(define (find s str . opts)
  (let* ((fwd (if-opt opt #t))
         (res (find-or-fail s #f str fwd)))
    (if res
        (set-line! s res)
        (edit-raise 'edit-find-error (ss "Pattern not found: " str)))))


;; Find regex forwards or backwards. Fail with
;; expection (edit-find-error) if not found.
;;     (find-re s "fo[o|a]")       ; Find forward.
;;     (find-re s "fo[o|a]" #f)    ; Find backward.
(define (find-re s restr . opts)
  (let ((fwd (if-opt opt #t)))
    (let ((res (find-or-fail s #t restr fwd)))
      (if res
          (set-line! s res)
          (edit-raise 'edit-find-error (ss "Pattern not found: " restr))))))


;; Search literal string forwards or backwards. Return true
;; on success.
;;     (search s "foo")           ; Search forward.
;;     (search s "foo" #f)        ; Search backward.
(define (search s str . opts)
  (let ((fwd (if-opt opt #t)))
    (let ((res (find-or-fail s #f str fwd)))
      (if res
          (begin
            (set-line! s res)
            #t)
          #f))))


;; Search regex forwards or backwards. Return true
;; on success.
;;     (search-re s "fo[o|a]")       ; Search forward.
;;     (search-re s "fo[o|a]" #f)    ; Search backward.
(define (search-re s restr . opts)
  (let ((fwd (if-opt opt #t)))
    (let ((res (find-or-fail s #t restr fwd)))
      (if res
          (begin
            (set-line! s res)
            #t)
          #f))))


;; Return used file name.
(define (filename s) (state-filename s))


;; Mark content dirty.
(define (dirty s) (mark-dirty s))


;; Is content edited?
(define (edited? s) (state-edited s))


;; Is line within content limits?
(define (within? s) (<= (state-line s) (linecount s)))


;; Execute block, retain current position, and return block value.
;;     (excursion s (lambda (s) (find s "foo") (get s)))
(define (excursion s fn)
  (let* ((orgline (state-line s))
         (ret (fn s)))
    (set-line! s orgline)
    ret))


;; Mark (store) current position to default or to named mark.
;;     (mark s)           ; Default mark.
;;     (mark s 'mymark)   ; Named mark.
(define (mark s . opts)
  (if-opt
   (hash-set! (state-marks s)
              opt
              (+ (state-line s) 1))
   (state-mark-set! s (+ (state-line s) 1))))


;; Goto default or named mark.
;;     (markgo s)           ; Default mark.
;;     (markgo s 'mymark)   ; Named mark.
(define (markgo s . opts)
  (let ((opt (if-opt opt #f)))
    (if (and opt
             (hash-ref (state-marks s) opt))
        (set-line! s (hash-ref (state-marks s) opt))
        (when (state-mark s)
          (set-line! s (- (state-mark s) 1))))))


;; Unmark (remove) default or named mark.
;;     (unmark s)           ; Default mark.
;;     (unmark s 'mymark)   ; Named mark.
(define (unmark s . opts)
  (let ((opt (if-opt opt #f)))
    (if (and opt
             (hash-ref (state-marks s) opt))
        (hash-remove! (state-marks s) opt)
        (state-mark-set! s #f))))


;; Perform "fn" for all lines in from-to.
;;     (from-to s 'first 10 (lambda (s) (sub s "foo" "bar")))
(define (from-to s from to fn)
  (let-values (((a b) (user-normalize-indices s from to)))
    (let ((orgline (state-line s)))
      (set-line! s a)
      (let loop ((i a))
        (fn s)
        (when (< i b)
          (set-line! s (1+ (state-line s)))
          (loop (+ i 1))))
      (set-line! s orgline))))


;; Catch/handle Edit Exception.
;;
;; With handler as #f catch no exception, and with #t catch all.
;; Otherwise supply single or a list of symbols which correspond to
;; exception identifiers (edit-file-error, edit-find-error).
;;
;;     (edit-catch #t
;;                 (find "diiduu"))
;;
(define-syntax edit-catch
  (lambda (x)
    (syntax-case x ()
      ((_ handler body ...)
       #'(begin
           (edit-handle-exception handler
                                  (lambda ()
                                    body ...)))))))

;; Raise Edit Exception.
;;
;; Exceptions:
;;     edit-file-error     - file can't be opened
;;     edit-find-error     - search failed
(define (edit-raise exn msg)
  (raise-exception (cons exn msg) #:continuable? #t))



;; ------------------------------------------------------------
;; Support routines:

;; Default exception handler.
(define (edit-exception-handler-default handler)
  (lambda (exn)
    (define (report exn)
      (display "(tuile edit): ")
      (display (cdr exn))
      (newline))
    (cond
     ((symbol? handler)
      (cond
       ((eq? (car exn) handler) #f)
       (else (raise-exception exn #:continuable? #t))))
     ((list? handler)
      (if (member (car exn) handler)
          #f
          (report exn)))
     (else (raise-exception exn #:continuable? #t)))))

;; Ignoring exception handler.
(define (edit-exception-handler-ignore exn) #f)

;; Handle given exception.
;;
;; If exception is #f, the exception will be handled without exiting
;; the program.
(define (edit-handle-exception handler thunk)
  ;; Exit with exception.
  (with-exception-handler
      (cond
       ((not handler) raise-exception)
       ((boolean? handler) edit-exception-handler-ignore)
       (else (edit-exception-handler-default handler)))
    thunk
    #:unwind? #t
    ))

;; Return given count of default.
(define (default-unsigned-count args default)
  (if (pair? args)
      (if (> (car args) default)
          (car args)
          default)
      default))

;; Read file to a vector.
(define (read-file-content filename)
  (with-exception-handler
      (lambda (exn)
        (edit-raise 'edit-file-error (ss "Can't access file: " filename)))
    (lambda ()
      (list->vector
       (map (lambda (line)
              (if (string-null? line)
                  #f
                  line))
            (file->lines filename))))
    #:unwind? #t))

;; Return indexed line content.
(define (get-line s line)
  (if (< line (linecount s))
      (vector-ref (state-lines s) line)
      #f))

;; Return current line content.
(define (current-line s)
  (get-line s (state-line s)))

;; Set line index.
(define (set-line! s line)
  (state-line-set! s
                   (abs-index s line)))

;; Fix added line content.
(define (fix-line line)
  (string-trim-right line
                     (lambda (ch)
                       (char-set-contains? char-set:whitespace ch))))

;; Set current line content.
(define (set-text! s text)
  (vector-set! (state-lines s)
               (state-line s)
               (fix-line text)))

;; Save content to filename.
(define (save-file s filename)
    (call-with-output-file filename
      (lambda (port)
        (for-list! (line (vector->list (state-lines s)))
                   (when line
                     (display line port))
                   (newline port)))))

;; Make edited status dirty.
(define (mark-dirty s)
  (state-edited-set! s #t))

;; Make edited status clean.
(define (mark-clean s)
  (state-edited-set! s #f))

;; Convert arguments to vector.
;;
;; Do nothing if already a vector.
(define (args-to-vector args)
  (cond
   ((vector? args) args)
   ((list? args) (list->vector args))
   (else (vector args))))

;; Return absolute index.
(define (abs-index s a)
  (cond
   ((< a 0) (+ (linecount s) a))
   ((>= a (linecount s)) (1- (linecount s)))
   (else a)))

;; Return multiple-values of 2.
(define (user-normalize-indices s au bu)
  (let ((a (user-pos-to-index s au))
        (b (user-pos-to-index s bu)))
    (if (> a b)
        (values b a)
        (values a b))))

;; Convert position to index.
(define (pos-to-index s pos)
  (cond
   ((number? pos)     (abs-index s pos))
   ((eq? pos 'first)  0)
   ((eq? pos 'after)  (+ (state-line s) 1))
   ((eq? pos 'last)   (- (linecount s) 1))
   ((eq? pos 'end)    (linecount s))))

;; Convert user position to internal position.
(define (user-pos-to-index s pos)
  (cond
   ((number? pos) (pos-to-index s (1- pos)))
   (else (pos-to-index s pos))))

;; Insert lines.
(define (insert-lines s args)
  (let-values (((text index)
                (cond
                 ((>= (length args) 2)
                  (values (args-to-vector (first args))
                          (user-pos-to-index s (second args))))
                 ((>= (length args) 1)
                  (values (args-to-vector (first args))
                          (state-line s)))
                 (else
                  (values (args-to-vector #f)
                          (state-line s))))))
    (if (= index (linecount s))
        (state-lines-set! s (vector-append (state-lines s) text))
        (state-lines-set! s (vector-insert (state-lines s) index text)))
    (values index (vector-length text))))

;; Find re-or-str (or fail) to given direction.
(define (find-or-fail s re-find re-or-str forward)
  (let ((line (state-line s))
        (len (linecount s)))
    (let-values (((off limcmp lim) (if forward
                                       (values + < len)
                                       (values - >= 0))))
      (let ((patcmp (if re-find
                        (let ((re (re-comp re-or-str)))
                          (lambda (line) (re-match? re line)))
                        (lambda (line) (string-contains line re-or-str)))))
        (call/cc
         (lambda (cc)
           (let loop ((line line))
             (when (limcmp line lim)
               (when (and (get-line s line)
                          (patcmp (get-line s line)))
                 (cc line))
               (loop (off line 1))))
           #f))))))


;; Convenience macro for using (tuile edit).
;;
;; Example:
;;
;;     (eu:edit-using #{eu:}# "README.md"
;;                    (let ((line (get)))
;;                      (pr line)))
;;
(define-syntax edit-using
  (lambda (x)

    ;; Helper to transform a list of datums.
    (define (expand-datums pfix state datums)

      (define eu-prefix (symbol-prefix-proc pfix))

      (define (expand-datum datum)

        (define (is-edit-proc? name)
          (case name
            (( ;; read ; no fix
              from-to
              ;; edit ; no fix
              save
              lines
              get
              ref
              line
              step
              firstline
              lastline
              linecount
              set
              match?
              match-re?
              sub
              sub-re
              update
              insert
              insert-step
              remove
              insertfile
              insertfile-step
              clear
              find
              find-re
              search
              search-re
              filename
              dirty
              edited?
              within?
              excursion
              mark
              markgo
              unmark
              ;; edit-catch ; no fix
              ;; edit-raise ; no fix
              )
             #t)
            (else #f)))

        (define (replace-recursive datum)

          (define (replace-list lst)
            (let lp ((lst lst)
                     (index 0)
                     (ret '()))
              (if (pair? lst)
                  (let ((item (car lst)))
                    (if (list? item)
                        (lp (cdr lst)
                            (1+ index)
                            (cons (lp item 0 '()) ret))
                        (if (and (is-edit-proc? item)
                                 (= index 0))
                            (lp (cdr lst)
                                (1+ index)
                                (cons state (cons (eu-prefix item) ret)))
                            (lp (cdr lst)
                                (1+ index)
                                (cons item ret)))))
                  (reverse ret))))

          (cond
           ((null? datum) datum)
           ((pair? datum)
            (case (car datum)
              ((let let*)
               ;; Don't replace let defined variables, only values and body.
               (let ((vardefs (map (lambda (vardef)
                                     `(,(first vardef) ,(replace-recursive (second vardef))))
                                   (second datum))))
                 `(,(car datum) ,vardefs
                   ,@(map replace-recursive (cddr datum)))))
              (else (replace-list datum))))
           (else datum)))

        (replace-recursive datum))

      (let lp ((datums datums))
        (if (pair? datums)
            (let ((datum (car datums)))
              (cons (expand-datum datum) (lp (cdr datums))))
            '())))

    (syntax-case x ()
      ((_ pfix filename body ...)
       (with-syntax ((state (datum->syntax x 'state)))
         #`(let ((state (edit filename)))
             #,@(datum->syntax x (expand-datums
                                  (syntax->datum (syntax pfix))
                                  (syntax->datum (syntax state))
                                  (syntax->datum (syntax (body ...)))))))))))
