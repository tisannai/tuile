(define-module (tuile led)
  #:use-module (tuile pr)
  #:use-module (tuile fmt)
  #:use-module (tuile utils)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile re)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)

  #:replace
  (
   read
   )

  #:export (

            read
            edit
            save
            save-as

            size
            at
            text

            goto
            step
            first
            last
            mark
            back
            search
            find

            get
            set
            add
            insert
            append
            include
            remove
            replace
            replace-re
            update
            clear

            ))


;; ------------------------------------------------------------
;; Led State.

;; Internal Gutman State per edited/accessed file.
(define-record-type state
  (fields
   (mutable filename)               ; Gutman file name.
   (mutable text)                   ; Content as vector of lines.
   (mutable at)                     ; Current line number.
   (mutable used)                   ; Current line number.
   (mutable mark)                   ; Default mark.
   ))


;; ------------------------------------------------------------
;; Internal support:

(define (-make-state)
  (make-state #f
              #f
              0
              0
              #f))


(define (-read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (read-lines-from-port port
                            #:as-vector
                            #t))))

(define (-load-file state filename)
  (state-text-set! state (-read-file filename))
  (state-used-set! state (vector-length (state-text state))))


;; Helper for updating the basenamed funcs in the "-using" bodies.
;;
;;     pfix        (<prefix:proc> <proc>)
;;     state       Edit state
;;     datums      List of body statements
;;
(define (-expand-datums pfix state datums)

  (define led-prefix (symbol-prefix-proc
                      (string->symbol
                       (string-replace-substring
                        (symbol->string (car pfix))
                        (symbol->string (cdr pfix))
                        ""))))

  (define (expand-datum datum)

    (define (is-edit-proc? name)
      (case name
        ((
          ;; read
          ;; edit
          save
          save-as

          size
          at
          text

          goto
          step
          first
          last
          mark
          back
          search
          find

          get
          set
          add
          insert
          append
          include
          remove
          replace
          replace-re
          update
          clear
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
                            (cons state (cons (led-prefix item) ret)))
                        (lp (cdr lst)
                            (1+ index)
                            (cons item ret)))))
              (reverse ret))))

      (cond

       ((null? datum) datum)

       ((pair? datum) (match datum

                        ;; NOTE: Special treatment for let and named-let. Created
                        ;; variable may collide with "edit" module funcs, but
                        ;; otherwise user must make sure there are no collisions.

                        ;; Other lets.
                        (((or 'let 'let*) (vardefs ...) body ...)
                         (let ((mapped-vardefs (map (lambda (vardef)
                                                      `(,(first vardef)
                                                        ,(replace-recursive (second vardef))))
                                                    vardefs)))
                           `(,(car datum) ,mapped-vardefs
                             ,@(map replace-recursive body))))
                        ;; Named-let.
                        (('let let-name (vardefs ...) body ...)
                         (let ((mapped-vardefs (map (lambda (vardef)
                                                      `(,(first vardef)
                                                        ,(replace-recursive (second vardef))))
                                                    vardefs)))
                           `(let ,let-name ,mapped-vardefs
                                 ,@(map replace-recursive body))))
                        ;; Non-lets.
                        (else (replace-list datum))))

       (else datum)))

    (replace-recursive datum))

  (let lp ((datums datums))
    (if (pair? datums)
        (let ((datum (car datums)))
          (cons (expand-datum datum) (lp (cdr datums))))
        '())))

(define (-read filename)
  (let ((state (-make-state)))
    (-load-file state filename)
    state))

(define (-edit filename)
  (let ((state (-make-state)))
    (state-filename-set! state filename)
    (-load-file state filename)
    state))


;; Return the index where value was found or #f.
(define (-search state at type value direction)
  (let ((comp-fn (case type
                   ((string)
                    (lambda (line)
                      (string-contains line value)))
                   ((regexp)
                    (lambda (line)
                      (re-match? value line)))
                   (else value)))
        (step-fn (case direction
                   ((forward) 1+)
                   (else 1-)))
        (limit-fn (case direction
                    ((forward) (lambda (at) (< at (state-used state))))
                    (else (lambda (at) (>= at 0))))))
    (let lp ((at at))
      (if (limit-fn at)
          (if (comp-fn (vector-ref (state-text state) at))
              at
              (lp (step-fn at)))
          #f))))


(define (-find state at type value direction)
  (let ((res (-search state at type value direction)))
    (if res
        (begin
          (goto state res)
          res)
        #f)))


(define (-insert-range-at state range at)
  (let* ((rlen (vector-length range))
         (ulen (state-used state))
         (vec (state-text state))
         (vlen (vector-length vec))
         (reqlen (+ ulen rlen)))
    (if (> reqlen vlen)
        ;; Add storage to the vector.
        (let* ((alen (* 2 (if (> reqlen (* 2 vlen))
                              reqlen
                              vlen)))
               (nvec (make-vector alen)))
          ;; ...head...|...range...|...tail...
          ;; |--------|
          (vector-copy! nvec 0 vec 0 at)
          ;; ...head...|...range...|...tail...
          ;;            |---------|
          (vector-copy! nvec at range)
          ;; ...head...|...range...|...tail...
          ;;                        |--------|
          (vector-copy! nvec (+ at rlen) vec at ulen)
          (state-used-set! state (+ ulen rlen))
          (state-text-set! state nvec))
        ;; Copy to existing allocation.
        (begin
          ;; Make space for range.
          ;; .........
          ;; .....|range|....
          ;;       `at
          (vector-move-right! vec at ulen vec (+ at rlen))
          ;; Copy range in place.
          (vector-copy! vec at range)
          (state-used-set! state (+ ulen rlen))))))


(define (-insert-at state text at)
  (cond
   ((vector? text)
    (-insert-range-at state text at)
    (vector-length text))
   ((list? text)
    (-insert-range-at state (list->vector text) at)
    (length text))
   (else
    (-insert-range-at state (vector text) at)
    1)))


(define (-remove-range-at state range at)
  (let ((used-range (if (> (+ at range) (state-used state))
                           (- (state-used state) at)
                           range)))
    (vector-move-left! (state-text state)
                       (+ at used-range)
                       (state-used state)
                       (state-text state)
                       at)))


(define (-update-at state proc at)
  (let ((updated-line (proc (vector-ref (state-text state) at))))
    (when updated-line
      (vector-set! (state-text state) at updated-line))))


(define (-opt-arg opts default)
  (if (null? opts) default (car opts)))


(define (-opt-at opts state)
  (if (null? opts)
      (state-at state)
      (let ((normalize (lambda (at)
                         (if (< at 0)
                             (+ (state-used state) at)
                             at)))
            (sanitize (lambda (at)
                        (cond
                         ((< at 0) 0)
                         ((> at (state-used state)) (1- (state-used state)))
                         (else at)))))
        ((compose sanitize normalize) (car opts)))))


;; ------------------------------------------------------------
;; API:

;; Convenience macro for using (tuile led) for file analysis. With
;; only file argument, a Led handle is returned. With more arguments,
;; the body is treated as analysis code.
;;
;; NOTE: The body may contain let and named-let, but there is no
;; special treatment of lambda, for example.
;;
;; Example:
;;
;;     (define ed (led:read "README.md"))
;;     OR
;;     (led:read "README.md"
;;               (let ((line (get)))
;;                  (pr line)))
;;
(define-syntax read
  (lambda (x)
    (let ((stx (syntax->datum x)))
      (syntax-case x ()
        ((_ filename) #'(-read filename))
        ((_ filename body ...)
         (with-syntax ((state (datum->syntax x 'state)))
           #`(let ((state (-read filename)))
               #,@(datum->syntax
                   x
                   (-expand-datums (cons (car stx) 'read)
                                   (syntax->datum (syntax state))
                                   (syntax->datum (syntax (body ...))))))))))))


;; Convenience macro for using (tuile led) for file editing. With only
;; file argument, a Led handle is returned. With more arguments, the
;; body is treated as editing code and file is saved automatically, as
;; last step.
;;
;; NOTE: The body may contain let and named-let, but there is no
;; special treatment of lambda, for example.
;;
;; Example:
;;
;;     (define ed (led:edit "README.md"))
;;     OR
;;     (led:edit "README.md"
;;               (goto 2)
;;               (set "New line number 2."))
;;
(define-syntax edit
  (lambda (x)
    (let ((stx (syntax->datum x)))
      (syntax-case x ()
        ((_ filename) #'(-edit filename))
        ((_ filename body ...)
         (with-syntax ((state (datum->syntax x 'state)))
           #`(let ((state (-edit filename)))
               #,@(datum->syntax
                   x
                   (-expand-datums (cons (car stx) 'edit)
                                   (syntax->datum (syntax state))
                                   (append
                                    (syntax->datum (syntax (body ...)))
                                    (list '(save))))))))))))



(define (save state)
  (when (state-filename state)
    (save-as state (state-filename state))))

(define (save-as state filename)
  (call-with-output-file filename
    (lambda (port)
      (let ((count (state-used state)))
        (for-n! (i count)
                (display (vector-ref (state-text state) i) port)
                (newline port))))))

(define (size state)
  (state-used state))

(define (filename state)
  (state-filename state))

(define (at state)
  (state-at state))

(define (text state)
  (vector-copy (state-text state) 0 (state-used state)))

(define (goto state line)
  (if (and (>= line 0)
           (< line (state-used state)))
      (begin
        (state-at-set! state line)
        #t)
      #f))

(define (step state . opt-step)
  (goto state (+ (state-at state) (-opt-arg opt-step 1))))

(define (first state)
  (goto state 0))

(define (last state)
  (goto state (1- (size state))))

(define (mark state)
  (state-mark-set! state (state-at state)))

(define (back state)
  (awhen (state-mark state)
         (goto state it)))

;; Search for string,regexp,proc 'forward or 'backward.
(define (search state type text . opt-dir)
  (-search state (state-at state) type text (-opt-arg opt-dir 'forward)))

;; Find for string,regexp,proc 'forward (default) or 'backward and
;; land there.
(define (find state type text . opt-dir)
  (-find state (state-at state) type text (-opt-arg opt-dir 'forward)))


(define (get state . opt-at)
  (vector-ref (state-text state) (-opt-at opt-at state)))

(define (set state text . opt-at)
  (vector-set! (state-text state) (-opt-at opt-at state) text))

;; Add text and move step forward.
(define (add state text)
  (let ((add-len (-insert-at state text (state-at state))))
    (step state add-len)))

(define (insert state text . opt-at)
  (-insert-at state text (-opt-at opt-at state)))

(define (append state text)
  (-insert-at state text (size state)))

(define (include state filename . opt-at)
  (-insert-at state (-read-file filename) (-opt-at opt-at state)))

;; Remove current line or number of lines.
;;    (remove s)            ; Remove current line and step 1.
;;    (remove s 10)         ; Remove 10 lines at current line.
;;    (remove s 10 20)      ; Remove lines 10-20.
(define (remove state . opt-args)
  (cond
   ((null? opt-args) (-remove-range-at state 1 (state-at state)))
   ((= (length opt-args) 1) (-remove-range-at state (car opt-args) (state-at state)))
   (else
    (let* ((a1 (first opt-args))
           (a2 (second opt-args)))
      (-remove-range-at state (1+ (- a2 a1)) a1)))))


(define (replace state from to . opt-at)
  (-update-at state
              (lambda (line)
                (string-replace-substring line
                                          from
                                          to))
              (-opt-at opt-at state)))


(define (replace-re state from to . opt-at)
  (-update-at state
              (lambda (line)
                (re-gsub from line to))
              (-opt-at opt-at state)))


;; Proc should return non-false, if the update is to be stored.
(define (update state proc . opt-at)
  (-update-at state proc (-opt-at opt-at state)))


(define (clear state)
  (state-used-set! state 0)
  (state-at-set! state 0))
