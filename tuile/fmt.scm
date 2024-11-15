(define-module (tuile fmt)
  #:use-module (common base)
  #:use-module ((srfi srfi-1)  #:select (first second third fold drop list-index last drop-right))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((ice-9 pretty-print) #:select (pretty-print))
  #:use-module ((tuile utils) #:select (delete-nth list-split list-specified))
  #:export
  (
   fmt
   fmt-group
   fmt-info
   ))


;; Formatters (with space as default pad):
;;
;; ind - indent, no string arguments      ; (ind 4)
;; lal - left-align                       ; (lal 6 "foo")
;; ral - right-align                      ; (ral 6 "bar")
;; cal - center-align                     ; (cal 7 "dii")
;; laf - left-align-fill                  ; (laf 6 #\* "foo")
;; raf - right-align-fill                 ; (raf 6 #\- "bar")
;; caf - center-align-fill                ; (caf 7 #\+ "dii")
;; cat - concatenate                      ; (cat "foo" "bar")
;; rev - concatenate in reverse           ; (rev "foo" "bar")
;; gap - gap                              ; (gap ("foo" "-") "bar")
;; scm - scheme pretty print              ; (scm #f '(a b))

;; lep - left-pad (same as indent, with arguments in fmt-group)
;; rip - right-pad                        ; (rip 5 "foo")

;; Converters (with zero as default pad):
;;
;; bin - binary code (default pad: 0)     ; (bin (8 " ") 123)
;; oct - octal  code (default pad: 0)     ; (oct 8 123)
;; hex - hex    code (default pad: 0)     ; (hex 8 123)
;; dec - decimal     (default pad: #\ )   ; (dec 8 123)

;; 0123456789012301234567890123012345678901230
;; |      :      |      :      |      :      |
;; foo                 bar                 dii  ; left-align, center-align, right-align
;; foo       bar       dii                      ; gap with 7



;; ------------------------------------------------------------
;; Internal functions:

;; Commands and their arguments counts. lal (and others) accept
;; internally more arguments, so the number is the minimum count for
;; those. Command with 0 count accept any number of arguments.
;;
;;     <command> . <no-of-args>
;;
(define format-commands-spec '((ind . 1)
                               (lal . 2)
                               (ral . 2)
                               (cal . 2)
                               (laf . 2)
                               (raf . 2)
                               (caf . 2)
                               (gap . 0)
                               (lep . 2)
                               (rip . 2)
                               (cat . 0)
                               (rev . 0)
                               (scm . 0)
                               (bin . 2)
                               (oct . 2)
                               (hex . 2)
                               (dec . 2)))

(define format-commands (map car format-commands-spec))


;; Convert arg to string.
(define (->str arg)
  (cond
   ((string? arg)
    arg)
   ((number? arg)
    (object->string arg))
   ((symbol? arg)
    (object->string arg))
   ((list? arg)
    (string-concatenate (map ->str arg)))
   ((boolean? arg)
    (if arg "true" "false"))))


;; Tail recursive full flatten.
(define (flatten . rest)
  (let lp ((stack rest)
           (res '()))
    (cond
     ((null? stack)
      ;; We are done, reverse the result.
      (reverse res))
     ((null? (car stack))
      ;; Eat out empty tails.
      (lp (cdr stack)
          res))
     ((pair? (car stack))
      ;; Convert stack into: (head tail rest-of-stack).
      (lp (cons (caar stack)
                (cons (cdar stack)
                      (cdr stack)))
          res))
     (else
      ;; Add head to result.
      (lp (cdr stack)
          (cons (car stack) res))))))


(define (string-repeat n str)
  (fold string-append "" (make-list n str)))

(define (pad-common pad-fn atom)
  (match atom
    (((num char) str-list ..1) (string-concatenate (map (lambda (str) (pad-fn str num (string-ref char 0))) str-list)))
    (((num char)) (make-string num (string-ref char 0)))
    ((num str-list ..1) (string-concatenate (map (lambda (str) (pad-fn str num #\ )) str-list)))
    ((num) (make-string num #\ ))))

;; Left-pad, indent.
(define (left-pad-entry atom)
  (define (left-pad str count pad-elem)
    (string-append (make-string count pad-elem) (->str str)))
  (pad-common left-pad atom))

;; Right-pad.
(define (right-pad-entry atom)
  (define (right-pad str count pad-elem)
    (string-append (->str str) (make-string count pad-elem)))
  (pad-common right-pad atom))

(define (left-align-or-clip str width pad-elem)
  (if (< (string-length str) width)
      (string-append str
                     (string-repeat (- width
                                       (string-length str))
                                    pad-elem))
      (substring str 0 width)))

(define (right-align-or-clip str width pad-elem)
  (if (< (string-length str) width)
      (string-append (string-repeat (- width
                                       (string-length str))
                                    pad-elem)
                     str)
      (let ((excess (- (string-length str) width)))
        (substring str
                   excess
                   (+ width excess)))))

(define (center-align-or-clip str width pad-elem)
  (if (< (string-length str) width)
      (let* ((len (string-length str))
             (left-pad (quotient (- width len) 2)))
        (string-append (string-repeat left-pad pad-elem)
                       str
                       (string-repeat (- width left-pad len) pad-elem)))
      (substring str 0 width)))

(define (num-to-string radix pad rest)
  (let-values (((width pad num) (match rest
                                  (((width pad) num) (values width pad num))
                                  ((width num) (values width pad num))
                                  ((num) (values (string-length (number->string num radix))
                                                 pad
                                                 num)))))
    (right-align-or-clip (number->string num radix)
                         width
                         pad)))


;; Convert number to binary.
;;
;;     '(bin 123)
;;     '(bin 10 123)
;;     '(bin (10 " ") 123)
;;
(define (format-bin rest)
  (num-to-string 2 "0" rest))

(define (format-hex rest)
  (num-to-string 16 "0" rest))

(define (format-oct rest)
  (num-to-string 8 "0" rest))

(define (format-dec rest)
  (num-to-string 10 " " rest))

;; Return values: str/str-list, width, pad.
(define (handle-align-or-clip-args atom fill)
  (apply values (cons (if (= (length atom) 2)
                          (if (list? (second atom))
                              ;; List argument.
                              (format-atom (second atom))
                              ;; Single argument.
                              (second atom))
                          ;; Many arguments.
                          (map format-atom (drop atom 1)))
                      (if (pair? (first atom))
                          (first atom)
                          (list (first atom) (string fill))))))

(define (call-align fn atom fill)
  (let-values (((str-def width pad) (handle-align-or-clip-args atom fill)))
    (if (list? str-def)
        (map (lambda (str)
               (fn (->str str) width pad))
             str-def)
        (fn (->str str-def) width pad))))

;; Separate fields with sized gap.
;;
;;     (gap 4 "foo" "bar")
;;     (gap (4 "-") "foo "bar")
;;
(define (format-gap rest)
  (let-values (((size char fields) (match rest
                                     (((size char) field ...) (values size char field))
                                     ((size field ...) (values size " " field)))))
    (string-join (flatten (map format-atom (cdr rest)))
                 (string-repeat size char))))

(define (format-cat rest)
  (string-concatenate (map fmt (list-specified rest))))

(define (format-rev rest)
  (string-concatenate (map fmt (reverse (list-specified rest)))))

(define (format-scm rest)
  (with-output-to-string (lambda ()
                           (pretty-print rest))))


(define (fmt-ind atom) (left-pad-entry atom))
(define (fmt-lal atom) (call-align left-align-or-clip atom #\ ))
(define (fmt-ral atom) (call-align right-align-or-clip atom #\ ))
(define (fmt-cal atom) (call-align center-align-or-clip atom #\ ))
(define (fmt-laf atom) (call-align left-align-or-clip (delete-nth atom 1) (second atom)))
(define (fmt-raf atom) (call-align right-align-or-clip (delete-nth atom 1) (second atom)))
(define (fmt-caf atom) (call-align center-align-or-clip (delete-nth atom 1) (second atom)))
(define (fmt-gap atom) (format-gap atom))
(define (fmt-lep atom) (left-pad-entry atom))
(define (fmt-rip atom) (right-pad-entry atom))
;; (define (fmt-cat atom) (map fmt atom))
;; (define (fmt-rev atom) (map fmt (reverse atom)))
(define (fmt-cat atom) (format-cat atom))
(define (fmt-rev atom) (format-rev atom))
(define (fmt-scm atom) (format-scm atom))
(define (fmt-bin atom) (format-bin atom))
(define (fmt-hex atom) (format-hex atom))
(define (fmt-oct atom) (format-oct atom))
(define (fmt-dec atom) (format-dec atom))


(define (format-atom atom)

  (cond

   ((list? atom)

    (case (first atom)
      ((ind) (fmt-ind (cdr atom)))
      ((lal) (fmt-lal (cdr atom)))
      ((ral) (fmt-ral (cdr atom)))
      ((cal) (fmt-cal (cdr atom)))
      ((laf) (fmt-laf (cdr atom)))
      ((raf) (fmt-raf (cdr atom)))
      ((caf) (fmt-caf (cdr atom)))
      ((gap) (fmt-gap (cdr atom)))
      ((lep) (fmt-lep (cdr atom)))
      ((rip) (fmt-rip (cdr atom)))
      ((cat) (fmt-cat (cdr atom)))
      ((rev) (fmt-rev (cdr atom)))
      ((scm) (fmt-scm (cdr atom)))
      ((bin) (fmt-bin (cdr atom)))
      ((oct) (fmt-oct (cdr atom)))
      ((hex) (fmt-hex (cdr atom)))
      ((dec) (fmt-dec (cdr atom)))

      (else
       (map fmt atom))))

   (else
    (->str atom))))


;; ------------------------------------------------------------
;; User API:

;; Perform formatting based on format descriptions.
;;
;;     (fmt `(ind 10) `(cal 12 "foobar") `(put "--"))
;;
(define (fmt . args)

  ;; Transform user shorthand to proper format descriptions.
  (define (format-shorthand atoms)

    (define (group-atoms atoms)
      (let lp ((items '())
               (groups '())
               (atoms atoms))
        (cond

         ((null? atoms)
          (drop (reverse (cons (reverse items)
                               groups))
                1))

         ((member (car atoms)
                  format-commands)
          (lp (list (car atoms))
              (cons (reverse items)
                    groups)
              (cdr atoms)))

         (else
          (lp (cons (car atoms)
                    items)
              groups
              (cdr atoms))))))

    (if (and (symbol? (first atoms))
             (member (first atoms)
                     format-commands))
        (group-atoms atoms)
        atoms))

  ;; Process multiples atoms.
  (define (format-atoms atoms)
    (if (pair? atoms)
        (cons (format-atom (car atoms))
              (format-atoms (cdr atoms)))
        '()))

  (string-concatenate (flatten (format-atoms (format-shorthand args)))))


;; Compile format definition and return the compiled function.
;;
;;     '((ind (3 "-")) (lal 6 (cat "*")) (lal 12))
;;
(define (old-fmt-compile format)

  (define index 0)

  ;; Convert format definitions to format function calls (fmt-lal etc.).
  (define (transform expr)
    (let* ((cmd (first expr))
           (argcount (assoc-ref format-commands-spec cmd)))
      (case cmd
        ;; (ind (3 "-"))
        ((ind) `(fmt-ind '(,@(cdr expr))))
        (else (let ((last-arg (last expr)))
                (if (list? last-arg)
                    ;; (lal 6 (cat "*"))
                    (case argcount
                      ((2) `(,(symbol-append 'fmt- cmd) (list ,(second expr)
                                                              ,(transform (third expr)))))
                      (else
                       (let ((parts (list-split expr (1- (length expr)))))
                         `(,(symbol-append 'fmt- cmd) (list ,@(append (cdr (car parts))
                                                                        (list (transform (cadr parts)))))))))
                    ;; (lal 12)
                    (let ((next-index (1+ index))
                          (ret (case argcount
                                 ((2) `(,(symbol-append 'fmt- cmd) (list ,(second expr)
                                                                         (list-ref fn ,index))))
                                 (else `(,(symbol-append 'fmt- cmd) (list ,@(append (cdr expr)
                                                                                    (list `(list-ref fn ,index)))))))))
                      (set! index next-index)
                      ret)))))))

  ((@ (system base compile) compile)
   `(lambda (fn)
      (string-concatenate (append (list ,@(map transform format))
                                  (list-tail fn ,index))))
   #:env (current-module)))


(define (fmt-compile format)

  (define index 0)

  ;; Convert format definitions to format function calls (fmt-lal etc.).
  (define (transform format)
    (let* ((cmd (first format))
           (argcount (assoc-ref format-commands-spec cmd)))
      (case cmd
        ;; (ind (3 "-"))
        ((ind) `(fmt-ind '(,@(cdr format))))
        (else (case argcount
                ((2) (case (length format)
                       ((3)
                        ;; (lal 3 (lep 3 ...)
                        `(,(symbol-append 'fmt- cmd) (list ,(second format)
                                                           ,(transform (third format)))))
                       (else
                        ;; (lal 3)
                        (let ((next-index (1+ index))
                              (ret `(,(symbol-append 'fmt- cmd) (list ,(second format)
                                                                      (list-ref fn ,index)))))
                          (set! index next-index)
                          ret))))
                (else (let ((last-arg (last format)))
                        (if (list? last-arg)
                            ;; (cat "*" (lep 3 ...))
                            (let ((parts (list-split format (1- (length format)))))
                              `(,(symbol-append 'fmt- cmd) (list ,@(append (cdr (car parts))
                                                                           (list (transform (cadr parts)))))))
                            ;; (cat "*") OR (cat)
                            (let ((next-index (1+ index))
                                  (ret `(,(symbol-append 'fmt- cmd) (list ,@(append (cdr format)
                                                                                    (list `(list-ref fn ,index)))))))
                              (set! index next-index)
                              ret)))))))))

  ((@ (system base compile) compile)
   `(lambda (fn)
      (string-concatenate (append (list ,@(map transform format))
                                  (list-tail fn ,index))))
   #:env (current-module)
   #:optimization-level 0))


;; The compiled version of fmt-group. The format is compiled to Guile
;; bytecode and the resulting (compiled) function is applied to each
;; line in the group.
;;
;; NOTE: The compilation takes significant amount of time and the
;; break even is somewhere around 10000 lines, i.e. use this only when
;; absolutely necesssary.
;;
(define (fmt-group-compiled format lines)
  (let ((cmd (fmt-compile format)))
    (let lp ((lines lines)
             (ret '()))
      (if (pair? lines)
          ;;     (fmt '(lal 6 "#") '(lal 12 "foo") "First dummy name.")
          (lp (cdr lines)
              (cons (cmd (car lines)) ret))
          (reverse ret)))))


;; Format multiple lines with the given "format".
;;
;; "format" includes a formatting rule for each column, and the
;; remaining line columns are output as is.
;;
;; Example:
;;
;;     (fmt-group '((lal 6) (lal 12))
;;                 '(("#" "foo" "First dummy name.")
;;                   ("#" "bar" "Second dummy name.")))
;;
(define (fmt-group format lines)

  (define (wrap format expr)
    (let* ((cmd (first format))
           (argcount (assoc-ref format-commands-spec cmd)))
      (case argcount
        ((2) (case (length format)
               ((3)
                ;; (lal 3 (lep 3 ...)
                (list (first format) (second format) (wrap (third format) expr)))
               (else
                ;; (lal 3)
                (list (first format) (second format) expr))))
        (else (let ((last-arg (last format)))
                (if (list? last-arg)
                    ;; (cat "*" (lep 3 ...))
                    (append (drop-right format 1) (list (wrap last-arg expr)))
                    ;; (cat "*") OR (cat)
                    (append format (list expr))))))))

  ;; Place argument to the format description.
  (define (old-wrap host expr)
    (let ((sub-host-index (list-index list? host)))
      (if sub-host-index
          (append (list-head host sub-host-index)
                  (list (wrap (list-ref host sub-host-index) expr))
                  (list-tail host (1+ sub-host-index)))
          (append host (list expr)))))

  (let lp ((lines lines)
           (ret '()))
    (if (pair? lines)
        ;;     (fmt '(lal 6 "#") '(lal 12 "foo") "First dummy name.")
        (lp (cdr lines)
            (cons (apply fmt (let lp2 ((rest format)
                                       (cols (car lines))
                                       (ret '()))
                               (if (pair? rest)
                                   (if (eq? (caar rest) 'ind)
                                       (lp2 (cdr rest)
                                            cols
                                            (append ret
                                                    (list (append (car rest)))))
                                       (lp2 (cdr rest)
                                            (cdr cols)
                                            (append ret
                                                    (list (wrap (car rest) (->str (car cols)))))))
                                   (append ret cols))))
                  ret))
        (reverse ret))))


;; Return field min/max/count info for args.
(define (fmt-info . args)

  (define (gen-fmt-info args)

    (define (find-length-prop cmp args)
      (let find ((prop (string-length (car args)))
                 (tail (cdr args)))
        (if (pair? tail)
            (let ((len (string-length (car tail))))
              (if (cmp len prop)
                  (find len  (cdr tail))
                  (find prop (cdr tail))))
            prop)))

    (let ((str-args (map ->str args)))
      (list (cons 'count (length args))
            (cons 'max (find-length-prop > str-args))
            (cons 'min (find-length-prop < str-args))
            (cons 'total (fold (lambda (i s)
                                 (+ s (string-length i)))
                               0
                               str-args)))))

  (gen-fmt-info (match args
                  ((args) args)
                  (else args))))


;; (use-modules (tuile pr))
#;
(when #t
  (pr (fmt `(ind 10) `(cal 12 "foobar") `(put "--")))
  (pr (fmt `(ind 10) `(caf 12 #\* "foobar") `(put "--")))
  (pr (fmt '(bin (12 "-") 6)))
  (pr (fmt '(gap 4 "foo" "bar")))
  (pr (fmt `(raf 10 #\- "x"))))

;; (use-modules (tuile pr))
;; (pr (fmt `(ral (5 "-") "foo" "**")))
;; (pr (fmt `(ral (5 "-") 123)))
;; (pr (fmt `(ind (5 "*") "dii") "---"))
;; (pr (fmt `(ind 5 "dii") "---"))
;; (pr (fmt `(ind (5 "+") "---" "**")))
;; (pr (fmt `(lep 5 "---" "**")))
;; (pr (fmt `(rip 5 "---" "**")))

;; (pr (fmt `(ind (5 "+"))))
;; (pr (fmt `(ind 5) "**"))
;; (pr (fmt `(ind 5 "---") "**"))
;; (pl (fmt-group '((ind (3 "-")) (lal 6 (cat "*")) (lal 12))
;;                '(("#" "foo" "First dummy name.")
;;                  ("#" "bar" "Second dummy name."))))

;; (pr (fmt-bin '(10 123)))
;; (pr (fmt-bin '(123)))
;; (pr (fmt '(bin 10 123)))
;;
;; (pl (fmt-group '((ind (3 "-")) (lal 6 (cat "*")) (lal 12))
;;                '(("#" "foo" "First dummy name.")
;;                  ("#" "bar" "Second dummy name."))))
;;
;; (pr (fmt '(cat "foo" "-" "bar")))
;; (pr (fmt '(rev "foo" "-" "bar")))
;; (pr (fmt '(rev "foo" "-" (lal 10 "bar"))))
;; (pr (fmt "foo" "-" "bar"))

;; (pr (fmt '(lal 5 "foo" "bar")))
;; (pr (fmt '(lal 5 "foo" (cat "foo" "bar"))))
;; (pr (fmt '(lal 5 (cat "foo" "bar"))))
