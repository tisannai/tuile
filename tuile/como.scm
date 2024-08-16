;; -*-scheme-*-

;; como.scm: Command Line Options library for Scheme (Guile).
;;
;; See: tuile/README.md for option details.
;;
;; Spec example:
;;
;;     (use-modules (tuile como))
;;
;;     (como-command "como_trial" "Tero Isannainen" "2020"
;;        '(
;;            [opt-single     file     "-f"    "File name."]
;;            [single         dir      "-d"    "Dir name."]
;;            [repeat         seg      "-s"    "Segment name(s)."]
;;            [switch         path     "-p"    "Full path display."]
;;            [default        -         -      "Rest of args."]
;;            ))
;;
;; Option query examples:
;;
;;     (como-usage)
;;     (if (como-given? "file")
;;         (display "Got file option\n")
;;         )
;;
;;     (if (como-given? '())
;;         (display (como-value '())))
;;


(define-module (tuile como)
  #:use-module (common base)
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((srfi srfi-1)   #:select (find first second third last fold))
  #:use-module ((srfi srfi-13)  #:select (string-contains))
;;   #:use-module (tuile compatible)
  #:export ( ;; Como classic:
            como-command
            como-usage
            como-given?
            como-values
            como-value
            como-apply
            como-if-given
            como-error
            como-command-line

            ;; Como actions:
            como-actions
            como-var
            ))



;; ------------------------------------------------------------
;; String utilities:

(define (ss . parts)
  (string-concatenate parts))

(define (ljust width str)
  (if (< (string-length (ss str))
         width)
      (let ((pad (- width (string-length str))))
        (ss str (make-string pad #\ )))
      str))

(define str-match? string-contains)

(define (str-split-with str pat)
  (let ((pat-len (string-length pat)))
    (let loop ((tail str)
               (lst '()))
      (if (> (string-length tail)
             0)
          (let ((pos (string-contains tail pat)))
            (if pos
                (loop (substring tail (+ pos pat-len))
                      (append lst (list (substring tail 0 pos))))
                (append lst (list tail))))
          lst))))

;; String utilities:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Option data storage.


;;
;; Program data.
;;
(define-record-type spec
  (fields name                    ; Program name.
          author                  ; Program author.
          year                    ; Program year.
          opts                    ; Program options data [list <opt>].
          ))


;;
;; Option data.
;;
;; Collection of all static and dynamic option data.
;;
(define-record-type opt
  (fields name                      ; Long option [string].
          type                      ; Type [symbol].
          sopt-rec                  ; Short option [string or symbol].
          desc                      ; Description.
          (mutable given)           ; Has option been given?
          (mutable value)           ; Option value(s) [list].
          cli                       ; Cli formatter  [fn].
          info                      ; Info formatter [fn].
          ))


(define (opt-sopt opt)
  (if (opt-sopt-rec opt)
      (opt-sopt-rec opt)
      ""))

(define (opt-dash-opt opt)
  (cond
   ((opt-sopt-rec opt) (opt-sopt-rec opt))
   (else (ss "--" (opt-name opt)))))


(define opt-given? opt-given)

;; Add to option's value list.
(define (add-opt-value! opt val)
  (if (null? (opt-value opt))
      (opt-value-set! opt (list val))
      (opt-value-set! opt (append (opt-value opt) (list val)))))


;; Apply cli formatter.
(define (apply-opt-cli opt)
  ((opt-cli opt) opt))


;; Apply usage formatter.
(define (apply-opt-info opt)
  ((opt-info opt) opt))



;; ------------------------------------------------------------
;; Option status:


;; Check if option is mandatory.
(define (required-opt? opt)
  (case (opt-type opt)
    ((help)       #f)
    ((switch)     #f)
    ((single)     #t)
    ((opt-single) #f)
    ((repeat)     #t)
    ((opt-repeat) #f)
    ((multi)      #t)
    ((opt-multi)  #f)
    ((any)        #t)
    ((opt-any)    #f)
    ((priority)   #f)
    ((default)    #f)
    ))


;; Check if option is visible.
(define (visible-opt? opt)
  (case (opt-type opt)
    ((help) #f)
    (else #t)))


;; Check if option is exclusive.
(define (exclusive-opt? opt)
  (eq? (opt-type opt) 'priority))


;; Check if option has multiple values.
(define (multi-value-opt? opt)
  (case (opt-type opt)
    ((help)       #f)
    ((switch)     #f)
    ((single)     #f)
    ((opt-single) #f)
    ((repeat)     #t)
    ((opt-repeat) #t)
    ((multi)      #t)
    ((opt-multi)  #t)
    ((any)        #t)
    ((opt-any)    #t)
    ((priority)   #f)
    ((default)    #t)
    ))



;; Find option with method based matching.
(define (find-opt-with como tag method)
  (find (lambda (opt)
          (equal? tag (method opt)))
        (spec-opts como)))


;; Find option with cli entry.
(define (find-opt-with-cli como cli)
  (cond

   ;; Match long opt "--".
   ((string=? "--" (substring cli 0 2))
    (find-opt-with como
                   (substring cli 2)
                   opt-name))

   ;; Match short opt "-".
   ((string=? "-" (substring cli 0 1))
    (find-opt-with como
                   cli
                   opt-sopt))
   (else
    #f)))


;; Get (find) option by name or null for default option.
(define (get-opt como name)
  (cond
   ((or (not name)
        (null? name))
    (find-opt-with como 'default opt-type))
   (else
    (find-opt-with como name opt-name))))


;; Check that all required options have been given, exit if required
;; option is missing.
(define (check-required como)
  (call/cc
   (lambda (cc)
     ;; First check if we have a "priority" option in use.
     (for-each (lambda (opt)
                 (when (and (exclusive-opt? opt)
                            (opt-given? opt))
                   (cc #f)))
               (spec-opts como))
     ;; Check for required options.
     (for-each (lambda (opt)
                 (when (and (required-opt? opt)
                            (not (opt-given? opt)))
                   (parse-error (ss "Missing required options: \"" (opt-name opt) "\""))))
               (spec-opts como)))))



;; ------------------------------------------------------------
;; Parse options.


;; Parse option values.
;;
;; Return: (cnt . cli)
(define (parse-values! opt cli take)
  (let loop ((rest cli)
             (cnt 0))
    (if (or (null? rest)
            (= cnt take)
            (equal? #\- (car (string->list (car rest)))))
        (cons cnt rest)
        (begin
          (add-opt-value! opt (car rest))
          (loop (cdr rest) (1+ cnt))))))


;; Parse switch.
(define (parse-switch! opt cli)
  (opt-given-set! opt #t)
  (cdr cli))


;; Parse single.
(define (parse-single! opt cli)
  (opt-given-set! opt #t)
  (let ((res (parse-values! opt (cdr cli) 1)))
    (if (= 1 (car res))
        (cdr res)
        (parse-error (ss "Wrong number of values for: \"" (opt-name opt) "\"")))))


;; Parse multi.
(define (parse-multi! opt cli)
  (opt-given-set! opt #t)
  (let ((res (parse-values! opt (cdr cli) -1)))
    (if (> (car res) 0)
        (cdr res)
        (parse-error (ss "Wrong number of values for: \"" (opt-name opt) "\"")))))


;; Parse any.
(define (parse-any! opt cli)
  (opt-given-set! opt #t)
  (let ((res (parse-values! opt (cdr cli) -1)))
    (cdr res)))


;; Como error report.
(define (parse-error msg)
  (display "\nComo error: ")
  (display msg)
  (newline)
  (como-usage)
  (exit 1))


;; Parse given command line. Update all option descriptors with given
;; status and values.
;;
;; Process:
;; * Find option by cli.
;; *   Report error if option is not found.
;; * Use option parser.
;; * Collect option info to como.
;;
;; Return: true if we should exit with usage display.
;;
(define (parse-cli! como cli)
  (call/ec
   (lambda (ec)
     (let parse-next ((rest (cdr cli))) ; Skip first, i.e. exe.
       (when (pair? rest)
         (let ((opt (find-opt-with-cli como (car rest))))
           (if opt
               (case (opt-type opt)
                 ((help)       (ec #t))
                 ((switch)     (parse-next (parse-switch! opt rest)))
                 ((single)     (parse-next (parse-single! opt rest)))
                 ((opt-single) (parse-next (parse-single! opt rest)))
                 ((repeat)     (parse-next (parse-single! opt rest)))
                 ((opt-repeat) (parse-next (parse-single! opt rest)))
                 ((multi)      (parse-next (parse-multi!  opt rest)))
                 ((opt-multi)  (parse-next (parse-multi!  opt rest)))
                 ((any)        (parse-next (parse-any!    opt rest)))
                 ((opt-any)    (parse-next (parse-any!    opt rest)))
                 ((priority)   (parse-next (parse-switch! opt rest)))
                 (else '()))
               (let ((default (find-opt-with como 'default opt-type)))
                 (if default
                     (begin
                       (opt-given-set! default #t)
                       (parse-values! default rest -1))
                     (parse-error (ss "Unknown option: " (car rest)))))))))
     #f)))



;; ------------------------------------------------------------
;; Usage displays.

;; Settable usage display procedure.
(define como-usage-proc #f)

;; "cli" and "info" formatters for all option types.

;; Common formatter for most of the options.
(define (opt-info-common opt)
  (ss (ljust 13 (opt-dash-opt opt)) (opt-desc opt)))


(define (opt-cli-help opt) #f)
(define (opt-info-help opt) #f)


(define (opt-cli-switch opt)
  (opt-dash-opt opt))


(define (opt-cli-single opt)
  (ss (opt-dash-opt opt) " <" (opt-name opt) ">"))


(define (opt-cli-opt-single opt)
  (ss "[" (opt-dash-opt opt) " <" (opt-name opt) ">" "]"))


(define (opt-cli-repeat opt)
  (ss (opt-dash-opt opt) " <" (opt-name opt) ">#"))


(define (opt-cli-opt-repeat opt)
  (ss "[" (opt-dash-opt opt) " <" (opt-name opt) ">#" "]"))


(define (opt-cli-multi opt)
  (ss (opt-dash-opt opt) " <" (opt-name opt) ">+"))


(define (opt-cli-opt-multi opt)
  (ss "[" (opt-dash-opt opt) " <" (opt-name opt) ">+" "]"))


(define (opt-cli-any opt)
  (ss (opt-dash-opt opt) " <" (opt-name opt) ">*"))


(define (opt-cli-opt-any opt)
  (ss "[" (opt-dash-opt opt) " <" (opt-name opt) ">*" "]"))


(define (opt-cli-priority opt)
  (ss "[" (opt-dash-opt opt) " !" "]"))


(define (opt-cli-default opt)
  "*default*")
(define (opt-info-default opt)
  (ss (ljust 13 "*default*") (opt-desc opt)))


;; Create "cli" portion of usage.
(define (usage-list como)
  (ss (string-join (cons (spec-name como)
                         (map (lambda (opt)
                                (apply-opt-cli opt))
                              (filter visible-opt?
                                      (spec-opts como))))
                   " ")
      "\n"))


;; Create "info" portion of usage.
(define (usage-desc como)
  (string-concatenate
   (map (lambda (opt)
          (ss "  "
              (apply-opt-info opt)
              "\n"))
        (filter visible-opt?
                (spec-opts como)))))


;; Display all usage info.
(define (usage como)
  (display (ss "\n  " (usage-list como)
               "\n"
               (usage-desc como)
               (ss "\n\n  Copyright (c) "
                   (spec-year como)
                   " by "
                   (spec-author como)
                   "\n\n"))))


;; ------------------------------------------------------------
;; Option spec creation.

;; Build program cli spec.
(define (create-como name author year opts-def)

  ;; Info table for all option types.
  (define opt-make-table
    (list (cons 'help          (list opt-cli-help          opt-info-help))
          (cons 'switch        (list opt-cli-switch        opt-info-common))
          (cons 'single        (list opt-cli-single        opt-info-common))
          (cons 'opt-single    (list opt-cli-opt-single    opt-info-common))
          (cons 'repeat        (list opt-cli-repeat        opt-info-common))
          (cons 'opt-repeat    (list opt-cli-opt-repeat    opt-info-common))
          (cons 'multi         (list opt-cli-multi         opt-info-common))
          (cons 'opt-multi     (list opt-cli-opt-multi     opt-info-common))
          (cons 'any           (list opt-cli-any           opt-info-common))
          (cons 'opt-any       (list opt-cli-opt-any       opt-info-common))
          (cons 'priority      (list opt-cli-priority      opt-info-common))
          (cons 'default       (list opt-cli-default       opt-info-default))
          ))

  ;; Create options.
  (define (make-opt-with-table type lopt sopt desc)
    (make-opt lopt type sopt desc
              #f
              '()
              (cadr  (assq type opt-make-table))
              (caddr (assq type opt-make-table))))

  (let ((-> make-opt-with-table))
    (make-spec name
               author
               year
               (cons (-> 'help "help" "-h" "Help for usage.")
                     (map (lambda (line)
                            (-> (list-ref line 0)
                                (symbol->string (list-ref line 1))
                                (if (symbol? (list-ref line 2))
                                    (symbol->string (list-ref line 2))
                                    (list-ref line 2))
                                (list-ref line 3)))
                          opts-def)))))



;; ------------------------------------------------------------
;; Como public API:

;; Como state.
(define como '())


;; Command line content return.
(define cli-content command-line)


;; Set command line return function (thunk).
(define (como-command-line fn)
  (set! cli-content fn))


;; Specify cli, parse cli, and check for required args.
(define (como-command name author year opts-def)
  (set! como-usage-proc usage)
  (set! como (create-como name author year opts-def))
  (when (parse-cli! como (cli-content))
    (usage como)
    (exit))
  (check-required como))


;; Display usage info.
(define (como-usage)
  (como-usage-proc como))


;; Get option by name (tag).
(define (como-opt opt-name)
  (get-opt como opt-name))


;; Check if option was given.
(define (como-given? opt-name)
  (opt-given? (get-opt como opt-name)))


;; Return all option values.
(define (como-values opt-name)
  (opt-value (get-opt como opt-name)))


;; Return single (first) option value.
(define (como-value opt-name)
  (car (opt-value (get-opt como opt-name))))


;; Return option value if given, otherwise return the given default
;; value.
(define (como-apply opt-name def-val)
  (let ((opt (get-opt como opt-name)))
    (if (opt-given? opt)
        (if (and (eq? 'opt-any (opt-type opt))
                 (null? (opt-value opt)))
            def-val
            (if (multi-value-opt? opt)
                (opt-value opt)
                (car (opt-value opt))))
        def-val)))


;; Execute "prog" if opt was given. "prog" takes the option as
;; argument.
;;
;;    (como-if-given "file"
;;       (lambda (opt)
;;          (display (como-value opt))
;;          (newline)))
;;
(define (como-if-given name prog)
  (let ((opt (como-opt name)))
    (when (opt-given? opt)
      (prog opt))))


(define como-error parse-error)

;; ------------------------------------------------------------
;; Como actions API:

(define como-vars (make-hash-table))

(define como-used-actions '())

(define (como-var name)
  (hash-ref como-vars name))


;;
;; "como-actions" is a direct action command line interface.
;;
;; Example:
;;
;;     shell> como-actions create file=foobar.txt
;;       OR
;;     shell> como-actions create file foobar.txt
;;
;; with definitions:
;;
;;     ...
;;     (define (create)
;;       (pr (ss "touch " (como-var "file"))))
;;     ...
;;     (como-actions "como-actions" "Tero Isannainen" "2020"
;;                   '(
;;                     [action    create    "Create file."]
;;                     [action    delete    "Delete file."]
;;                     [option    file      "File name."       "foobar.txt"]
;;                     [option    dir       "Directory."       "."]
;;                     [default   dir       "Directory."       "."]
;;                     )
;;                   '(revert . create))
;;
;; If option default value is "false" or "true" the option is of switch type.
;;
;; Options (as optional association list):
;;
;;     revert      Action (as symbol) to revert to if none is defined
;;                 (default: #f).
;;
;;     label-gap   Gap between action/option label and description in
;;                 help (default: 2).
;;
;;     setup       Apply lambda before action is executed (default: #f).
;;
;; (use-modules (tuile pr))
(define (como-actions program author year action-list . opts)

  (define (option key . default)
    (if (assoc key opts)
        (assoc-ref opts key)
        (and (pair? default) (car default))))
  (define (opt-revert)     (option 'revert #f))
  (define (opt-label-gap)  (option 'label-gap 2))
  (define (opt-setup)      (option 'setup #f))

  ;; Convert object to string if it is not yet.
  (define (->string obj)
    (if (string? obj)
        obj
        (object->string obj)))

  ;; Is value of switch type?
  (define (is-switch-value? opt)
    (if (string? opt)
        (or (string=? opt "true")
            (string=? opt "false"))
        #f))

  ;; Convert switch-string to switch-value.
  (define (switch-str->value val)
    (cond
     ((string=? val "false") #f)
     ((string=? val "true") #t)
     (else #f)))

  ;; Is option of switch type?
  (define (is-switch-type? var)
    (member var switches))

  ;; Initialize variables to specified value or #f.
  (define (como-vars-init defs)
    (for-each (lambda (def)
                (hash-set! como-vars (->string (second def))
                           (if (> (length def) 3)
                               ;; Has default, use it.
                               (if (is-switch-value? (last def))
                                   (begin
                                     (set! switches (cons (->string (second def)) switches))
                                     (switch-str->value (last def)))
                                   (last def))
                               ;; No default.
                               #f)))
              defs)
    (when default
      (hash-set! como-vars "default" '())))

  ;; Set como-var a value.
  (define (como-var-set! name val)
    (hash-set! como-vars name val))

  ;; Check if cli item is of queried type.
  (define (type-is? type entry)
    (find (lambda (i)
            (and (eq? type (car i))
                 (eq? entry (cadr i))))
          action-list))

  (define actions  '())
  (define options  '())
  (define default  #f)

  (define switches '())

  ;; Display all usage info.
  (define (usage como)
    (let* ((longest-label (apply max (map (lambda (spec) (string-length (->string (second spec))))
                                          (append (list (list '*default* '*default*))
                                                  actions
                                                  options))))
           (width (+ longest-label (opt-label-gap)))
           (f2-expand (lambda (f2)
                        (if (member #\newline (string->list f2))
                            (let ((lines (string-split f2 #\newline)))
                              (string-join
                               (cons (first lines)
                                     (map (lambda (line)
                                            (ss (ljust (+ width 4) "") line))
                                          (cdr lines)))
                               "\n"))
                            f2)))
           (formatters (list (cons 'revert-action (lambda (f1 f2)       (ss "  * " (ljust width f1) (f2-expand f2))))
                             (cons 'normal-action (lambda (f1 f2)       (ss "    " (ljust width f1) (f2-expand f2))))
                             (cons 'option        (lambda (f1 f2 f3 f4) (ss "    " (ljust width f1) (f2-expand f2) "\n" f3 "= \"" f4 "\"")))
                             (cons 'default       (lambda (f1 f2)       (ss "    " (ljust width f1) (f2-expand f2))))))
           (action-lines (map (lambda (def)
                                (let ((formatter (if (and (opt-revert)
                                                          (equal? (second def)
                                                                  (opt-revert)))
                                                     (assoc-ref formatters 'revert-action)
                                                     (assoc-ref formatters 'normal-action))))
                                  (formatter (->string (second def)) (third def))))
                              actions))
           (option-lines (append (map (lambda (def)
                                        ((assoc-ref formatters 'option)
                                         (->string (second def))
                                         (third def)
                                         (make-string (+ 6 (opt-label-gap) longest-label) #\ )
                                         (->string (como-var (->string (second def))))))
                                      options)
                                 (if default
                                     (list ((assoc-ref formatters 'default) "*default*" (third default)))
                                     '()))))
      (display (ss "\n  " program " <actions-and-options>\n\n"
                   (if (pair? action-lines)
                       (ss (string-join action-lines "\n") "\n\n")
                       "")
                   (if (pair? option-lines)
                       (ss (string-join option-lines "\n") "\n\n")
                       "")
                   (ss "\n  Copyright (c) "
                       year
                       " by "
                       author
                       "\n\n")))))

  ;; Set external "como-usage" since "parse-error" depends on it.
  (set! como-usage-proc usage)

  ;; Parse cli types.
  (for-each (lambda (i)
              (case (car i)
                ((action)  (set! actions (append actions (list i))))
                ((option)  (set! options (append options (list i))))
                ((default) (set! default i))
                (else
                 (parse-error (ss "Unknown argument type in \"como-actions\": \"" (->string (car i)) "\"")))))
            action-list)

  ;; Check that at least one action exists.
  (when (= (length actions) 0)
    (parse-error "No actions defined"))

  (como-vars-init options)

  ;; Collect user actions.
  (let ((used-actions '()))

    (let parse-next ((rest (cdr (cli-content))))
      (when (pair? rest)
        (cond

         ;; Help
         ((string=? "help" (car rest))
          (usage #f)
          (exit 1))

         ;; Action
         ((type-is? 'action (string->symbol (car rest)))
          (set! used-actions (append used-actions (list (string->symbol (car rest)))))
          (parse-next (cdr rest)))

         ;; Option
         ((or (type-is? 'option (string->symbol (car rest)))
              (str-match? (car rest) "="))

          (if (str-match? (car rest) "=")

              (let ((parts (str-split-with (car rest) "=")))
                ;; Ensure that option exists.
                (when (not (assoc-ref (map cdr options) (string->symbol (first parts))))
                  (parse-error (ss "Option \"" (first parts) "\" is not declared")))
                (como-var-set! (first parts)
                               (if (is-switch-type? (first parts))
                                   (switch-str->value (second parts))
                                   (second parts)))
                (parse-next (cdr rest)))

              (if (pair? (cdr rest))
                  (let ((var (first rest))
                        (val (second rest)))
                    (como-var-set! var
                                   (if (is-switch-type? var)
                                       (switch-str->value val)
                                       val))
                    (parse-next (cddr rest)))
                  (parse-error (ss "Variable \"" (car rest) "\" is missing value")))))

         ;; Default
         (else
          (if default
              (begin
                (como-var-set! "default" (append (como-var "default") (list (car rest))))
                (parse-next (cdr rest)))
              (parse-error "No default argument defined"))))))

    (when (= (length used-actions) 0)
      (if (opt-revert)
          (set! used-actions (list (opt-revert)))
          (parse-error "No actions given")))

    (for-each (lambda (action)
                (when (opt-setup) ((opt-setup)))
                (primitive-eval (list action)))
              used-actions)))
