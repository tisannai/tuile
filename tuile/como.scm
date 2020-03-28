;; -*-scheme-*-

;; como.scm: Command Line Options library for Scheme (Guile).

;; Parse spec and handle cli arguments (from argv).
;;
;; Spec example:
;;
;;     (use-modules (tuile como))
;;
;;     (como-command "como_trial" "Tero Isannainen" "2017"
;;        '(
;;            [opt-single     file     "-f"    "File name."]
;;            [single         dir      "-d"    "Dir name."]
;;            [multi          seg      "-s"    "Segment name."]
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
  #:use-module ((ice-9 format)  #:select (format))
  #:use-module ((ice-9 control) #:select (call/ec))
  #:use-module ((srfi srfi-9)   #:select (define-record-type))
  #:use-module ((srfi srfi-1)   #:select (find))
  #:export (
            como-command
            como-usage
            como-given?
            como-values
            como-value
            como-apply
            como-command-line
            ))



;; ------------------------------------------------------------
;; Option data storage.


;;
;; Program data.
;;
(define-record-type spec
  (make-spec name author year opts)
  spec?
  (name    spec-name)                   ; Program name.
  (author  spec-author)                 ; Program author.
  (year    spec-year)                   ; Program year.
  (opts    spec-opts)                   ; Program options data [list <opt>].
  )


;;
;; Option data.
;;
;; Collection of all static and dynamic option data.
;;
(define-record-type opt
  (make-opt name type sopt desc given value cli info)
  opt?
  (name    opt-name)                    ; Long option [string].
  (type    opt-type)                    ; Type [symbol].
  (sopt    opt-sopt)                    ; Short option [string or symbol].
  (desc    opt-desc)                    ; Description.
  (given   opt-given?   set-opt-given!) ; Has option been given?
  (value   opt-value    set-opt-value!) ; Option value(s) [list].
  (cli     opt-cli)                     ; Cli formatter  [fn].
  (info    opt-info)                    ; Info formatter [fn].
  )


;; Add to option's value list.
(define (add-opt-value! opt val)
  (if (null? (opt-value opt))
      (set-opt-value! opt (list val))
      (set-opt-value! opt (append (opt-value opt) (list val)))))


;; Apply cli formatter.
(define (apply-opt-cli opt)
  ((opt-cli opt) opt))


;; Apply usage formatter.
(define (apply-opt-info opt)
  ((opt-info opt) opt))



;; ------------------------------------------------------------
;; Option status:

;; Find option.
(define (get-opt-from-opts opts name)
  (cond

   ((null? opts)
    #f)

   ((string=? (opt-name (car opts)) name)
    (car opts))

   (else
    (get-opt-from-opts (cdr opts) name))))


;; Return option by name.
;;
;; Default option name is '().
(define (get-opt como name)
  (if (null? name)
      (find-opt-with como 'default opt-type)
      (get-opt-from-opts (spec-opts como) name)))


;; Check if option is mandatory.
(define (required-opt? opt)
  (case (opt-type opt)
    ((help)       #f)
    ((switch)     #f)
    ((single)     #t)
    ((opt-single) #f)
    ((multi)      #t)
    ((opt-multi)  #f)
    ((any)        #t)
    ((opt-any)    #f)
    ((default)    #f)
    ))


;; Check if option is visible.
(define (visible-opt? opt)
  (case (opt-type opt)
    ((help) #f)
    (else #t)))


;; Check if option has multiple values.
(define (multi-value-opt? opt)
  (case (opt-type opt)
    ((help)       #f)
    ((switch)     #f)
    ((single)     #f)
    ((opt-single) #f)
    ((multi)      #t)
    ((opt-multi)  #t)
    ((any)        #t)
    ((opt-any)    #t)
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



;; Check that all required options have been given, exit if required
;; option is missing.
(define (check-required como)
  (for-each (lambda (opt)
              (when (and (required-opt? opt)
                         (not (opt-given? opt)))
                (parse-error (format #f "Missing required options: \"~a\"" (opt-name opt)))))
            (spec-opts como)))



;; ------------------------------------------------------------
;; Option spec creation.

;; Info table for all option types.
(define opt-make-table
  (list (cons 'help          (list opt-cli-help          opt-info-help))
        (cons 'switch        (list opt-cli-switch        opt-info-switch))
        (cons 'single        (list opt-cli-single        opt-info-single))
        (cons 'opt-single    (list opt-cli-opt-single    opt-info-opt-single))
        (cons 'multi         (list opt-cli-multi         opt-info-multi))
        (cons 'opt-multi     (list opt-cli-opt-multi     opt-info-opt-multi))
        (cons 'any           (list opt-cli-any           opt-info-any))
        (cons 'opt-any       (list opt-cli-opt-any       opt-info-opt-any))
        (cons 'default       (list opt-cli-default       opt-info-default))
        ))


;; Create options.
(define (make-opt-with-table type lopt sopt desc)
  (make-opt lopt type sopt desc
            #f
            '()
            (cadr  (assq type opt-make-table))
            (caddr (assq type opt-make-table))))


;; Build program cli spec.
(define (create-como name author year opts-def)
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
;; Parse options.


;; Parse option values.
;;
;; Return: (cnt . cli)
(define (parse-values! opt cli take)
  (let loop ((rest cli)
             (cnt 0))
    (if (or (null? rest)
            (= cnt take))
        (cons cnt rest)
        (if (equal? #\- (car (string->list (car rest))))
            (cons cnt rest)
            (begin
              (add-opt-value! opt
                              (car rest))
              (loop (cdr rest)
                    (1+ cnt)))))))


;; Parse switch.
(define (parse-switch! opt cli)
  (set-opt-given! opt #t)
  (cdr cli))


;; Parse single.
(define (parse-single! opt cli)
  (set-opt-given! opt #t)
  (set! cli (cdr cli))
  (let ((res (parse-values! opt cli 1)))
    (if (= 1 (car res))
        (cdr res)
        (parse-error (format #f "Wrong number of values for: \"~a\"" (opt-name opt))))))


;; Parse multi.
(define (parse-multi! opt cli)
  (set-opt-given! opt #t)
  (set! cli (cdr cli))
  (let ((res (parse-values! opt cli -1)))
    (if (> (car res) 0)
        (cdr res)
        (parse-error (format #f "Wrong number of values for: \"~a\"" (opt-name opt))))))


;; Parse any.
(define (parse-any! opt cli)
  (set-opt-given! opt #t)
  (set! cli (cdr cli))
  (let ((res (parse-values! opt cli -1)))
    (cdr res)))


;; Como error report.
(define (parse-error msg)
  (display "\nComo error: ")
  (display msg)
  (newline)
  (como-usage)
  (exit))


;; Parse given command line. Update all option descriptors with given
;; flag and values.
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
     (let parse-next ((rest (cdr cli))) ; Skip first.
       (when (pair? rest)
         (let ((opt (find-opt-with-cli como (car rest))))
           (if opt
               (case (opt-type opt)
                 ((help)       (ec #t))
                 ((switch)     (parse-next (parse-switch! opt rest)))
                 ((single)     (parse-next (parse-single! opt rest)))
                 ((opt-single) (parse-next (parse-single! opt rest)))
                 ((multi)      (parse-next (parse-multi!  opt rest)))
                 ((opt-multi)  (parse-next (parse-multi!  opt rest)))
                 ((any)        (parse-next (parse-any!    opt rest)))
                 ((opt-any)    (parse-next (parse-any!    opt rest)))
                 (else '()))
               (let ((default (find-opt-with como 'default opt-type)))
                 (if default
                     (begin
                       (set-opt-given! default #t)
                       (parse-values! default rest -1))
                     (parse-error (string-append "Unknown option: " (car rest)))))))))
     #f)))



;; ------------------------------------------------------------
;; Usage displays.


;; "cli" and "info" formatters for all option types.

(define (opt-cli-help opt) #f)
(define (opt-info-help opt) #f)


(define (opt-cli-switch opt)
  (opt-sopt opt))
(define (opt-info-switch opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-single opt)
  (string-append (opt-sopt opt) " <" (opt-name opt) ">"))
(define (opt-info-single opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-opt-single opt)
  (string-append "[" (opt-sopt opt) " <" (opt-name opt) ">" "]"))
(define (opt-info-opt-single opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-multi opt)
  (string-append (opt-sopt opt) " <" (opt-name opt) ">+"))
(define (opt-info-multi opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-opt-multi opt)
  (string-append "[" (opt-sopt opt) " <" (opt-name opt) ">+" "]"))
(define (opt-info-opt-multi opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-any opt)
  (string-append (opt-sopt opt) " <" (opt-name opt) ">*"))
(define (opt-info-any opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-opt-any opt)
  (string-append "[" (opt-sopt opt) " <" (opt-name opt) ">*" "]"))
(define (opt-info-opt-any opt)
  (format #f "~12,a ~a" (opt-sopt opt) (opt-desc opt)))


(define (opt-cli-default opt)
  (string-append "*default*"))
(define (opt-info-default opt)
  (format #f "~12,a ~a" "*default*" (opt-desc opt)))


;; Create "cli" portion of usage.
(define (usage-list como)
  (string-append (string-join (cons (spec-name como)
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
          (string-append "  "
                         (apply-opt-info opt)
                         "\n"))
        (filter visible-opt?
                (spec-opts como)))))


;; Display all usage info.
(define (usage como)
  (display (string-append "\n  " (usage-list como)
                          "\n"
                          (usage-desc como)
                          (format #f
                                  "\n\n  Copyright (c) ~a by ~a\n\n"
                                  (spec-year como)
                                  (spec-author como)))))



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
  (set! como (create-como name author year opts-def))
  (when (parse-cli! como (cli-content))
    (usage como)
    (exit))
  (check-required como))


;; Display usage info.
(define (como-usage)
  (usage como))


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
        (if (multi-value-opt? opt)
            (opt-value opt)
            (car (opt-value opt)))
        def-val)))
