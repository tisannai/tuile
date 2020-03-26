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
  #:use-module (ice-9 format)
  #:export (
            opt-given?
            opt-values
            opt-value
            help
            switch
            single
            opt-single
            multi
            opt-multi
            opt-any
            default
            como
            como-command
            como-usage
            como-opt
            como-given?
            como-values
            como-value
            como-apply
            como-if-given
            ))

;; Call stored function.
(define (call func)
  (eval func (interaction-environment)))

;;
;; Option data.

;; OPT: name type sopt desc given value
(define (opt-name opt)
  (list-ref opt 0))

;; TYPE: hidden switch single opt-single multi opt-multi
(define (opt-type opt)
  (list-ref opt 1))

;; Short opt: "-f"
(define (opt-sopt opt)
  (list-ref opt 2))

;; Description: "File option."
(define (opt-desc opt)
  (list-ref opt 3))

;; Is option given?
(define (opt-given? opt)
  (list-ref opt 4))

;; Set option's given attr.
(define (set-opt-given! opt val)
  (list-set! opt 4 val))

;; Return option value.
(define (opt-values opt)
  (list-ref opt 5))

;; Return first option value.
(define (opt-value opt)
  (car (opt-values opt)))

;; Set option's value attr.
(define (set-opt-value! opt val)
  (list-set! opt 5 val))

;; Add to option's value attr.
(define (add-opt-value! opt val)
  (if (null? (list-ref opt 5))
      (list-set! opt 5 (list val))
      (append! (list-ref opt 5) (list val))))

;; Return cli formatter.
(define (opt-cli opt)
  ((list-ref opt 6) opt))

;; Return info formatter.
(define (opt-info opt)
  ((list-ref opt 7) opt))


;;
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


;; Find option.
(define (get-opt-from-opts opts name)
  (cond
   ((null? opts) #f)
   ((string=? (opt-name (car opts)) name) (car opts))
   (else (get-opt-from-opts (cdr opts) name))))

(define (opt-opts como)
  (cadr (assq 'opts como)))

(define (get-opt como name)
  (if (null? name)
      (find-opt-by opt-type como 'default)
      (get-opt-from-opts (opt-opts como) name)))


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


;;
;; Option spec creation.

;; Help option.
(define (help lopt sopt desc)
  (list lopt 'help sopt desc #f '() opt-cli-help opt-info-help))

;; Switch option.
(define (switch lopt sopt desc)
  (list lopt 'switch sopt desc #f '() opt-cli-switch opt-info-switch))

;; Single option.
(define (single lopt sopt desc)
  (list lopt 'single sopt desc #f '() opt-cli-single opt-info-single))

;; OptSingle option.
(define (opt-single lopt sopt desc)
  (list lopt 'opt-single sopt desc #f '() opt-cli-opt-single opt-info-opt-single))

;; Multi option.
(define (multi lopt sopt desc)
  (list lopt 'multi sopt desc #f '() opt-cli-multi opt-info-multi))

;; OptMulti option.
(define (opt-multi lopt sopt desc)
  (list lopt 'opt-multi sopt desc #f '() opt-cli-opt-multi opt-info-opt-multi))

;; Any option.
(define (any lopt sopt desc)
  (list lopt 'any sopt desc #f '() opt-cli-any opt-info-any))

;; OptAny option.
(define (opt-any lopt sopt desc)
  (list lopt 'opt-any sopt desc #f '() opt-cli-opt-any opt-info-opt-any))

;; Default option.
(define (default lopt sopt desc)
  (list lopt 'default sopt desc #f '() opt-cli-default opt-info-default))




;; Parse command spec and store option descriptor to "como".
(define (prepare-spec name author year spec)
  (let ((como '()))
    (set! como (list
                (list 'name name)
                (list 'author author)
                (list 'year year)))
    ;; Put help as first opt.
    (append! como
             (list
              (list 'opts
                    (append (list (help "--help" "-h" "Help for usage."))
                            (map (lambda (i)
                                   (let ((type  (list-ref i 0))
                                         (lopt  (symbol->string (list-ref i 1)))
                                         (sopt  (if (symbol? (list-ref i 2))
                                                    (symbol->string (list-ref i 2))
                                                    (list-ref i 2)))
                                         (desc  (list-ref i 3)))
                                     (let ((optcmd (list type lopt sopt desc)))
                                       (call optcmd))))
                                 spec)))))
    como))


;; Find option by "by" method.
(define (find-opt-by by como tag)
  (let find ((opts (opt-opts como)))
    (cond
     ((null? opts) #f)
     ((equal? (by (car opts)) tag) (car opts))
     (else (find (cdr opts))))))


;; Find option by cli.
(define (find-opt-by-cli como cli)
  (cond
   ((equal? '(#\- #\-) (list-head (string->list (car cli)) 2))
    ;; Match long opt "--".
    (find-opt-by opt-name como (substring (car cli) 2)))
   ((equal? '(#\-) (list-head (string->list (car cli)) 1))
    ;; Match short opt "-".
    (find-opt-by opt-sopt como (car cli)))
   (else
    #f )))


;; Parse option values.
;;
;; Return: (cnt . cli)
(define (parse-values! opt cli take)
  (let ((cnt 0))
    (let parse ((rest cli))
      (if (or (null? rest) (= cnt take))
          (cons cnt rest)
          (if (equal? #\- (car (string->list (car rest))))
              (cons cnt rest)
              (begin
                (add-opt-value! opt (car rest))
                (set! cnt (1+ cnt))
                (parse (cdr rest))))))))


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
    (if (> (car res) 0 )
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
(define (parse-cli! como cli)
  (let ((do-usage #f))
    (let parse-next ((rest (cdr cli))) ; Skip first.
      (cond
       ((null? rest) do-usage)
       (else
        (let ((opt (find-opt-by-cli como rest)))
          (if opt
              (case (opt-type opt)
                ((help)
                 (set! do-usage #t)
                 (parse-next (parse-switch! opt rest)))
                ((switch)     (parse-next (parse-switch! opt rest)))
                ((single)     (parse-next (parse-single! opt rest)))
                ((opt-single) (parse-next (parse-single! opt rest)))
                ((multi)      (parse-next (parse-multi!  opt rest)))
                ((opt-multi)  (parse-next (parse-multi!  opt rest)))
                ((any)        (parse-next (parse-any!    opt rest)))
                ((opt-any)    (parse-next (parse-any!    opt rest)))
                (else '()))
              (let ((default (find-opt-by opt-type como 'default)))
                (if default
                    (begin
                      (set-opt-given! default #t)
                      (parse-values! default rest -1)
                      do-usage )
                    (parse-error (string-append "Unknown option: " (car rest))))))))))))


;; Create "cli" portion of usage.
(define (usage-list como)
  (let disp ((rest (opt-opts como))
             (parts '()))
    (cond
     ((null? rest) parts)
     (else
      (if (visible-opt? (car rest))
          (if (null? parts)
              (set! parts (list (opt-cli (car rest))))
              (append! parts (list (opt-cli (car rest))))))
      (disp (cdr rest) parts)))))


;; Create "info" portion of usage.
(define (usage-desc como)
  (let disp ((rest (opt-opts como)))
    (cond
     ((null? rest) #f)
     (else
      (if (visible-opt? (car rest))
          (begin
            (display "  ")
            (display (opt-info (car rest)))
            (newline)))
      (disp (cdr rest))))))


;; Display generated usage info.
(define (usage como)
  (display "\n  ")
  (display
   (string-join
    (append (list (cadr (assq 'name como))) (usage-list como)) " "))
  (newline)
  (newline)
  (usage-desc como)
  (display (format #f "\n\n  Copyright (c) ~a by ~a\n\n" (cadr (assq 'year como)) (cadr (assq 'author como)))))


;; Check that all required options have been given.
(define (check-required como)
  (let check ((opts (opt-opts como)))
    (cond
     ((null? opts) #f)
     (else (if (and (required-opt? (car opts))
                    (not (opt-given? (car opts))))
               (parse-error (format #f "Missing required options: \"~a\"" (opt-name (car opts))))
               (check (cdr opts)))))))


;;
;; Como public API:

(define como '())

;; Specify cli, parse cli, and check for required args.
(define (como-command name author year spec)
  (set! como (prepare-spec name author year spec))
  (let ((do-usage (parse-cli! como (command-line))))
    (if do-usage
        (begin
          (usage como)
          (exit))))
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
  (opt-values (get-opt como opt-name)))

;; Return single (first) option value.
(define (como-value opt-name)
  (opt-value (get-opt como opt-name)))

;; Return option value if given, otherwise return the given default
;; value.
(define (como-apply opt-name def-val)
  (let ((opt (get-opt como opt-name)))
    (if (opt-given? opt)
        (if (multi-value-opt? opt)
            (opt-values opt)
            (opt-value opt))
        def-val)))

;;
;; Execute "prog" if opt was given. "prog" takes the option as
;; argument.
;;
;; Example:
;;    (como-if-given "file"
;;       (lambda (opt)
;;          (display (opt-value opt))
;;          (newline)))
(define (como-if-given name prog)
  (let ((opt (como-opt name)))
    (if (opt-given? opt)
        (prog opt))))
