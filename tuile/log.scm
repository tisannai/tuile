;; Logger with enabled and disabled logging groups.

(define-module (tuile log)
  #:use-module (tuile pr)
  #:use-module (tuile compatible)
  #:export
  (
   prl
   prl-setup
   prl-enable
   prl-disable
   prl-prefix

   prm
   prm-enable
   prm-disable
   ))


(eval-when (expand load eval)
  (define prl-enabled-logs '()))


(define* (prl-setup #:key (enable '()) (prefix-fn #f))
  (when (pair? enable)
    (apply prl-enable enable))
  (when (pair? prefix-fn)
    (prl-prefix prefix-fn)))


(define (prl-enable . rest)
  (for-each
   (lambda (item)
     (set! prl-enabled-logs
       (assoc-set! prl-enabled-logs item item)))
   rest))


(define (prl-disable . rest)
  (for-each
   (lambda (item)
     (set! prl-enabled-logs
       (assoc-remove! prl-enabled-logs item)))
   rest))


;; Log to group.
;;
;; Groups are symbols, except special groups.
;; #t is special group for logging always.
;; #f is special group for logging never.
(define (prl grp . rest)

  (define (output rest)
    (when prl-prefix-fn
      (let ((out (prl-prefix-fn grp rest)))
        (when (string? out)
          (prp out))))
    (pr rest))

  (cond
   ((symbol? grp)
    (when (assoc-ref prl-enabled-logs grp)
      (output rest)))
   ((and (boolean? grp)
         grp)
    (output rest))))


;; Set prefix processor function. The function is a 2 argument
;; function which gets group and rest.
;;
;; Prefix processing can disable by giving "#f" as prefix function.
;;
;; If prefix processing function returns a string, it will be
;; displayed.
;;
;; Example:
;;
;;   (prl-prefix (lambda (grp rest)
;;                 (ss "* " (symbol->string grp) ": ")))
;;
(define prl-prefix-fn #f)
(define (prl-prefix fn)
  (set! prl-prefix-fn fn))


;; Log to group, as macro.
;;
;; "prm" outputs nothing if group is not enabled.
;;
(define-syntax prm
  (lambda (x)
    (syntax-case x ()
      ((_ grp body ...)
       ;; Test group enable before syntax output.
       ;;
       ;; NOTE: The group is a quoted symbol which is a list at
       ;; expansion time. Hence, we need "cadr" to extract the actual
       ;; group symbol at macro expansion time.
       (when (assoc-ref prl-enabled-logs (cadr (syntax->datum (syntax grp))))
           #'(apply prl (list grp body ...)))))))

;; Macro version of "prl-enable".
(define-syntax prm-enable
  (lambda (x)
    (syntax-case x ()
      ((_ body ...)
       #'(eval-when (expand load eval)
           (prl-enable body ...))))))

;; Macro version of "prl-disable".
(define-syntax prm-disable
  (lambda (x)
    (syntax-case x ()
      ((_ body ...)
       #'(eval-when (expand load eval)
           (prl-disable body ...))))))
