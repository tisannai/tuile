;; Logger with enabled and disabled logging groups.

(define-module (tuile log)
  #:use-module (tuile pr)
  #:export
  (prl
   prl-enable
   prl-disable
   prl-prefix
   ))


(define prl-enabled-logs '())


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
          (prp out))
        (pr rest))))

  (cond
   ((symbol? grp)
    (when (assoc-ref prl-enabled-logs grp)
      (output rest)))
   ((and (boolean? grp)
         grp)
    (output rest))))


;; Prefix function.
(define prl-prefix-fn #f)


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
(define (prl-prefix fn)
  (set! prl-prefix-fn fn))
