(define-module (tuile guma)
  #:use-module (rnrs records inspection)
  #:use-module (rnrs records procedural)
  #:use-module (ice-9 textual-ports)
  #:export
  (guma-expand-string
   guma-expand-file
   ))



;; Format Guma string to expanded output. Use "opt-config" for code to
;; be evaluated before expansion.
(define (guma-expand-string string . opt-config)

  (define (looking-at chars at)
    (let ((at-len (string-length at)))
      (cond
       ((null? chars) #f)
       ((>= (length chars) at-len)
        (string=? (list->string (list-head chars at-len)) at))
       (else #f))))

  (define (expand-macro macro)
    (eval (with-input-from-string macro
            (lambda () (read)))
          (resolve-module '(tuile guma))))

  (when (pair? opt-config)
    (let loop ((config (car opt-config)))
      (when (pair? config)
        (eval (car config)
              (resolve-module '(tuile guma)))
        (loop (cdr config)))))

  (let loop ((input (string->list string))
             (state 'text)
             (macro '())
             (res '()))

    (if (pair? input)
        (case state
          ((text) (cond
                   ((looking-at input "-<")
                    (loop (list-tail input 2)
                          'macro
                          macro
                          res))
                   (else
                    (loop (cdr input)
                          state
                          macro
                          (cons (car input) res)))))
          ((macro) (cond
                    ((looking-at input ">-")
                     (loop (list-tail input 2)
                           'text
                           '()
                           (append (reverse
                                    (string->list
                                     (expand-macro
                                      (list->string
                                       (reverse macro)))))
                                   res)))
                    (else
                     (loop (cdr input)
                           state
                           (cons (car input) macro)
                           res)))))
        (list->string (reverse res)))))


;; Format Guma file to expanded output. Use "opt-config" for code to
;; be evaluated before expansion.
(define (guma-expand-file filename . opt-config)
  (apply guma-expand-string (cons (with-input-from-file filename
                                    (lambda () (get-string-all (current-input-port))))
                                  opt-config)))
