(define-module (tuile edit-using)
;;   #:use-module ((tuile edit) #:prefix #{eu:}#)
;;   #:use-module (tuile edit)
  #:export
  (
   edit-using
   ))

;; This module is for making use of (tuile edit) easier. However, the
;; user must first take (tuile edit) in use as:
;;
;;     ((tuile edit) #:prefix #{eu:}#)
;;
;; since otherwise there will be unbound variables.
;;
;; Example use:
;;
;;     (edit-using "edit-test.txt"
;;                 (pr (get)))


;; Convenience macro for using (tuile edit).
(define-syntax edit-using
  (lambda (x)

    ;; Helper to transform a list of datums.
    (define (expand-datums state datums)

      (define eu-prefix (symbol-prefix-proc '#{eu:}#))

      (define (expand-datum datum)
        (if (pair? datum)
            (case (car datum)
              ((;; read ; no fix
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
               (cons (eu-prefix (car datum))
                     (cons state
                           (map expand-datum (cdr datum)))))
              (else (cons (car datum) (map expand-datum (cdr datum)))))
            datum))
 
      (let lp ((datums datums))
        (if (pair? datums)
            (let ((datum (car datums)))
              (cons (expand-datum datum) (lp (cdr datums))))
            '())))

    (syntax-case x ()
      ((_ filename body ...)
       (with-syntax ((state (datum->syntax x 'state))
                     (eu:edit (datum->syntax x 'eu:edit)))
         #`(let ()
             (use-modules ((tuile edit) #:prefix #{eu:}#))
             (let ((state (eu:edit filename)))
               #,@(datum->syntax x (expand-datums (syntax->datum (syntax state))
                                                  (syntax->datum (syntax (body ...))))))))))))


;; (use-modules (tuile pr))
;; (let ()
;;   (when #t
;;     (edit-using "README.md"
;;                 (pr (get)))))
