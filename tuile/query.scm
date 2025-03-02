(define-module (tuile query)
  #:use-module (tuile pr)
  #:use-module (tuile utils)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 match)
  #:export
  (
   query-prompt
   query-ask
   query-yn
   query-yn-bool
   ))


;; ------------------------------------------------------------
;; Internal:


(define (query-default-answer default-answer)
  (cond
   ((pair? default-answer) (car default-answer))
   (else #f)))

(define (query-question->yes-no question default-answer)
  (let ((default (query-default-answer default-answer))
        (question (if (string-null? question)
                      ""
                      (si "#{question} "))))
    (cond
     ((not default) (si "#{question}[y/n]? "))
     ((eq? default 'yes) (si "#{question}[Y/n]? "))
     ((eq? default 'no) (si "#{question}[y/N]? ")))))

(define (query-readline prompt default-answer)
  (let ((answer (readline prompt)))
    (cond
     ((or (eof-object? answer)
          (and (string? answer)
               (string=? answer "")))
      (aif (query-default-answer default-answer) it #f))
     (else answer))))

(define (query-answer->yes-no answer)
  (let ((ans (if (string? answer)
                 (string-downcase answer)
                 answer)))
      (match ans
        (#f #f)
        ('yes 'yes)
        ('no 'no)
        ("yes" 'yes)
        ("y" 'yes)
        (else 'no))))

;; Internal:
;; ------------------------------------------------------------


(define (query-prompt question . default-answer)
  (query-readline question default-answer))

(define (query-ask question . default-answer)
  (query-readline (si "#{question}? ") default-answer))

(define (query-yn question . default-answer)
  (query-answer->yes-no
   (query-readline
    (query-question->yes-no question default-answer)
    default-answer)))

(define (query-yn-bool question . default-answer)
  (let ((ret (apply query-yn (cons question default-answer))))
    (case ret
      ((yes) #t)
      (else #f))))


;; (define a #f)
;; (set! a (query-ask "What"))
;; (ppr a)
;; (set! a (query-yn "Do you want beef" 'yes))
;; (ppr a)
;; (set! a (query-yn-bool "Do you want beef" 'no))
;; (ppr a)
