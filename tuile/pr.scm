(define-module (tuile pr)
  #:export
  (pr
   prp
   prd
   ss
   :lj
   :rj
   :ls
   :rs
   :jn
   :sp
   :ms
   :nl
   ))


;; ------------------------------------------------------------
;; Internal functions:

;; Flatten argument list to arbitraty depth.
(define (flat-args . args)
  (if (pair? args)

      (cond
       ((list? (car args))
        ;; (format #t "list?\n")
        (append (apply flat-args (car args)) (apply flat-args (cdr args))))
       ((null? (car args))
        ;; (format #t "null?\n")
        '())
       (else
        ;; (format #t "else\n")
        (cons (car args) (flat-args (cdr args)))))
      
      '()))


;; Convert all args to string type.
;;
;; to-string . args
(define (to-string . args)
  (map
   (lambda (obj)
     (if (string? obj)
         obj
         (object->string obj)))
   args))


;; Macro to flatten args and convert all arguments to strings.
(define-syntax fa
  (syntax-rules ()
    ((_ args)
     (apply to-string (apply flat-args args)))))


;; Char or string argument as char.
(define (ch-or-str-as-ch arg)
  (if (char? arg)
      arg
      (car (string->list arg))))


;; Char or string argument as string.
(define (ch-or-str-as-str arg)
  (if (string? arg)
      arg
      (make-string 1 arg)))



;; ------------------------------------------------------------
;; External functions:


;; String from args.
(define (ss . args)
  (apply string-append (fa args)))


;; Line print from args.
(define (pr . args)
  (display (apply ss args))
  (newline))


;; Print from args.
(define (prp . args)
  (display (apply ss args)))


;; Printer for debugging. Alias to pr.
(define prd pr)


;; Left justify with padding.
;;
;; left-just <width> <pad-str-or-ch> <strings>
(define (:lj width pad . rest)
  (let* ((pad-ch (ch-or-str-as-ch pad))
         (str (string-concatenate (fa rest)))
         (pad-cnt (if (< (string-length str) width)
                      (- width (string-length str))
                      0)))
    (string-append str (make-string pad-cnt pad-ch))))


;; Right justify with padding.
;;
;; right-just <width> <pad-str-or-ch> <strings>
(define (:rj width pad . rest)
  (let* ((pad-ch (ch-or-str-as-ch pad))
         (str (string-concatenate (fa rest)))
         (pad-cnt (if (< (string-length str) width)
                      (- width (string-length str))
                      0)))
    (string-append (make-string pad-cnt pad-ch) str)))

    
;; Left-justify with space.
(define (:ls width . rest)
  (:lj width #\  (car rest)))

    
;; right-justify with space.
(define (:rs width . rest)
  (:rj width #\  rest))


;; Join with given string (or char).
(define (:jn join-str-or-char . rest)
  (string-join (fa rest) (ch-or-str-as-str join-str-or-char)))


;; Join with space.
(define (:sp . args)
  (string-join (fa args) " "))


;; Make string from template (string or char).
(define (:ms width char-or-str)
  (make-string width (ch-or-str-as-ch char-or-str)))


;; Newline string.
(define :nl "\n")
