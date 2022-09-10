;; CON: configuration format (for Scheme), core functions.

(define-module (tuile con-core)
  #:use-module ((ice-9 hash-table) #:select (alist->hash-table))
  #:use-module ((ice-9 textual-ports) #:select (get-string-all))
  #:use-module ((ice-9 eval-string) #:select (eval-string))
  #:use-module ((ice-9 popen) #:select (open-input-pipe close-pipe))
  #:use-module ((srfi srfi-1) #:select (first second drop))
  #:export
  (
   inc

   def
   fun
   sel

   arr
   dic
   num
   str

   mul
   add
   sub
   neg

   len
   ref
   set

   gen
   run
   
   cat
   glu
   div
   rip
   fix

   env
   egg
   ))


;; Load file.
(define (inc file)
  (eval-string (call-with-input-file file
                 (lambda (port)
                   (get-string-all port)))))


;; Variable definition.
(define-syntax def (identifier-syntax define))


;; Lambda definition.
(define-syntax fun (identifier-syntax lambda))


;; Selection.
;;
;;     (sel (foo #t)
;;          (arr "foo" 12.3)
;;          (arr "bar" 10))
;; ->
;;     (if (equal? foo #t)
;;         (arr "foo" 12.3)
;;         (arr "bar" 10))
;;
(define-syntax sel
  (lambda (x)
    (syntax-case x ()
      ((_ (a b) true-expr false-expr)
       #'(if (equal? a b) true-expr false-expr))
      ((_ a true-expr false-expr)
       #'(if a true-expr false-expr)))))


;; Create array (i.e. a list).
(define arr list)


;; Create dictionary.
;;
;;     (dict (1 2)
;;           ("foo" 12.3))
;; ->
;;     (alist->hash-table (list (cons 1     2)
;;                              (cons "foo" 12.3)))
;;
(define-syntax dic
  (lambda (x)
    (let ((stx (syntax->datum x)))
      #`(alist->hash-table
         #,(datum->syntax x
                          (cons 'list
                                (map (lambda (p)
                                       (list 'cons (car p) (cadr p)))
                                     (cdr stx))))))))


;; Convert string to number.
(define num string->number)


;; Convert non-string to string.
(define (str obj)
  (cond
   ((string? obj) obj)
   (else (object->string obj))))


;; Multiplication.
(define mul *)


;; Addition.
(define (add a b)
  (cond
   ((list? a) (if (list? b)
                  (append a b)
                  (append a (list b))))
   (else (+ a b))))


;; Subtraction.
(define sub -)


;; Negation.
(define (neg a) (- a))


;; Lenght of array or dictionary.
(define (len arr-or-dic)
  (cond
   ((list? arr-or-dic) (length arr-or-dic))
   ((hash-table? arr-or-dic) (hash-count (const #t) arr-or-dic))
   (else 0)))


;; Reference dictionary key, array index, or array range.
(define (ref arr-or-dic a . b)
  (if (hash-table? arr-or-dic)
      (hash-ref arr-or-dic a)
      (let* ((arr arr-or-dic)
             (len (length arr))
             (norm-inc (lambda (i) (if (< i len) i (1- len))))
             (norm-exc (lambda (i) (if (<= i len) i len)))
             (list-range (lambda (lst a b) (list-head (list-tail lst a) (- b a)))))
        (if (null? b)
            ;; Single index.
            (list-ref arr (norm-inc a))
            ;; Range indeces.
            (let ((b (car b)))
              (if (<= a b)
                  (let ((a (norm-inc a))
                        (b (norm-exc b)))
                    (list-range arr a b))
                  (let ((a (norm-exc a))
                        (b (norm-inc b)))
                    (list-range arr (- len a) (- len b)))))))))


;; Set dictionary value with key.
(define (set dic a v)
  (hash-set! dic a v)
  dic)


;; Generate indeces.
(define gen iota)


;; Alias to map.
(define-syntax run (identifier-syntax map))


;; Flatten items to a list of items and concatenate as string (with no
;; seperator).
(define (cat . items)
  (define (->string obj)
    (cond
     ((list? obj)
      (string-concatenate (map ->string obj)))
     ((string? obj)
      obj)
     (else (object->string obj))))
  (string-concatenate (map ->string items)))


;; Flatten items to a list of items and join as string with
;; seperator. Separator is inserted only to the first level in the
;; item hierarchy.
(define (glu sep . items)
  (define (->string obj)
    (cond
     ((list? obj)
      (string-concatenate (map ->string obj)))
     ((string? obj)
      obj)
     (else (object->string obj))))
  (string-join (map ->string items) sep))


;; Divide string into parts with pattern.
(define (div str sep)

  (define (looking-at? chars sep-chars)
    (let loop ((a chars)
               (b sep-chars))
      (cond
       ((null? b) #t)
       ((null? a) #f)
       ((char=? (car a) (car b)) (loop (cdr a) (cdr b)))
       (else #f))))

  (let* ((sep-chars (string->list sep))
         (sep-len (length sep-chars)))
    (let loop ((chars (string->list str))
               (part '())
               (ret '()))
      (if (pair? chars)
          (if (looking-at? chars sep-chars)
              (loop (drop chars sep-len)
                    '()
                    (cons (list->string (reverse part)) ret))
              (loop (cdr chars)
                    (cons (car chars) part)
                    ret))
          (reverse (if (pair? part)
                       (cons (list->string (reverse part)) ret)
                       ret))))))


;; Return substring of "str".
(define (rip str a b)
  (let* ((len (string-length str))
         (norm-inc (lambda (i) (if (< i len) i (1- len))))
         (norm-exc (lambda (i) (if (<= i len) i len))))
    (if (<= a b)
        (let ((a (norm-inc a))
              (b (norm-exc b)))
          (substring str a b))
        (let ((a (norm-exc a))
              (b (norm-inc b)))
          (substring str (- len a) (- len b))))))


;; Trim string, if needed.
(define (fix str)
  (string-trim-right str))


;; Return environment variable.
(define env getenv)


;; Execute shell command.
(define (egg cmd)

  ;; Execute shell command and return responses as list.
  ;;
  ;; Return: (status-code stdout stderr)
  ;;
  (define (capture-shell-command cmd)
    (let* ((stdout #f)
           (stderr #f)
           (status #f))
      (define (command cmd)
        (let ((fh (open-input-pipe cmd)))
          (set! stdout (get-string-all fh))
          (set! status (close-pipe fh))))
      (let ((err-pipe (pipe)))
        (with-error-to-port (cdr err-pipe)
          (lambda ()
            (command cmd)))
        (close-port (cdr err-pipe))
        (set! stderr (get-string-all (car err-pipe))))
      (list (status:exit-val status) stdout stderr)))

  (let ((ret (capture-shell-command cmd)))
    (if (= 0 (first ret))
        (second ret)
        #nil)))
