;; File name and path queries.
;;
;; Concepts:
;;
;; fs: "file-string", i.e. the normal file path specifier string used
;;     in POSIX.
;;
;; fn: proper or improper list of path parts in reverse order,
;;     i.e. the filename is the first entry in this list.
;;
;; filename: filename without path.
;;
;; basename: filename without suffix.
;;
;; extname: filename extension.
;;
;; fullext: complete filename extension, i.e. longer than extname if
;;          there are multiple suffices.
;;
;; dir: absolute directory of file, also for relative paths.
;;
;; reldir: relative directory of file, if any.
;;
;; reldir-dotted: relative directory of file with "." or ".." prefix.
;;
;; dir: resolved directory of file, i.e. for absolute file path the
;;      dir itself and for relative file path the extended dir.
;;
;; path: dir with filename included.

(define-module (tuile fn)
  #:use-module ((srfi srfi-1) #:select (first second fold take drop take-right drop-right))
  #:export (
            fs-filename
            fs-basename
            fs-extname
            fs-fullext
            fs-dirname

            fs-is-abs?
            fs-is-rel?
            fs-dir
            fs-path
            fs-reldir
            fs-reldir-dotted
            fs-relpath
            fs-relpath-dotted

            fs-mkdir-p
            fs-mkpath-p

            fn-is-abs?
            fn-is-rel?
            fn-body
            fn-parts
            fn-file
            fn-dir
            fn->abs
            fs->fn
            fn->fs
            ))


;; ------------------------------------------------------------
;; File:

;; Return filename of file-string.
(define (fs-filename fs)
  (fn-file (fs->fn fs)))


;; Return basename of file-string.
(define (fs-basename fs)
  (car (string-split (fs-filename fs) #\.)))


;; Return extension of file-string.
(define (fs-extname fs)
  (let ((split (string-split (fs-filename fs) #\.)))
    (if (> (length split) 1)
        (car (last-pair split))
        #f)))


;; Return "full" extension of file-string.
(define (fs-fullext fs)
  (string-join (cdr (string-split (fs-filename fs) #\.)) "."))


;; Return "unresolved" directory name.
(define (fs-dirname fs)
  (fn->fs (fn-dir (fs->fn fs))))


;; ------------------------------------------------------------
;; Directory:

;; fs is absolute path?
(define (fs-is-abs? fs)
  (and (> (string-length fs) 0)
       (char=? (string-ref fs 0) #\/)))


;; fs is relative path?
(define (fs-is-rel? fs)
  (not (fs-is-abs? fs)))


;; Return resolved directory of file-string.
(define (fs-dir fs)
  (fn->fs (fn-dir (fn->abs (fs->fn fs)))))


;; Return resolved path of file-string.
(define (fs-path fs)
  (fn->fs (fn->abs (fs->fn fs))))


;; Return relative directory of file-string.
(define (fs-reldir fs)
  (fn->fs (fn-dir (fs->fn fs))))


;; Return relative directory of file-string with dot prefix.
(define (fs-reldir-dotted fs)
  (fs-relpath-dotted (fn->fs (fn-dir (fs->fn fs)))))


;; Return relative directory of file-string with dot prefix.
(define (fs-relpath-dotted fs)
  (if (fs-is-abs? fs)
      fs
      (cond
       ((= (string-length fs) 0) ".")
       ((and (>= (string-length fs) 2)
             (string=? ".." (substring fs 0 2)))
        fs)
       ((and (>= (string-length fs) 2)
             (string=? "./" (substring fs 0 2)))
        fs)
       (else
        (string-append "./" fs)))))


;; Make directory and all parents, if needed.
(define (fs-mkdir-p fs)
  (letrec ((is-dir? (lambda (path)
                      (and (file-exists? path)
                           (file-is-directory? path))))
           (create-if-missing (lambda (path)
                                (when (not (is-dir? path))
                                  (mkdir path))))
           (mkdir-p (lambda (path segments)
                      (if (null? segments)
                          (create-if-missing path)
                          (begin
                            (create-if-missing path)
                            (mkdir-p (string-append path "/" (car segments))
                                     (cdr segments))))))
           (segments (string-split (if (and (> (string-length fs) 1)
                                            (string=? "./" (substring fs 0 2)))
                                       (substring fs 2)
                                       fs)
                                   #\/)))
    (mkdir-p (car segments) (cdr segments))))


;; Make directory and all parents, if needed.
(define (fs-mkpath-p fs)
  (fs-mkdir-p (fs-dir fs)))


;; ------------------------------------------------------------
;; fn:

;; fn is absolute path?
(define (fn-is-abs? fn)
  (car fn))


;; fn is relative path?
(define (fn-is-rel? fn)
  (not (car fn)))

;; Return fn path portion, i.e. drop the absolute/relative info.
(define fn-body cdr)

;; Return fn path portion (in order), i.e. drop the absolute/relative info.
(define (fn-parts fn) (reverse (fn-body fn)))

;; Return filename part of fn.
(define (fn-file fn) (cadr fn))

;; Return directory part of fn.
(define (fn-dir fn) (cons (car fn) (cddr fn)))

;; Return fn resolved, i.e. fn as with absolute path.
(define (fn->abs fn)
  (if (fn-is-abs? fn)
      fn
      (let loop ((tail (reverse (fn-body fn)))
                 (base (fn-body (fs->fn (getcwd)))))
        (cond
         ((string=? (car tail) "..") (loop (cdr tail) (cdr base)))
         ((string=? (car tail) ".") (loop (cdr tail) base))
         (else (cons #t (append (reverse tail) base)))))))


;; Return proper/clean list of fn in order.
#;
(define (fn->list fn)
  (let join ((ret '())
             (tail fn))
    (if (pair? tail)
        (join (cons (car tail) ret) (cdr tail))
        ret)))


;; File-string to fn.
(define (fs->fn fs)

  (define (->part part) (list->string (reverse part)))

  (let* ((chars (string->list fs))
         (absolute (char=? (car chars) #\/)))
    (let loop ((chars (if absolute (cdr chars) chars))
               (part '())
               (ret '()))
      (if (pair? chars)
          (let ((ch (car chars)))
            (cond

             ;; Dir separator.
             ((char=? ch #\/)
              (if (pair? part)
                  (loop (cdr chars)
                        '()
                        (cons (->part part) ret))
                  (loop (cdr chars)
                        '()
                        ret)))

             ;; Relative to current.
             ((and (null? part)
                   (char=? ch #\.))
              (if (char=? (cadr chars) #\.)
                  ;; Upwards.
                  (loop (cddr chars)
                        '()
                        (cons ".." ret))
                  (loop (cdr chars)
                        '()
                        ret)))

             (else (loop (cdr chars)
                         (cons ch part)
                         ret))))
          (cons absolute
                (if (pair? part)
                          (cons (->part part) ret)
                          ret))))))

#;
(define (fs->fn fs)

  ;; (->fn ("foo" "bar" "dii.txt") '())
  ;;   ->
  ;; '("dii.txt" "bar" "foo")
  (define (->fn lst ret)
    (if (pair? lst)
        (->fn (cdr lst) (cons (car lst) ret))
        ret))

  (let ((parts (string-split fs #\/)))
    (cond

     ((= 1 (length parts))
      (cons (car parts) #f))

     ((string=? "" (car parts))
      (->fn (cdr parts) '()))

     ((string=? ".." (car parts))
      (->fn parts #f))

     ((string=? "." (car parts))
      (->fn (cdr parts) #f))

     (else
      (->fn parts #f)))))


;; fn to file-string.
(define (fn->fs fn)
  (if (fn-is-abs? fn)
      (string-append "/" (string-join (reverse (fn-body fn)) "/"))
      (string-join (reverse (fn-body fn)) "/")))



;; File-language processor.
;;
;;     (fl "/foo/bar/jii.haa" "df") -> "/foo/bar/jii.haa"
;;     (fl "/foo/bar/jii.haa" "db") -> "/foo/bar/jii"
;;     (fl "/foo/bar/jii.haa" ("da" "my-file.txt")) -> "/foo/bar/my-file.txt"
;;
;; <     - begin
;; >     - end
;; f     - filename
;; b     - basename
;; e     - extname
;; d     - dir (absolute)
;; r     - reldir
;; i     - index
;; s     - slice
;; [0-9] - index (from end)
;; [0-9] - range (from end or begin)
;; a     - argument
;; h     - home path
;;

(define (fl fs . expr-list)

  ;; Parse expr to list of commands.
  ;;
  ;;     ( '(ref-begin . 1) 'filename )
  ;;
  (define (parse expr)

    (define (->digit ch)
      (- (char->integer ch) (char->integer #\0)))

    (define (is-digit? ch)
      (or (char-numeric? ch)
          (char=? ch #\.)))

    (define (parse-ref chars)
      (let loop ((chars chars)
                 (digits '()))
        (if (and (pair? chars)
                 (char-numeric? (car chars)))
            (loop (cdr chars)
                  (cons (car chars) digits))
            (cons chars (string->number (list->string (reverse digits)))))))

    (define (parse-number chars)

      ;; Return: (<chars> . <number>)
      (define (parse-short-ref chars)
        (cons (cdr chars)
              (->digit (car chars))))

      ;; Return: (<chars> . <number>)
      (define (parse-long-ref chars)

        (define (->number digits)
          (fold (lambda (i s) (+ i (* 10 s))) 0 digits))

        (let loop ((chars (cdr chars))
                   (digits '()))
          (if (and (pair? chars)
                   (char-numeric? (car chars)))
              (loop (cdr chars)
                    (cons (->digit (car chars)) digits))
              (->number (reverse digits)))))

      (if (char-numeric? (car chars))
          (parse-short-ref chars)
          (parse-long-ref chars)))

    (define (parse-numbers chars)
      (let loop ((chars chars)
                 (numbers '()))
        (if (and (pair? chars)
                 (is-digit? (car chars)))
            (let ((ret (parse-number chars)))
              (loop (car ret)
                    (cons (cdr ret) numbers)))
            (cons chars
                  (reverse numbers)))))

    (let ((chars (string->list (if (pair? expr)
                                   (car expr)
                                   expr)))
          (args (if (pair? expr)
                    (cdr expr)
                    '())))

      (let loop ((chars chars)
                 (args args)
                 (ret '()))

        (if (pair? chars)

            (if (is-digit? (car chars))

                ;; Digit specifier.
                (let ((res (parse-numbers chars)))
                  (cond
                   ((= (length (cdr res)) 1)
                    ;; Single number reference.
                    (loop (car res)
                          args
                          (cons (cons 'slice (list (cadr res) 0)) ret)))
                   (else
                    ;; Number-pair reference.
                    (loop (car res)
                          args
                          (cons (cons 'slice (cdr res)) ret)))))

                ;; Non-digit specifier.
                (case (car chars)
                  ((#\<) (let ((res (parse-number (cdr chars))))
                           (loop (car res)
                                 args
                                 (cons (cons 'index-begin (cdr res))
                                       ret))))
                  ((#\>) (let ((res (parse-number (cdr chars))))
                           (loop (car res)
                                 args
                                 (cons (cons 'index-end (cdr res))
                                       ret))))
                  ((#\f) (loop (cdr chars) args (cons 'filename ret)))
                  ((#\b) (loop (cdr chars) args (cons 'basename ret)))
                  ((#\e) (loop (cdr chars) args (cons 'extname ret)))
                  ((#\d) (loop (cdr chars) args (cons 'dir ret)))
                  ((#\r) (loop (cdr chars) args (cons 'reldir ret)))
                  ((#\a) (loop (cdr chars) (cdr args) (cons (cons 'argument (car args)) ret)))
                  ((#\h) (loop (cdr chars) args (cons 'homepath ret)))))

            ret))))


  (define (rootify part)
    (string-append "/" part))


  (define (command-slice fn args)

    (define (normalize-index index)
      (cond
       ((< index 0) 0)
       ((> index (length (fn-parts fn))) (length (fn-parts fn)))
       (else index)))

    (let* ((ai (normalize-index (first args)))
           (bi (normalize-index (second args))))

      (if (>= ai bi)

          ;; Slice from end of path.
          (let ((ret (drop-right (take-right (fn-parts fn) ai)
                                 bi)))
            (if (= ai (length (fn-parts fn)))
                (cons (rootify (car ret)) (cdr ret))
                ret))

          ;; Slice from begin of path.
          (let ((ret (take (drop (fn-parts fn) ai)
                           (- bi ai))))
            (if (= ai 0)
                (cons (rootify (car ret)) (cdr ret))
                ret)))))


  (let* ((fn (fs->fn fs))
         (res (map
               (lambda (expr)

                 (let loop ((commands (parse expr))
                            (ret '()))

                   (if (pair? commands)

                       (if (pair? (car commands))

                           ;; Command with argument.
                           (let ((command (caar commands))
                                 (args (cdar commands)))

                             (case command

                               ((slice) (loop (cdr commands)
                                              (append (command-slice fn args)
                                                      ret)))

                               ((index-end) (loop (cdr commands)
                                                  (let* ((parts (fn-parts fn))
                                                         (res (list-ref parts (- (length parts) args 1))))
                                                    (cons (if (= args (1- (length parts)))
                                                              (rootify res)
                                                              res)
                                                          ret))))

                               ((index-begin) (loop (cdr commands)
                                                    (let* ((parts (fn-parts fn))
                                                           (res (list-ref parts args)))
                                                      (cons (if (= args 0)
                                                                (rootify res)
                                                                res)
                                                            ret))))

                               ((argument) (loop (cdr commands)
                                                 (cons args ret)))))

                           ;; Direct command.
                           (case (car commands)

                             ((filename) (loop (cdr commands)
                                               (cons (fn-file fn) ret)))

                             ((basename) (loop (cdr commands)
                                               (cons (fs-basename (fn-file fn)) ret)))

                             ((extname) (loop (cdr commands)
                                              (cons (fs-extname (fn-file fn)) ret)))

                             ((dir) (loop (cdr commands)
                                          (cons (fn->fs (fn-dir (fn->abs fn))) ret)))

                             ((reldir) (loop (cdr commands)
                                             (cons (fn->fs (fn-dir fn)) ret)))

                             ((homepath) (loop (cdr commands)
                                               (cons (getenv "HOME") ret)))
                             ))
                       (string-join ret "/"))))
               expr-list)))

    (if (= (length res) 1)
        (car res)
        res)))

(use-modules (tuile pr))
;;(pr (fl "../foo/bar/hii.txt" "db"))
;;(pr (fl "../foo/bar/hii.txt" '("dba" "txt")))
;;(pr (fl "../foo/bar/hii.txt" '("dba" "foo" "bar")))
;;(pr (fl "../foo/bar/hii.txt" '("dbaa" "foo" "bar")))
;;(pr (fl "../foo/bar/hii.txt" '("hbaa" "foo" "bar")))
;;(pr (fl "/foo/bar/hii.txt" "db"))
(pr (fl "/foo/bar/hii.txt" "4"))
(pr (fl "/foo/bar/hii.txt" "3"))
(pr (fl "/foo/bar/hii.txt" "10"))
(pr (fl "/foo/bar/hii.txt" ">0"))
(pr (fl "/foo/bar/hii.txt" ">1"))
(pr (fl "/foo/bar/hii.txt" "<0"))
(pr (fl "/foo/bar/hii.txt" "<1"))
