;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2023
;;;; All rights reserved.


;;; module:
;;
;; File name and path queries.
;;
;; NOTE: We assume clean file-path strings, i.e. no trailing slashes,
;; double slashes, etc. Use fps-clean to clean-up the file-path. Some
;; function work with "broken" path strings.
;;
;;
;; Concepts:
;;
;; fps: file-path as "file-string", i.e. the normal file path specifier string used
;;      in POSIX.
;;
;; fpd: file-path as descriptor: proper or improper list of path parts
;;      in reverse order, i.e. the filename is the first entry in this
;;      list: (list <abs-path?> <file> <dir1> <dir2> ...)
;;
;; fpl: file-path language: file-path manipulation with one character
;;      commands. This allows very compact file-path manipulation.
;;
;;
;; file: filename without path.
;;
;; base: filename without suffix.
;;
;; ext: filename extension (last suffix).
;;
;; fullext: complete filename extension, i.e. longer than ext if
;;          there are multiple suffices.
;;
;; host: directory name of host level.
;;
;; dir: absolute directory of file, also for relative paths.
;;
;; reldir: relative directory of file, if any.
;;
;; reldir-dotted: relative directory of file with "." or ".." prefix.
;;
;; path: absolute dir with filename included.
;;
;;
;; dir: directory in default format.
;;
;; rel: relative directory.
;;
;; abs: absolute directory.
;;
;; dot: dotted relative directory ("./").
;;
;; off: offsetted relative directory ("../").
;;
;; path: absolute path
;;
(define-module (tuile file-path)
  #:use-module ((srfi srfi-1) #:select (first second fold take drop take-right drop-right))
  #:export (
            fps-file
            fps-base
            fps-ext
            fps-fullext
            fps-dir
            fps-type
            fps-clean

            fps-dir?
            fps-hidden?
            fps-abs?
            fps-rel?
            fps-dot?
            fps-off?
;;             fps-dir
            fps->abs
;;             fps-dir
;;             fps-reldir-dotted
;;             fps-relpath
;;             fps-relpath-dotted

            fps-mkdir-p
            fps-mkpath-p
            fps-ls

            fpd-abs?
            fpd-rel?
            fpd-dot?
            fpd-off?
            fpd-root?
            fpd-type
            fpd-body
            fpd-parts
            fpd-file
            fpd-dir
;;             fpd-to-type
            fpd-up
            fpd->abs
            fps->fpd
            fpd->fps

            fpl
            ))


(define (clean-fps fps)
  (if (string=? (string-take-right fps 1)
                "/")
      (string-drop-right fps 1)
      fps))

(define (fps-end fps)
  (let ((len (string-length fps)))
    (if (and (> len 1)
             (char=? (string-ref fps (1- len)) #\/))
        (- len 1)
        len)))


;; ------------------------------------------------------------
;; File:

;; Return filename of file-string.
(define (fps-file fps)
  (fpd-file (fps->fpd fps)))


;; Return basename of file-string.
(define (fps-base fps)
  (if (= (string-length fps) 0)
      #f
      (let ((filename (fps-file fps)))
        (let lp ((idx 1))
          (if (< idx (string-length filename))
              (cond
               ((char=? (string-ref filename idx) #\.) (substring filename 0 idx))
               (else (lp (1+ idx))))
              filename)))))


;; Return extension of file-string.
(define (fps-ext fps)
  (define (return dot end)
    (if (and dot
             (not (= dot 0))
             (not (char=? (string-ref fps (1- dot)) #\/)))
        (substring fps (1+ dot) end)
        #f))
  (if (= (string-length fps) 0)
      #f
      (let ((end (fps-end fps)))
        (let lp ((idx (1- end)))
          (if (> idx 1)
              (cond
               ((char=? (string-ref fps idx) #\.) (return idx end))
               ((char=? (string-ref fps idx) #\/) #f)
               (else (lp (1- idx))))
              #f)))))


;; Return "full" extension of file-string.
(define (fps-fullext fps)
  ;; NOTE: "return" is same as in fps-ext.
  (define (return dot end)
    (if (and dot
             (not (= dot 0))
             (not (char=? (string-ref fps (1- dot)) #\/)))
        (substring fps (1+ dot) end)
        #f))
  (if (= (string-length fps) 0)
      #f
      (let ((end (fps-end fps)))
        (let lp ((idx (1- end))
                 (dot #f))
          (if (> idx 1)
              (cond
               ((char=? (string-ref fps idx) #\.) (lp (1- idx) idx))
               ((char=? (string-ref fps idx) #\/) (return dot end))
               (else (lp (1- idx) dot)))
              #f)))))


;; Return "unresolved" directory name.
(define (fps-dir fps)
  (fpd->fps (fpd-dir (fps->fpd fps))))


;; Return "unresolved" directory name.
(define (fps-host fps . count)
  (let ((n (if (pair? count) (car count) 1)))
    (let ((host-path (fpd-up (fpd->abs (fps->fpd fps)) n)))
      (if host-path
          (if (null? (fpd-body host-path))
              #f
              (car (fpd-body host-path)))
          #f))))


;; Return file-path path type.
(define (fps-type fps)
  (define (prefixed? fps prefix)
    (let ((len (string-length prefix)))
      (if (>= (string-length fps) len)
          (string=? (substring fps 0 len) prefix)
          #f)))
  (if (string-null? fps)
      #f
      (cond
       ((char=? (string-ref fps 0) #\/) 'abs)
       ((not (char=? (string-ref fps 0) #\.)) 'rel)
       ((prefixed? fps "./") 'dot)
       ((prefixed? fps "../") 'off)
       (else 'rel))))


;; Return cleaned file-path string.
(define (fps-clean fps)
  ;; TODO: clean-up code:
  ;; * remove trailing slash.
  ;; * remove double slashes.
  fps)


;; ------------------------------------------------------------
;; Directory:

;; fps is a directory?
(define (fps-dir? fps)
  (file-is-directory? fps))


;; fps is hidden file or directory?
(define (fps-hidden? fps)
  (if (string-null? fps)
      #f
      (char=? (string-ref (fps-file fps) 0) #\.)))


;; fps is absolute path?
(define (fps-abs? fps)
  (eq? (fps-type fps) 'abs))


;; fps is relative path?
(define (fps-rel? fps)
  (eq? (fps-type fps) 'rel))


;; fps is dotted path?
(define (fps-dot? fps)
  (eq? (fps-type fps) 'dot))


;; fps is offset path?
(define (fps-off? fps)
  (eq? (fps-type fps) 'off))


;; ;; Return resolved directory of file-string.
;; (define (fps-dir fps)
;;   (fpd->fps (fpd-dir (fpd->abs (fps->fpd fps)))))


;; Return resolved path of file-string.
(define (fps->abs fps . base)
  (fpd->fps (apply fpd->abs (cons (fps->fpd fps) base))))


;; ;; Return relative directory of file-string.
;; (define (fps-dir fps)
;;   (fpd->fps (fpd-dir (fps->fpd fps))))


;; ;; Return relative directory of file-string with dot prefix.
;; (define (fps-reldir-dotted fps)
;;   (fps-relpath-dotted (fpd->fps (fpd-dir (fps->fpd fps)))))


;; ;; Return relative directory of file-string with dot prefix.
;; (define (fps-relpath-dotted fps)
;;   (if (fps-abs? fps)
;;       fps
;;       (cond
;;        ((= (string-length fps) 0) ".")
;;        ((and (>= (string-length fps) 2)
;;              (string=? ".." (substring fps 0 2)))
;;         fps)
;;        ((and (>= (string-length fps) 2)
;;              (string=? "./" (substring fps 0 2)))
;;         fps)
;;        (else
;;         (string-append "./" fps)))))

;; Make for a directory path, directory and all parents, if needed.
(define (fps-mkdir-p fps)

  (define (advance-dir base new-dir)
    (let ((path (string-append base "/" new-dir)))
      (if (file-exists? path)
          (if (file-is-directory? path)
              path
              #f)
          (begin
            (mkdir path)
            path))))

  (let ((fpd (fps->fpd fps)))
    (let lp ((base (if (fpd-abs? fpd) "" "."))
             (tail (reverse (cdr fpd))))
      (when (pair? tail)
        (let ((ret (advance-dir base (car tail))))
          (if ret
              (lp ret (cdr tail))
              #f))))))


;; Make for a file path, directory and all parents, if needed.
(define (fps-mkpath-p fps)
  (fps-mkdir-p (fps-dir fps)))


(define (fps-ls fps)
  (let ((files ((@ (ice-9 ftw) scandir) fps)))
    (if files
        (list-tail files 2)
        #f)))


;; ------------------------------------------------------------
;; fpd, file-path descriptor:

;; fpd is absolute path?
(define (fpd-abs? fpd)
  (eq? (fpd-type fpd) 'abs))

;; fpd is relative path?
(define (fpd-rel? fpd)
  (eq? (fpd-type fpd) 'rel))

;; fpd is dot path?
(define (fpd-dot? fpd)
  (eq? (fpd-type fpd) 'dot))

;; fpd is offset path?
(define (fpd-off? fpd)
  (eq? (fpd-type fpd) 'off))

;; fpd is root?
(define (fpd-root? fpd)
  (and (eq? (fpd-type fpd) 'abs)
       (null? (fpd-body fpd))))

;; Return fpd type.
(define fpd-type car)

;; Return fpd path portion, i.e. drop the absolute/relative info.
(define fpd-body cdr)

;; Return fpd path portion (in order), i.e. drop the absolute/relative info.
(define (fpd-parts fpd) (reverse (fpd-body fpd)))

;; Return filename part of fpd.
(define (fpd-file fpd)
  (if (pair? (fpd-body fpd))
      (second fpd)
      #f))

;; Return directory part of fpd.
(define (fpd-dir fpd)
  (if (pair? (fpd-body fpd))
      (cons (car fpd) (cddr fpd))
      (cons (car fpd) '())))

;; ;; Change fpd type.
;; (define (fpd-to-type fpd type)
;;   #f)

;; Drop "n" top directory levels.
;;
;; NOTE: If relative path is exhausted, the result is converted
;; automatically to an absolute path.
;;
(define (fpd-up fpd n)
  (let ((body (fpd-body fpd)))
    (if (>= (length body) n)
        (cons (fpd-type fpd) (drop body n))
        (if (fpd-abs? fpd)
            #f
            (fpd-up (fpd->abs fpd) n)))))

;; Return fpd resolved, i.e. fpd as with absolute path.
;;
;; NOTE: Base directory (for relative paths) is taken as "current
;; directory" or it is given as optional argument.
;;
(define (fpd->abs fpd . base)
  (if (fpd-abs? fpd)
      fpd
      (if (null? (fpd-body fpd))
          fpd
          (let lp ((tail (reverse (fpd-body fpd)))
                   (base (fpd-body (fps->fpd (if (pair? base)
                                                 (car base)
                                                 (getcwd))))))
            (cond
             ((string=? (car tail) "..") (lp (cdr tail) (cdr base)))
             ((string=? (car tail) ".") (lp (cdr tail) base))
             (else (cons 'abs (append (reverse tail) base))))))))

;; File-path-string to file-path-descriptor (fpd).
(define (fps->fpd fps)

  (define (->part part) (list->string (reverse part)))

  (let ((chars (string->list fps))
        (type (fps-type fps)))

    (let lp ((chars (if (eq? type 'abs) (cdr chars) chars))
               (part '())
               (ret '()))

      (if (pair? chars)

          (let ((ch (car chars)))

            (cond

             ;; Dir separator.
             ((char=? ch #\/)
              (if (pair? part)
                  (lp (cdr chars)
                        '()
                        (cons (->part part) ret))
                  (lp (cdr chars)
                        '()
                        ret)))

             ;; Relative to current.
             ((and (null? part)
                   (char=? ch #\.)
                   (cdr chars))
              (cond
               ((char=? (second chars) #\.)
                ;; Offset dir, upwards.
                (lp (cddr chars)
                      '()
                      (cons ".." ret)))
               ((char=? (second chars) #\/)
                ;; Dotted dir.
                (lp (cdr chars)
                     '()
                     ret))
               (else
                ;; Hidden name.
                (lp (cdr chars)
                      (cons ch part)
                      ret))))

             ;; Name part.
             (else (lp (cdr chars)
                         (cons ch part)
                         ret))))

          (cons type
                (if (pair? part)
                    (cons (->part part) ret)
                    ret))))))

;; fpd to file-string.
(define (fpd->fps fpd)
  (case (fpd-type fpd)
    ((abs) (string-append "/" (string-join (reverse (fpd-body fpd)) "/")))
    ((rel) (string-join (reverse (fpd-body fpd)) "/"))
    ((dot) (string-append "./" (string-join (reverse (fpd-body fpd)) "/")))
    ((off) (string-append "../" (string-join (reverse (fpd-body fpd)) "/")))))



;; File-language processor.
;;
;; Examples:
;;
;;     (fpl "/foo/bar/jii.haa" "df") -> "/foo/bar/jii.haa"
;;     (fpl "/foo/bar/jii.haa" "db") -> "/foo/bar/jii"
;;     (fpl "/foo/bar/jii.haa" ("da" "my-file.txt")) -> "/foo/bar/my-file.txt"
;;
;; One-character commands:
;;
;;     s          - self
;;     f          - filename
;;     b          - basename
;;     e          - extname
;;     d          - dir (relative path)
;;     p          - path (absolute path)
;;     [0-9]      - slice (from end by count)
;;     [0-9][0-9] - slice (between indeces, higher is exclusive)
;;     <          - index begin (follow by number)
;;     >          - index end (follow by number)
;;     i          - argument (insert directory part)
;;     x          - argument (extend filename part)
;;     h          - home path
;;
;; Indeces are plain number (e.g. "3") or a multi-digit number
;; (e.g. ".123").
;;
;; Slice is given from end if first index is bigger than second.
;; Slice is given from beginning if first index is smaller than
;; second.
;;
;; The "a" command accepts arguments, which are listed in the tail
;; part of command.
;;
(define (fpl fps . expr-list)

  ;; Parse expr to list of commands.
  ;;
  ;;     ( '(index-begin . 1) 'filename )
  ;;
  (define (parse expr)

    (define (->digit ch)
      (- (char->integer ch) (char->integer #\0)))

    (define (is-digit? ch)
      (or (char-numeric? ch)
          (char=? ch #\.)))

    #;
    (define (parse-ref chars)           ; ;
    (let lp ((chars chars)              ; ;
    (digits '()))                       ; ;
    (if (and (pair? chars)              ; ;
    (char-numeric? (car chars)))        ; ;
    (lp (cdr chars)                     ; ;
    (cons (car chars) digits))          ; ;
    (cons chars (string->number (list->string (reverse digits)))))))

    ;; Return: (<chars> . <number>)
    (define (parse-number chars)

      ;; Return: (<chars> . <number>)
      (define (parse-short-ref chars)
        (cons (cdr chars)
              (->digit (car chars))))

      ;; Return: (<chars> . <number>)
      (define (parse-long-ref chars)

        (define (->number digits)
          (fold (lambda (i s) (+ i (* 10 s))) 0 digits))

        (let lp ((chars (cdr chars))
                 (digits '()))
          (if (and (pair? chars)
                   (char-numeric? (car chars)))
              (lp (cdr chars)
                  (cons (->digit (car chars)) digits))
              (->number (reverse digits)))))

      (if (char-numeric? (car chars))
          (parse-short-ref chars)
          (parse-long-ref chars)))

    ;; Return: (<chars> . (<numbers>))
    (define (parse-numbers chars)
      (let lp ((chars chars)
               (numbers '()))
        (if (and (pair? chars)
                 (is-digit? (car chars)))
            (let ((ret (parse-number chars)))
              (lp (car ret)
                  (cons (cdr ret) numbers)))
            (cons chars
                  (reverse numbers)))))

    ;; Body: parse
    (let ((chars (string->list (if (pair? expr)
                                   (car expr)
                                   expr)))
          (args (if (pair? expr)
                    (cdr expr)
                    '())))

      (let lp ((chars chars)
               (args args)
               (ret '()))

        (if (pair? chars)

            (if (is-digit? (car chars))

                ;; Digit specifier.
                (let ((res (parse-numbers chars)))
                  (cond
                   ((= (length (cdr res)) 1)
                    ;; Single number reference.
                    (lp (car res)
                        args
                        (cons (cons 'slice (list (cadr res) 0)) ret)))
                   (else
                    ;; Number-pair reference.
                    (lp (car res)
                        args
                        (cons (cons 'slice (cdr res)) ret)))))

                ;; Non-digit specifier.
                (case (car chars)
                  ((#\<) (let ((res (parse-number (cdr chars))))
                           (lp (car res)
                               args
                               (cons (cons 'index-begin (cdr res))
                                     ret))))
                  ((#\>) (let ((res (parse-number (cdr chars))))
                           (lp (car res)
                               args
                               (cons (cons 'index-end (cdr res))
                                     ret))))
                  ((#\s) (lp (cdr chars) args (cons 'self ret)))
                  ((#\f) (lp (cdr chars) args (cons 'filename ret)))
                  ((#\b) (lp (cdr chars) args (cons 'basename ret)))
                  ((#\e) (lp (cdr chars) args (cons 'extname ret)))
                  ((#\d) (lp (cdr chars) args (cons 'dir ret)))
                  ((#\p) (lp (cdr chars) args (cons 'path ret)))
                  ((#\i) (lp (cdr chars) (cdr args) (cons (cons 'dir-argument (car args)) ret)))
                  ((#\x) (lp (cdr chars) (cdr args) (cons (cons 'ext-argument (car args)) ret)))
                  ((#\h) (lp (cdr chars) args (cons 'homepath ret)))))

            (reverse ret)))))


  ;; Make part a root directory.
  (define (rootify part)
    (string-append "/" part))


  ;; Slice command.
  (define (command-slice fpd args)

    (define (normalize-index index)
      (cond
       ((< index 0) 0)
       ((> index (length (fpd-parts fpd))) (length (fpd-parts fpd)))
       (else index)))

    (let* ((ai (normalize-index (first args)))
           (bi (normalize-index (second args))))

      (if (>= ai bi)

          ;; Slice from end of path.
          (let ((ret (drop-right (take-right (fpd-parts fpd) ai)
                                 bi)))
            (if (= ai (length (fpd-parts fpd)))
                (cons (rootify (car ret)) (cdr ret))
                ret))

          ;; Slice from begin of path.
          (let ((ret (take (drop (fpd-parts fpd) ai)
                           (- bi ai))))
            (if (= ai 0)
                (cons (rootify (car ret)) (cdr ret))
                ret)))))


  ;; Body: fpl
  (let* ((fpd (fps->fpd fps))
         (res (map
               (lambda (expr)

                 (let lp ((commands (parse expr))
                          (ret '()))

                   (if (pair? commands)

                       (if (pair? (car commands))

                           ;; Command with argument.
                           ;;     (<command> . <arg-or-args>)
                           (let ((command (caar commands))
                                 (args (cdar commands)))

                             (case command

                               ((slice) (lp (cdr commands)
                                            (append ret (reverse (command-slice (fpd->abs fpd) args)))))

                               ((index-end) (lp (cdr commands)
                                                (let* ((parts (fpd-parts (fpd->abs fpd)))
                                                       (index (- (length parts) args 1))
                                                       (res (if (>= index 0)
                                                                (list-ref parts index)
                                                                "")))
                                                  (cons res ret))))

                               ((index-begin) (lp (cdr commands)
                                                  (let* ((parts (fpd-parts (fpd->abs fpd)))
                                                         (res (if (< args (length parts))
                                                                  (list-ref parts args)
                                                                  "")))
                                                    (cons res ret))))

                               ((dir-argument) (lp (cdr commands)
                                                   (cons args ret)))

                               ((ext-argument)
                                (lp (cdr commands)
                                    (if (pair? ret)
                                        (cons (string-append (car ret) args) (cdr ret))
                                        (cons args ret))))))

                           ;; Direct command.
                           ;;     <command>
                           (case (car commands)

                             ((self) (lp (cdr commands)
                                         (cons (fpd->fps fpd) ret)))

                             ((filename) (lp (cdr commands)
                                             (cons (fpd-file fpd) ret)))

                             ((basename) (lp (cdr commands)
                                             (cons (fps-base (fpd-file fpd)) ret)))

                             ((extname) (lp (cdr commands)
                                            (cons (fps-ext (fpd-file fpd)) ret)))

                             ((dir) (lp (cdr commands)
                                        (cons (fpd->fps (fpd-dir fpd)) ret)))

                             ((path) (lp (cdr commands)
                                         (cons (fpd->fps (fpd-dir (fpd->abs fpd))) ret)))

                             ((homepath) (lp (cdr commands)
                                             (cons (getenv "HOME") ret)))
                             ))
                       (string-join (reverse ret) "/"))))
               (if (list? (car expr-list))
                   expr-list
                   (list expr-list)))))

    (if (= (length res) 1)
        (car res)
        res)))


;; (define test-1 #f)
;; (define test-2 #f)

#;
(when test-1
  (let ()

    (use-modules (tuile pr))

    ;; (define p "")
    ;; (define p "/foo/bar/")
    (define p "foo/bar/hii.haa.txt")
    ;; (define p "foo/bar/.hii.haa.txt")
    ;; (define p "./foo/bar/hii.haa.txt")
    ;; (define p "../foo/bar/hii.haa.txt")
    ;; (define p "/foo/bar/hii.haa.txt")
    (pr (fps-file p))
    (pr (fps-base p))
    (pr (fps-ext p))
    (pr (fps-fullext p))
    (pr (fps-dir p))
    (pr (fps-host p))
    ;; (pr (fps-host p 2))
    ;; (pr (fps-host p 3))
    (pr (fps-type p))
    ;; (pr (fps-dir? p))
    (pr "--hidden")
    (pr (fps-hidden? p))
    (pr (fps-abs? p))
    (pr (fps->abs p))
    (pr (fps-dir p))

    (pr "-- fpl")
    (pr (fpl p '("dbx" ".gtkw")))
    (pr (fpl p "3"))
;;     (pr (fpl "/foo/bar/jii.haa" "df"))
;;     (pr (fpl "/foo/bar/jii.haa" "db"))
;;     (pr (fpl "/foo/bar/jii.haa" '("da" "my-file.txt")))
    ))

#;
(when test-2
  (let ()
    (use-modules (tuile pr))
    (define p "foo/bar/hii.haa.txt")
;;      (pr (fpl p "db"))
;;     (pr (fpl p "dbx" ".foo"))
;;     (pr (fpl p '("dbx" ".foo")))
;;     (pr (fpl p "1"))
;;     (pr (fpl p "10"))
;;     (pr (fpl p "02"))
    (pr (fpl p "p"))
    (pr (fpl p "d"))
    (pr (fpl p ">5"))
    (pr (fpl p "<5"))
;;     (pr (fpl p "<0"))
;;     (pr (fpl p "2"))
;;     (pr (fpl p "51"))
;;     (pr (fpl p ">"))
    ))
