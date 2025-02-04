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
;; functions work with "broken" path strings. Also, the resulting path
;; might be an empty string, use fps-final to produce a non-empty
;; path.
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
;; end: complete filename extension, i.e. longer than ext if there are
;;      multiple suffices.
;;
;; host: directory name of host level.
;;
;; dir: absolute directory of file, also for relative paths.
;;
;; reldir: relative directory of file, if any.
;;
;; reldir-dotted: relative directory of file with "." or ".." prefix.
;;
;; path: directory path and filename included, if non-directory.
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
;; full: absolute path
;;
(define-module (tuile file-path)
  #:use-module ((srfi srfi-1) #:select (first second fold take drop take-right drop-right))
  #:use-module ((tuile utils) #:select (dir-glob))
  #:use-module (tuile pr)
  #:export (
            fps-file
            fps-base
            fps-ext
            fps-end
            fps-dir
            fps-host
            fps-type
            fps-off-count
            fps-info
            fps-clean
            fps-final
            fps-join
            fps-sub

            fps-dir?
            fps-just-file?
            fps-hidden?
            fps-abs?
            fps-rel?
            fps-dot?
            fps-off?
            fps->abs
            fps->rel

            fps-mkdir-p
            fps-mkpath-p
            fps-touch
            fps-ls
            fps-ls-path
            fps-glob
            fps-recurse
            fps-recurse-dir-before
            fps-recurse-dir-after
            fps-find-files
            fps-find
            fps-copy
            fps-copy-r

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
            fpd->fps-dir
            fpd-up
            fpd-sub
            fpd->abs
            fpd->rel
            fps->fpd
            fpd->fps

            fpl

            ))


(define (fps-end-index fps)
  (let ((len (string-length fps)))
    (if (and (> len 1)
             (char=? (string-ref fps (1- len)) #\/))
        (- len 1)
        len)))

(define (fps-file-parts fps)
  (define (return ret)
    (let lp ((ends (cdr ret))
             (start (car ret)))
      (if (pair? ends)
          (cons (substring fps start (car ends)) (lp (cdr ends)
                                                     (1+ (car ends))))
          '())))
  (if (= (string-length fps) 0)
      #f
      (let ((end (fps-end-index fps)))
        (let lp ((idx (1- end))
                 (ret (list end)))
          (if (> idx -1)
              (cond
               ((char=? (string-ref fps idx) #\.) (lp (1- idx) (cons idx ret)))
               ((char=? (string-ref fps idx) #\/) (return (cons (1+ idx) ret)))
               (else (lp (1- idx) ret)))
              (return (cons 0 ret)))))))


;; ------------------------------------------------------------
;; fps, file-path string:

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
      (let ((end (fps-end-index fps)))
        (let lp ((idx (1- end)))
          (if (> idx 1)
              (cond
               ((char=? (string-ref fps idx) #\.) (return idx end))
               ((char=? (string-ref fps idx) #\/) #f)
               (else (lp (1- idx))))
              #f)))))


;; Return "full" extension of file-string.
(define (fps-end fps)
  ;; NOTE: "return" is same as in fps-ext.
  (define (return dot end)
    (if (and dot
             (not (= dot 0))
             (not (char=? (string-ref fps (1- dot)) #\/)))
        (substring fps (1+ dot) end)
        #f))
  (if (= (string-length fps) 0)
      #f
      (let ((end (fps-end-index fps)))
        (let lp ((idx (1- end))
                 (dot #f))
          (if (> idx 1)
              (cond
               ((char=? (string-ref fps idx) #\.) (lp (1- idx) idx))
               ((char=? (string-ref fps idx) #\/) (return dot end))
               (else (lp (1- idx) dot)))
              (if dot
                  (return dot end)
                  #f))))))


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
;;
;;     abs    Absolute path starting with "/"
;;     off    Offset path from current starting with ".."
;;     dot    Relative path under current starting with "."
;;     rel    Relative path under current starting with "<name>"
;;
(define (fps-type fps)
  (if (string-null? fps)
      #f
      (cond
       ((char=? (string-ref fps 0) #\/) 'abs)
       ((not (char=? (string-ref fps 0) #\.)) 'rel)
       ((and (>= (string-length fps) 2)
             (char=? (string-ref fps 0) #\.)
             (char=? (string-ref fps 1) #\.)) 'off)
       ((and (>= (string-length fps) 2)
             (char=? (string-ref fps 0) #\.)
             (char=? (string-ref fps 1) #\/)) 'dot)
       (else 'rel))))

;; (dot)
;; (dot)
;; (off "..")
;; (dot "foo")
;; (off "foo" "..")
(define (new-fps-type fps)
  (if (string-null? fps)
      #f
      (cond
       ((char=? (string-ref fps 0) #\/) 'abs)
       ((not (char=? (string-ref fps 0) #\.)) 'rel)
       (else (let (
                   (len (string-length fps))
                   (->i (lambda (ch) (case ch
                                       ((#\.) 0)
                                       ((#\/) 1)
                                       (else 2))))
                   ;;  1   2   3   4  5
                   ;;  .  ..  ./ ../  rel
                   (state #(#(1 0 #f abs) ; 0
                            #(2 3 #f dot) ; 1
                            #(#f 4 #f off) ; 2
                            #(#f #f #f dot) ; 3
                            #(#f #f #f off) ; 4
                            #f)))
               ;; (ppre state)
               (let lp ((i 0)
                        (s 0))
                 (if (and (< i len) (vector-ref state s))
                     (lp (1+ i)
                         (vector-ref (vector-ref state s) (->i (string-ref fps i))))
                     (if (vector-ref state s)
                         (vector-ref (vector-ref state s) 3)
                         'rel))))))))


;; Return the number of offset directories in "off" type fpd.
(define (fps-off-count fps)
  (let lp ((fps fps)
           (count 0))
    (if (not (string-null? fps))
        (cond
         ((string-prefix? "../" fps) (lp (substring fps 3)
                                         (1+ count)))
         (else count))
        count)))


;; Return full file information in an alist.
;;
;;     '((dir-type 'rel|'abs)
;;       (dir (<p0> <p1> ...))
;;       (file <filename>)
;;       (base <basename>)
;;       (ext <extname>)
;;       (end <endname>)
;;       (parts (<base> <e0> <e1> ...)))
;;
(define (fps-info fps)
  (let* ((fpd (fps->fpd fps))
         (dir-type (fpd-type fpd))
         (dir (fpd->fps-dir fpd))
         (abs (fpd->fps-dir (fpd->abs fpd)))
         (file (fpd-file fpd))
         (parts (fps-file-parts fps))
         (base (car parts))
         (ext (car (last-pair parts)))
         (end (string-join (cdr parts) ".")))

    `((dir-type . ,dir-type)
      (dir      . ,dir)
      (abs      . ,abs)
      (file     . ,file)
      (base     . ,base)
      (ext      . ,ext)
      (end      . ,end)
      (parts    . ,parts))))


;; Return cleaned file-path string.
(define (fps-clean fps)
  (if (string-null? fps)
      fps
      (let ((len (1- (string-length fps))))
        (let lp ((i 0)
                 (ret '()))
          (if (< i len)
              (let ((ch (string-ref fps i)))
                (if (char=? ch #\/)
                    (if (not (char=? (string-ref fps (+ i 1)) #\/))
                        (lp (1+ i) (cons ch ret))
                        (lp (1+ i) ret))
                    (lp (1+ i) (cons ch ret))))
              (list->string (reverse (if (char=? (string-ref fps i) #\/)
                                         ret
                                         (cons (string-ref fps i) ret)))))))))


;; Return a non-empty string.
(define (fps-final fps)
  (if (string-null? fps)
      "."
      fps))


;; Join tail to the fps.
(define (fps-join fps tail)
  (let ((tail-type (fps-type tail)))
    (case tail-type
      ((abs) (string-append fps tail))
      ((rel) (string-append fps "/" tail))
      ((off) (let* ((off-count (fps-off-count tail))
                    (fps-offsetted (fpd-up (fps->fpd fps) off-count))
                    (tail-offsetted (drop-right (fps->fpd tail) off-count)))
               (fpd->fps (cons (fpd-type fps-offsetted)
                               (append (fpd-body tail-offsetted)
                                       (fpd-body fps-offsetted))))))
      ((dot) (string-append fps (substring tail 1))))))


;; Replace "from" with "to" in fps.
(define (fps-sub fps from to)
  (fpd->fps (fpd-sub (fps->fpd fps) from to)))


;; ------------------------------------------------------------
;; Directory:

;; fps is a directory?
(define (fps-dir? fps)
  (file-is-directory? fps))


;; fps is a file (with no directory part either)?
(define (fps-just-file? fps)
  (and (not (file-is-directory? fps))
       (fps-rel? fps)))


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


;; Return resolved path of file-string.
(define (fps->abs fps . base)
  (fpd->fps (apply fpd->abs (cons (fps->fpd fps) base))))


;; Return relative path of dot path.
(define (fps->rel fps)
  (fpd->fps (fpd->rel (fps->fpd fps))))


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


;; Create an empty file.
(define (fps-touch fps)
  (fps-mkpath-p fps)
  (mknod fps 'regular #o666 0))


;; Return directory entries as leaf names (no path).
(define (fps-ls fps)
  (let ((files ((@ (ice-9 ftw) scandir) fps)))
    (if files
        (list-tail files 2)
        #f)))


;; Return directory entries with path included.
(define (fps-ls-path fps)
  (map (lambda (file) (string-append fps "/" file)) (fps-ls fps)))


;; Return globbed directory entries as leaf names (no path).
;;
;;     (fps-glob "foo/bar/*.mp3")
;;
(define (fps-glob pattern)
  (let ((fpd (fps->fpd pattern)))
    (dir-glob (fps-final (fpd->fps (fpd-dir fpd))) (fpd-file fpd))))


;; Recurse directory hierarchy and execute "fn" for each file.
(define (fps-recurse fps fn)
  (define (recurse fps fn)
    (if (file-is-directory? fps)
        (let lp ((listing (fps-ls fps)))
          (when (pair? listing)
            (recurse (ss fps "/" (car listing)) fn)
            (lp (cdr listing))))
        (fn fps)))
  (recurse fps fn))


;; Recurse directory hierarchy and execute "dir-fn" for each directory
;; before executing "file-fn" for each file in the directory.
(define (fps-recurse-dir-before fps dir-fn file-fn)
  (define (recurse fps dir-fn file-fn)
    (if (file-is-directory? fps)
        (begin
          (when (not (string=? fps "."))
            (dir-fn fps))
          (let lp ((listing (fps-ls fps)))
            (when (pair? listing)
              (recurse (ss fps "/" (car listing)) dir-fn file-fn)
              (lp (cdr listing)))))
        (file-fn fps)))
  (recurse fps dir-fn file-fn))


;; Recurse directory hierarchy and execute "dir-fn" for each directory
;; after executing "file-fn" for each file in the directory.
(define (fps-recurse-dir-after fps dir-fn file-fn)
  (define (recurse fps dir-fn file-fn)
    (if (file-is-directory? fps)
        (begin
          (let lp ((listing (fps-ls fps)))
            (when (pair? listing)
              (recurse (ss fps "/" (car listing)) dir-fn file-fn)
              (lp (cdr listing))))
          (when (not (string=? fps "."))
            (dir-fn fps)))
        (file-fn fps)))
  (recurse fps dir-fn file-fn))


;; Recursively return all files.
(define (fps-find-files fps)
  (let ((files '()))
    (fps-recurse fps (lambda (file) (set! files (cons file files))))
    (reverse files)))


;; Recursively return all files and directories.
(define (fps-find fps)
  (let* ((files '())
         (register-fn (lambda (file) (set! files (cons file files)))))
    (fps-recurse-dir-before fps register-fn register-fn)
    (reverse files)))


;; Copy "from" as "to" and create directory for "to", if needed.
(define (fps-copy from to)
  (fps-mkpath-p to)
  (copy-file from to))


;; Recursively copy "from" to "to" and create directory for "to", if
;; needed.
(define (fps-copy-r from to)
  (if (file-is-directory? from)
      (let* ((clean-from (fps-clean from))
             (files (fps-find-files clean-from))
             (drop-count (string-length (ss (fps-clean clean-from) "/"))))
        (fps-mkdir-p to)
        (for-each (lambda (file)
                    (let* ((rel-file (substring file drop-count))
                           (dest-file (ss to "/" rel-file)))
                      (fps-copy file dest-file)))
                  files))
      (fps-copy from to)))


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

;; Return filepath part of fpd.
(define (fpd->fps-dir fpd)
  (fpd->fps (fpd-dir fpd)))

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

;; Replace "from" with "to" in fpd.
(define (fpd-sub fpd from to)
  (let lp ((parts (cdr fpd))
             (ret '()))
      (if (pair? parts)
          (let ((part (car parts)))
            (lp (cdr parts)
                (if (string=? from part)
                    (cons to ret)
                    (cons part ret))))
          (cons (car fpd) (reverse ret)))))

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
             ((and (pair? tail) (string=? (car tail) "..")) (lp (cdr tail) (cdr base)))
             ((and (pair? tail) (string=? (car tail) ".")) (lp (cdr tail) base))
             (else (cons 'abs (append (reverse tail) base))))))))

;; Return fpd as rel, if fpd is of dot type.
;;
(define (fpd->rel fpd)
  (if (fpd-dot? fpd)
      (cons 'rel (cdr fpd))
      fpd))

;; File-path-string to file-path-descriptor (fpd).
(define (fps->fpd fps)

  (let ((type (fps-type fps))
        (len (string-length fps)))

    (if (and (eq? type 'dot)
             (or (= len 1)
                 (= len 2)))

        (list type)                     ; Special case: (dot)

        (let lp ((i (if (eq? type 'abs) 1 0))
                 (s (if (eq? type 'abs) 1 0))
                 (ret '()))

          (if (< i len)

              (let ((ch (string-ref fps i)))

                (cond

                 ;; Dir separator.
                 ((char=? ch #\/)
                  (if (> i s)
                      (lp (1+ i)
                          (1+ i)
                          (cons (substring fps s i) ret))
                      (lp (1+ i)
                          (1+ i)
                          ret)))

                 ;; Relative to current.
                 ((and (= i s)
                       (char=? ch #\.)
                       (< (1+ i) len))
                  (cond
                   ((char=? (string-ref fps (1+ i)) #\.)
                    ;; Offset dir, upwards.
                    (lp (+ i 2)
                        (+ i 2)
                        (cons ".." ret)))
                   ((char=? (string-ref fps (1+ i)) #\/)
                    ;; Dotted dir.
                    (lp (1+ i)
                        (1+ i)
                        ret))
                   (else
                    ;; Hidden name.
                    (lp (1+ i)
                        s
                        ret))))

                 ;; Name part.
                 (else (lp (1+ i)
                           s
                           ret))))

              (cons type
                    (if (> i s)
                        (cons (substring fps s i) ret)
                        ret)))))))


;; fpd to file-string.
(define (fpd->fps fpd)
  (case (fpd-type fpd)
    ((abs) (string-append "/" (string-join (reverse (fpd-body fpd)) "/")))
    ((rel) (string-join (reverse (fpd-body fpd)) "/"))
    ((dot) (if (pair? (cdr fpd))
               (string-append "./" (string-join (reverse (fpd-body fpd)) "/"))
               "."))
    ;; ((off) (string-append "../" (string-join (reverse (fpd-body fpd)) "/")))
    ((off) (string-join (reverse (fpd-body fpd)) "/"))
    ))



;; File-language processor.
;;
;; Examples:
;;
;;     (fpl "/foo/bar/jii.haa" "df") -> "/foo/bar/jii.haa"
;;     (fpl "/foo/bar/jii.haa" "db") -> "/foo/bar/jii"
;;     (fpl "/foo/bar/jii.haa" '("da" "my-file.txt")) -> "/foo/bar/my-file.txt"
;;
;; One-character commands:
;;
;;     s          - self
;;     S          - self (absolute path)
;;     f          - filename
;;     b          - basename
;;     e          - extname
;;     u          - endname (fullext)
;;     d          - dir (relative path)
;;     p          - path (full/absolute path)
;;     [0-9]      - slice (from end by count)
;;     [0-9][0-9] - slice (between indeces, higher is exclusive)
;;     <          - index begin (follow by number)
;;     >          - index end (follow by number)
;;     a          - argument (add/append directory part)
;;     c          - argument (concatenate filename part)
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
                  ((#\S) (lp (cdr chars) args (cons 'self-absolute ret)))
                  ((#\f) (lp (cdr chars) args (cons 'filename ret)))
                  ((#\b) (lp (cdr chars) args (cons 'basename ret)))
                  ((#\e) (lp (cdr chars) args (cons 'extname ret)))
                  ((#\u) (lp (cdr chars) args (cons 'endname ret)))
                  ((#\d) (lp (cdr chars) args (cons 'dir ret)))
                  ((#\p) (lp (cdr chars) args (cons 'path ret)))
                  ((#\a) (lp (cdr chars)
                             (cdr args)
                             ;; Null-string is not concatenated to the path.
                             (if (string-null? (car args))
                                 ret
                                 (cons (cons 'dir-argument (car args)) ret))))
                  ((#\c) (lp (cdr chars) (cdr args) (cons (cons 'ext-argument (car args)) ret)))
                  ((#\h) (lp (cdr chars) args (cons 'homepath ret)))
                  (else (pro (current-error-port)
                             (si "file-path: unknown command character: \"#{(car chars)}\""))
                        (lp (cdr chars) args ret))))

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

                             ((self-absolute) (lp (cdr commands)
                                                  (cons (fpd->fps (fpd->abs fpd)) ret)))

                             ((filename) (lp (cdr commands)
                                             (cons (fpd-file fpd) ret)))

                             ((basename) (lp (cdr commands)
                                             (cons (fps-base (fpd-file fpd)) ret)))

                             ((extname) (lp (cdr commands)
                                            (cons (fps-ext (fpd-file fpd)) ret)))

                             ((endname) (lp (cdr commands)
                                            (cons (fps-end (fpd-file fpd)) ret)))

                             ((dir) (lp (cdr commands)
                                        (cons (fpd->fps (fpd-dir fpd)) ret)))

                             ((path) (lp (cdr commands)
                                         (cons (fpd->fps (fpd-dir (fpd->abs fpd))) ret)))

                             ((homepath) (lp (cdr commands)
                                             (cons (getenv "HOME") ret)))
                             ))
                       (let ((parts (reverse ret)))
                         (string-join (if (or (and (string? (car parts))
                                                   (string-null? (car parts)))
                                              (unspecified? (car parts)))
                                          (cdr parts)
                                          parts)
                                      "/")))))
               (if (list? (car expr-list))
                   expr-list
                   (list expr-list)))))

    (if (= (length res) 1)
        (car res)
        res)))



;; (define test-1 #t)
;; (define test-2 #t)

#;
(when test-1
  (let ()

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
    (pr (fps-end p))
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
    (pr (fpl p '("dbc" ".gtkw")))
    (pr (fpl p "3"))
;;     (pr (fpl "/foo/bar/jii.haa" "df"))
;;     (pr (fpl "/foo/bar/jii.haa" "db"))
;;     (pr (fpl "/foo/bar/jii.haa" '("da" "my-file.txt")))
    ))

#;
(when test-2
  (let ()
    (define p "foo/bar/hii.haa.txt")
;;      (pr (fpl p "db"))
;;     (pr (fpl p "dbc" ".foo"))
;;     (pr (fpl p '("dbc" ".foo")))
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

;; (pr (fpl "tmp/gen-mmap-csr" "sac" "csr_1" ".v"))
;; (pr (fpl "tmp/gen-mmap-csr" "sac" "csr_1" ".v"))
;; (pr (fpl "/foo/bar/jii.haa" "df"))

;; (pr (fpl "" "sa" "gu.c"))
;; (pr (fpl "gu.c" "abc" "" ".o"))
;; (pd (fpl "../dii/duu.daa" "d"))
;; (pd (fpl "../duu.daa" "d"))
;; (pd (fpd->fps (fps->fpd "../duu.daa")))
;; (pd (fps->fpd "./duu.daa"))
;; (pd (fps->fpd "../duu.daa"))
;; (pd (fps->fpd "../dii/duu.daa"))
;; (pd (fpd->fps (fps->fpd "./duu.daa")))


;; (ppre (fps-file "diiduu/"))
;; (pd (fps-find "diiduu"))

;; (let ((files '()))
;;   (fps-recurse "test" (lambda (file) (set! files (cons file files))))
;;   (pd (reverse files)))
;; (fps-copy-r "diiduu" "foobar")

;; (pd (fps-clean "//foo////bar/dii/"))
;; (pd (fps-clean "//foo////bar/dii/foo.txt"))
;; (pd (fps-clean "dii/foo.txt"))


;; (pd (fps->fpd "/"))
;; (pd (fps->fpd "/foo"))
;; (pd (fps->fpd "/foo/bar"))
;; (pd (fps->fpd "."))
;; (pd (fps->fpd "./"))
;; (pd (fps->fpd "../"))
;; (pd (fps->fpd "./foo"))
;; (pd (fps->fpd "../foo"))
;; (pd (fpd->fps (fps->fpd "/")))
;; (pd (fpd->fps (fps->fpd "/foo")))
;; (pd (fpd->fps (fps->fpd "/foo/bar")))
;; (pd (fpd->fps (fps->fpd ".")))
;; (pd (fpd->fps (fps->fpd "./")))
;; (pd (fpd->fps (fps->fpd "../")))
;; (pd (fpd->fps (fps->fpd "./foo")))
;; (pd (fpd->fps (fps->fpd "../foo")))

;; (pd (fps-sub "tb/ss/test/test-1-2.scm" "test" "runs"))

;; (pr (fps-glob "*"))

;; (ppr (fps-ls "."))

;; (ppr (fps-join "./dii/duu" "foo/bar"))
;; (ppr (fps-join "./dii/duu" "../foo/bar"))
;; (ppr (fps-join "./dii/duu" "/foo/bar"))
;; (ppr (fps-join "./dii/duu" "./foo/bar"))

;; (pd (fpl "duu.daa" "db"))
;; (pd (fpl "./duu.daa" "db"))
;; (pd (fpl "../duu.daa" "db"))
