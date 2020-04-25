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
;; absdir: absolute directory of file, if any.
;;
;; reldir: relative directory of file, if any.
;;
;; reldir-dotter: relative directory of file with "." or ".." prefix.
;;
;; dir: resolved directory of file, i.e. for absolute file path the
;;      dir itself and for relative file path the extended dir.
;;
;; *path: dir with filename included.

(define-module (tuile fn)
  #:export (
            fs-filename
            fs-basename
            fs-extname
            fs-fullext
            fs-absdir
            fs-reldir
            fs-reldir-dotted
            fs-dir
            fs-abspath
            fs-relpath
            fs-relpath-dotted
            fs-path

            fn-is-abs?
            fn-is-rel?
            fn->list
            fs->fn
            fn->fs
            ))


;; ------------------------------------------------------------
;; File:

;; Return filename of file-string.
(define (fs-filename fs)
  (car (fs->fn fs)))


;; Return basename of file-string.
(define (fs-basename fs)
  (car (string-split (fs-filename fs) #\.)))


;; Return extension of file-string.
(define (fs-extname fs)
  (car (last-pair (string-split (fs-filename fs) #\.))))


;; Return "full" extension of file-string.
(define (fs-fullext fs)
  (string-join (cdr (string-split (fs-filename fs) #\.)) "."))



;; ------------------------------------------------------------
;; Directory:

;; Return absolute directory of file-string.
(define (fs-absdir fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        (fn->fs (cdr fn))
        #f)))


;; Return relative directory of file-string.
(define (fs-reldir fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        #f
        (fn->fs (cdr fn)))))


;; Return relative directory of file-string with dot prefix.
(define (fs-reldir-dotted fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        #f
        (let ((ret (fn->fs (cdr fn))))
          (if (and (>= (string-length ret) 2)
                   (string=? ".." (substring ret 0 2)))
              ret
              (if (= (string-length ret) 0)
                  "."
                  (string-append "./" ret)))))))


;; Return resolved directory of file-string.
(define (fs-dir fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        (fn->fs (cdr fn))
        (string-append (getcwd) "/" (fn->fs (cdr fn))))))


;; Return absolute path of file-string.
(define (fs-abspath fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        fs
        #f)))


;; Return relative path of file-string.
(define (fs-relpath fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        #f
        (fn->fs fn))))


;; Return relative directory of file-string with dot prefix.
(define (fs-relpath-dotted fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        #f
        (string-append (fs-reldir-dotted fs) "/" (car fn)))))


;; Return resolved path of file-string.
(define (fs-path fs)
  (let ((fn (fs->fn fs)))
    (if (fn-is-abs? fn)
        (fn->fs fn)
        (string-append (getcwd) "/" (fn->fs fn)))))


;; ------------------------------------------------------------
;; fn:

;; fn is absolute path?
(define (fn-is-abs? fn)
  (list? fn))


;; fn is relative path?
(define (fn-is-rel? fn)
  (not (list? fn)))


;; Return proper/clean list of fn in order.
(define (fn->list fn)
  (let join ((ret '())
             (tail fn))
    (if (pair? tail)
        (join (cons (car tail) ret) (cdr tail))
        ret)))


;; File-string to fn.
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
      (string-append "/" (string-join (fn->list fn) "/"))
      (string-join (fn->list fn) "/")))
