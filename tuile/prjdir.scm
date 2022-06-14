;; Prjdir - Expand filepath in project root directory context.

;; Filepath is expanded depending of the path formatting.
;;
;; Expansions:
;;
;; * If filepath starts with "/" (i.e. absolute path), no expansion is
;;   performed.
;;
;; * If filepath starts with "./" (i.e. relative path), "./" is
;;   replaced with current directory,
;;
;; * Otherwise filepath is extended with project root.
;;
;; The most flexible way of defining the project root is to place an
;; empty file (".prjdir") to the project root directory. This supports
;; having multiple project roots and project root gets selected always
;; by the current directory.
;;
;; Another file based way is to place the ".prjdir" file to HOME
;; directory and define the project root as the file content. Note that
;; if HOME directory is the project root, it must be listed in the
;; file. This way of definition supports one global setting for project
;; root.
;;
;; Project directory root may be defined also with the help of
;; PRJDIR_USER_RE or PRJDIR_USER_PATH environment variables.
;;
;; If PRJDIR_USER_RE is defined, the part of current directory that
;; matches PRJDIR_USER_RE is used as project root. Hence, expansion is
;; dynamic and depends on what is the current working directory. This
;; expansion only works when the user is within the project directory
;; hierarchy.
;;
;; If PRJDIR_USER_PATH is defined, the root directory is taken as
;; PRJDIR_USER_PATH value. PRJDIR_USER_PATH is additionally a "backup"
;; for PRJDIR_USER_RE detection. If PRJDIR_USER_RE fails but
;; PRJDIR_USER_PATH is defined, then root is defined by
;; PRJDIR_USER_PATH.
;;
;; The lookup priority for definitions:
;;
;; 1. .../.prjdir
;; 2. <PRJDIR_USER_RE>
;; 3. <PRJDIR_USER_PATH>
;; 4. <HOME>/.prjdir
;;
;; If empty file based root definition is used, the result will be
;; cached. Hence the first lookup takes some time, but the latter
;; lookups are instant. The cache can be cleared with "prjdir-clear".
;;
;; Filepath is expanded with:
;;
;;     (prjdir-expand <filepath> )
;;
;; Optionally the expansion can be called as:
;;
;;     (prjdir-expand <filepath> <domain> )
;;
;; In the latter format, the project root is extended with <domain>,
;; before <filepath> is appended as final piece.


(define-module (tuile prjdir)
  #:use-module ((tuile re) #:select (re-match?))
  #:use-module ((tuile utils) #:select (file->lines))
  #:use-module ((ice-9 textual-ports) #:select (get-line))
  #:export
  (
   prjdir-root
   prjdir-root-if
   prjdir-expand
   prjdir-clear
   ))


;; Cached project root.
(define cached-root #f)

;; Search needed for file based root.
(define search-needed #t)


;; Return root directory or false.
(define (prjdir-root-if)

  (define (->parts) (reverse (cdr (string-split (getcwd) #\/))))
  (define (->path parts) (string-append "/"
                                        (string-join (reverse parts) "/")))
  (define (home-file) (string-concatenate (list (getenv "HOME")
                                                "/"
                                                ".prjdir")))

  (cond

   (cached-root cached-root)

   (search-needed
    (set! cached-root
      (let loop ((parts (->parts)))
        (if (pair? parts)
            (if (file-exists? (->path (cons ".prjdir" parts)))
                (->path parts)
                (loop (cdr parts)))
            #f)))
    (set! search-needed #f)
    cached-root)

   ((getenv "PRJDIR_USER_RE")
    (let* ((pwd (getcwd))
           (m (re-match? (getenv "PRJDIR_USER_RE") pwd)))
      (if m
          (substring pwd (car m) (cdr m))
          #f)))

   ((getenv "PRJDIR_USER_PATH")
    (if (string-contains (getcwd) (getenv "PRJDIR_USER_PATH"))
        (getenv "PRJDIR_USER_PATH")
        #f))

   ((file-exists? (home-file))
    (set! cached-root (call-with-input-file (home-file) (lambda (port) (get-line port))))
    cached-root)

   (else #f)))


;; Return root directory or fail (with exception).
(define (prjdir-root)
  (let ((root (prjdir-root-if)))
    (if root
        root
        (raise-exception 'prjdir-error))))


;; Expand filepath with project root, if needed. Optionally a domain
;; is inserted, if any.
(define (prjdir-expand filepath . rest)
  (let ((domain (if (pair? rest) (car rest) #f)))
    (cond

     ;; Absolute path.
     ((char=? (string-ref filepath 0) #\/)
      filepath)

     ;; Relative path (./).
     ((string=? (substring filepath 0 2) "./")
      (string-concatenate (list (getcwd) "/" (substring filepath 2))))

     ;; Relative path (../).
     ((string=? (substring filepath 0 3) "../")
      (let loop ((ap (->parts))
                 (rp (string-split filepath #\/)))
        (if (string=? (car rp) "..")
            (loop (cdr ap) (cdr rp))
            (->path (append (reverse rp) ap)))))

     ;;     ;; Plain file.
     ;;     ((= (length (string-split filepath #\/) 1))
     ;;      (string-concatenate (list (getcwd) "/" filepath)))

     ;; Project path.
     (else
      (let ((root (prjdir-root)))
        (if domain
            (string-concatenate (list root "/" domain "/" filepath))
            (string-concatenate (list root "/" filepath))))))))


;; Clear caching related state.
(define (prjdir-clear)
  (set! cached-root #f)
  (set! search-needed #t))
