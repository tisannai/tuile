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
;; by the current directory. PRJDIR_FILE environment variable can be
;; used to define the root file name if default is not usable.
;;
;; Another file based way is to place the ".prjdir" file to HOME
;; directory and define the project root as the file content. Note that
;; if HOME directory is the project root, it must be listed in the
;; file. This way of definition supports one global setting for project
;; root.
;;
;; Project directory root may be defined also with the help of
;; PRJDIR_ROOT or PRJDIR_RE environment variables.
;;
;; If PRJDIR_RE is defined, the part of current directory that
;; matches PRJDIR_RE is used as project root. Hence, expansion is
;; dynamic and depends on what is the current working directory. This
;; expansion only works when the user is within the project directory
;; hierarchy.
;;
;; If PRJDIR_ROOT is defined, the root directory is taken as
;; PRJDIR_ROOT value. PRJDIR_ROOT is used as "backup" for other means.
;;
;;
;; The lookup priority for definitions:
;;
;; 1. .../.prjdir
;; 2. <PRJDIR_RE>
;; 3. <HOME>/.prjdir
;; 4. <PRJDIR_ROOT>
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
   prjdir-filename
   prjdir-root
   prjdir-root-if
   prjdir-expand
   prjdir-clear
   ))


;; Project file name (memorized).
(define prjdir-filename
  (let ((file #f))
    (lambda ()
      (if file
          file
          (if (getenv "PRJDIR_FILENAME")
              (begin
                (set! file (getenv "PRJDIR_FILENAME"))
                file)
              ".prjdir")))))


;; Cached project root.
(define cached-root #f)

;; Search needed for file based root.
(define search-needed #t)

;; Return current directory parts in reverse order.
(define (->parts) (reverse (cdr (string-split (getcwd) #\/))))

;; Construct absolute path from reversed directory parts.
(define (->path parts) (string-append "/"
                                      (string-join (reverse parts) "/")))

(define (home-file) (string-append (getenv "HOME")
                                   "/"
                                   (prjdir-filename)))


(define (extract-path-from-file file)
  (if (= (stat:size (stat file)) 0)
      (dirname file)
      (call-with-input-file file (lambda (port) (get-line port)))))


;; Return root directory or false.
(define (prjdir-root-if)

  (cond

   (cached-root cached-root)

   (search-needed
    (set! cached-root
      (let loop ((parts (->parts)))
        (if (pair? parts)
            (let ((file (->path (cons (prjdir-filename) parts))))
              (if (file-exists? file)
                  (extract-path-from-file file)
                  (loop (cdr parts))))
            #f)))
    (set! search-needed #f)
    cached-root)

   ((getenv "PRJDIR_RE")
    (let* ((pwd (getcwd))
           (m (re-match? (getenv "PRJDIR_RE") pwd)))
      (if m
          (substring pwd (car m) (cdr m))
          #f)))

   ((file-exists? (home-file))
    (set! cached-root (extract-path-from-file (home-file)))
    cached-root)

   ((getenv "PRJDIR_ROOT") (getenv "PRJDIR_ROOT"))

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
      (string-append (getcwd) "/" (substring filepath 2)))

     ;; Relative path (../).
     ((string=? (substring filepath 0 3) "../")
      (let loop ((ap (->parts))
                 (rp (string-split filepath #\/)))
        (if (string=? (car rp) "..")
            (loop (cdr ap) (cdr rp))
            (->path (append (reverse rp) ap)))))

     ;;     ;; Plain file.
     ;;     ((= (length (string-split filepath #\/) 1))
     ;;      (string-append (getcwd) "/" filepath))

     ;; Project path.
     (else
      (let ((root (prjdir-root)))
        (if domain
            (string-append (list root "/" domain "/" filepath))
            (string-append (list root "/" filepath))))))))


;; Clear caching related state.
(define (prjdir-clear)
  (set! cached-root #f)
  (set! search-needed #t))
