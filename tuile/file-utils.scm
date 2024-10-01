(define-module (tuile file-utils)
  #:use-module (tuile pr)
  #:use-module (tuile file-path)
  #:use-module ((tuile utils) #:select (dir-glob opt-arg))
  #:export
  (
   file-copy
   file-move
   file-touch
   file-mkdir-p
   file-path-p
   file-uid-of-name
   file-name-of-uid
   file-gid-of-name
   file-names-in-gid
   file-chown
   file-chgrp
   file-chmod
   file-directory?
   file-file?
   file-list
   file-list-files
   file-list-dirs
   file-list-files-r
   file-list-dirs-r
   file-glob
   file-filter-files
   file-chdir
   file-remove
   file-dir-empty?
   file-find-upwards
   ))


;; Copy file or directory to destination file or directory.
;;
;; If "from" is directory, the whole directory is copied recursively
;; to the "to" directory.
;;
;; If "from" is a file and:
;;
;;   * "to" is a directory: the file is copied to that directory.
;;
;;   * "to" is not a directory: the file is copied as "to", and
;;   missing directories are created, if needed.
;;
(define (file-copy from to)
  (cond
   ((file-is-directory? from)
    (fps-copy-r from to))
   (else
    (cond
     ((file-is-directory? to)
      (let ((to-file (ss to "/" (fps-file from))))
        (fps-copy from to-file)))
     (else
      (fps-copy from to))))))


;; Move (rename) file or directory.
(define file-move rename-file)

;; Create a file to default (#o644) or specified mode.
(define (file-touch file . mode)
  (open file
        O_CREAT
        (if (pair? mode)
            (car mode)
            #o644)))

;; Recursive create directories.
(define file-mkdir-p fps-mkdir-p)

;; Recursive create directories for the full file path.
(define file-path-p fps-mkpath-p)

;; Return uid of name.
(define (file-uid-of-name name)
  (vector-ref (getpwnam name) 0))

;; Return name of uid.
(define (file-name-of-uid uid)
  (vector-ref (getpwuid uid) 0))

;; Return group id of name.
(define (file-gid-of-name name)
  (vector-ref (getgrnam name) 2))

;; Return names for group id.
(define (file-names-in-gid gid)
  (vector-ref (getgrgid gid) 3))

;; Change owner of the file.
(define (file-chown file owner)
  (if (file-exists? file)
      (begin
        (chown file (file-uid-of-name owner) (stat:gid (stat file)))
        #t)
      #f))

;; Change group of the file.
(define (file-chgrp file group)
  (if (file-exists? file)
      (begin
        (chown file (stat:uid (stat file)) (file-gid-of-name group))
        #t)
      #f))

;; Change mode of the file.
(define (file-chmod file mode)
  (if (file-exists? file)
      (begin
        (chmod file mode)
        #t)
      #f))

;; Does file exist and is it a directory?
(define (file-directory? file-or-dir)
  (and (file-exists? file-or-dir)
       (file-is-directory? file-or-dir)))

;; Does file exist and is it a (regular) file?
(define (file-file? file-or-dir)
  (and (file-exists? file-or-dir)
       (not (file-is-directory? file-or-dir))))

;; List entries in directory.
(define (file-list dir)
  (if (file-directory? dir)
      (let* ((ds (opendir dir))
             (ret (let lp ((fs (readdir ds))
                           (ret '()))
                    (if (eof-object? fs)
                        (reverse ret)
                        (case (string-ref fs 0)
                          ((#\.)
                           (cond
                            ((string=? fs ".")
                             (lp (readdir ds)
                                 ret))
                            ((string=? fs "..")
                             (lp (readdir ds)
                                 ret))
                            (else (lp (readdir ds)
                                      (cons fs ret)))))
                          (else (lp (readdir ds)
                                      (cons fs ret))))))))
        (closedir ds)
        ret)
      #f))


;; List files in the directory.
(define (file-list-files dir)
  (filter (negate file-is-directory?) (file-list dir)))


;; List directories in the directory.
(define (file-list-dirs dir)
  (filter file-is-directory? (file-list dir)))


;; List files recursively.
(define (file-list-files-r dir)
  (let ((files '()))
    (fps-recurse dir (lambda (file) (set! files (cons file files))))
    (reverse files)))


;; List directories recursively.
(define (file-list-dirs-r dir)
  (let ((dirs '()))
    (fps-recurse-dir-before dir
                            (lambda (dir) (set! dirs (cons dir dirs)))
                            identity)
    (reverse dirs)))


;; Glob for pattern.
(define (file-glob pat)
  (let ((fpd (fps->fpd pat)))
    (dir-glob (fpd-dir fpd) (fpd-file fpd))))


;; Return filtered list of files.
(define (file-filter-files dir proc)
  (let ((files '()))
    (fps-recurse dir
                 (lambda (file)
                   (when (proc file)
                     (set! files (cons file files)))))
    (reverse files)))


;; Change directory "dir" and return true if directory existed,
;; otherwise return false.
(define (file-chdir dir)
  (if (file-directory? dir)
      (begin
        (chdir dir)
        #t)
      #f))


;; Remove file (if exists) or remove recursively the directory?
(define (file-remove file-or-dir)

  (define (remove-item file-or-dir)
    (if (file-is-directory? file-or-dir)
        (rmdir file-or-dir)
        (delete-file file-or-dir)))

  (if (file-directory? file-or-dir)
      (begin
        (fps-recurse-dir-after file-or-dir
                               remove-item
                               remove-item)
        #t)
      (if (file-exists? file-or-dir)
          (begin
            (delete-file file-or-dir)
            #t)
          #f)))


;; Is file a directory and empty?
(define (file-dir-empty? dir)
  (and (file-directory? dir)
       (null? (file-list dir))))


;; Find file from the current or parent directory.
(define (file-find-upwards filename . opt-limit)
  (define (->path dir file) (string-append dir "/" file))
  (let ((dir-search-limit (opt-arg opt-limit "/")))
    (let lp ((fpd (fps->fpd (getcwd))))
      (let* ((dir (fpd->fps fpd))
             (file (->path dir filename)))
        (if (or (string=? "/" dir) (string=? dir-search-limit dir))
            #f
            (if (file-exists? file)
                dir
                (lp (fpd-up fpd 1))))))))
