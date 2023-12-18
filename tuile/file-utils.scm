(define-module (tuile file-utils)
  #:use-module (tuile pr)
  #:use-module (tuile file-path)
  #:use-module ((tuile utils) #:select (dir-glob))
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
   file-name-of-gid
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
   file-filter
   file-chdir
   file-remove
   file-dir-empty?
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


(define file-move rename-file)

(define (file-touch file . mode)
  (open file
        O_CREAT
        (if (pair? mode)
            (car mode)
            #o644)))

(define file-mkdir-p fps-mkdir-p)

(define file-path-p fps-mkpath-p)

(define (file-uid-of-name name)
  (vector-ref (getpwnam name) 0))

(define (file-name-of-uid uid)
  (vector-ref (getpwnam uid) 0))

(define (file-gid-of-name name)
  (vector-ref (getgrnam name) 0))

(define (file-name-of-gid gid)
  (vector-ref (getgrnam gid) 0))

(define (file-chown file owner)
  (if (file-exists? file)
      (begin
        (chown file (file-uid-of-name owner) (stat:gid (stat file)))
        #t)
      #f))

(define (file-chgrp file group)
  (if (file-exists? file)
      (begin
        (chown file (stat:uid (stat file)) (file-gid-of-name group))
        #t)
      #f))

(define (file-chmod file mode)
  (if (file-exists? file)
      (begin
        (chmod file mode)
        #t)
      #f))

(define (file-directory? file-or-dir)
  (and (file-exists? file-or-dir)
       (file-is-directory? file-or-dir)))

(define (file-file? file-or-dir)
  (and (file-exists? file-or-dir)
       (not (file-is-directory? file-or-dir))))

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


(define (file-list-files dir)
  (filter (negate file-is-directory?) (file-list dir)))


(define (file-list-dirs dir)
  (filter file-is-directory? (file-list dir)))


(define (file-list-files-r dir)
  (let ((files '()))
    (fps-recurse dir (lambda (file) (set! files (cons file files))))
    (reverse files)))


(define (file-list-dirs-r dir)
  (let ((dirs '()))
    (fps-recurse-dir-before dir
                            (lambda (file)
                              (when (file-is-directory? file)
                                (set! dirs (cons file dirs)))))
    (reverse dirs)))


(define (file-glob pat)
  (let ((fpd (fps->fpd pat)))
    (dir-glob (fpd-dir fpd) (fpd-file fpd))))


(define (file-filter dir proc)
  (let ((files '()))
    (fps-recurse-dir-before dir
                            (lambda (file)
                              (when (proc file)
                                (set! dirs (cons file dirs)))))
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
                               (lambda (file-or-dir)
                                 (remove-item file-or-dir)))
        #t)
      (if (file-exists? file-or-dir)
          (begin
            (delete-file file-or-dir)
            #t)
          #f)))


(define (file-dir-empty? dir)
  (and (file-directory? dir)
       (null? (file-list dir))))
