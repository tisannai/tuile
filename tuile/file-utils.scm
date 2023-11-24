(define-module (tuile file-utils)
  #:use-module (tuile pr)
  #:use-module (tuile file-path)
  #:export
  (
   file-copy
   file-move
   file-touch
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
        (fpl-recurse-dir-after file-or-dir
                               (lambda (file-or-dir)
                                 (remove-item file-or-dir)))
        #t)
      (if (file-exists?)
          (begin
            (delete-file file-or-dir)
            #t)
          #f)))


(define (file-dir-empty? dir)
  (and (file-directory? dir)
       (null? (file-list dir))))
