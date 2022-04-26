(define-module (tuile dir)
  #:use-module ((ice-9 ftw) #:select (file-system-tree))
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:export
  (
   dir-find
   ))


(define (dir-find dir-or-file)
  (define remove-stat
    ;; Remove the "stat" objects the "file-system-tree" provides
    ;; for each file in the tree.
    (match-lambda
      ((name stat)                      ; Flat file.
       name)
      ((name stat children ...)         ; Directory.
       (list name (map remove-stat children)))))

  (remove-stat (file-system-tree dir-or-file)))
