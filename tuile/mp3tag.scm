(define-module (tuile mp3tag)
  #:export
  (
;;    file-open
;;    file-close
;;    file-save
;;    file-save-id3v1
;;    get-title
;;    get-artist
;;    get-album
;;    get-comment
;;    get-genre
;;    get-year
;;    get-track
;;    set-title
;;    set-artist
;;    set-album
;;    set-comment
;;    set-genre
;;    set-year
;;    set-track
   with-file
   ))

(load-extension "libmp3tag" "init_mp3tag")

(define (with-file file thunk)
  (let* ((fh (file-open file))
         (ret (thunk fh)))
    (file-close fh)
    ret))
