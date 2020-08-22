(define-module (tuile json-util)
  #:use-module (tuile massoc)
  #:use-module (json)
  #:export
  (json-load
   json-dump))


;; Load JSON file.
(define (json-load filename)
  (with-input-from-file filename
    (lambda ()
      (json->scm))))


;; Dump data to JSON file using pretty formatting.
(define (json-dump filename data)
  (with-output-to-file filename
    (lambda ()
      (scm->json data #:pretty #t))))
