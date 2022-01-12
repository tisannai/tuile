(define-module (tuile csv-util)
  #:export
  (
   csv-parse-from-port
   read-csv-file-with-options
   ))

(use-modules (tuile utils))
(use-modules (srfi srfi-43))
(use-modules (srfi srfi-19))
(use-modules (ice-9 textual-ports))


;; Convert csv input (from port) to list of lines, where each line
;; includes list of fields.
;;
;; port          Streaming port (file or string stream).
;; separator     Column separator (default: #\;)
;; ignores       String including ignoreable chars (default: "\r").
(define* (csv-parse-from-port port #:key (separator #\;) (ignores "\r"))

  (define (quote? ch) (char=? ch #\"))
  (define (escape? ch) (char=? ch #\\))
  (define (separator? ch) (char=? ch separator))
  (define (newline? ch) (char=? ch #\newline))
  (define (+ch ch field) (cons ch field))
  (define (+field field line) (cons (list->string (reverse field)) line))
  (define (+line line lines) (cons (reverse line) lines))

  (let parse-char ((state 'in-field)
                   (field '())
                   (line '())
                   (lines '()))
    (let ((ch (get-char port)))
      (cond
       ((eof-object? ch)                ; End of file.
        (reverse lines))
       ((string-index ignores ch)       ; Ignored char.
        (parse-char state field line lines))
       (else                            ; Regular input.
        (case state
          ((in-field)        ; Normal field input, i.e. not-in-string.
           (cond ((quote? ch)
                  (parse-char 'in-string field line lines))
                 ((separator? ch)
                  (parse-char state '() (+field field line) lines))
                 ((newline? ch)
                  (parse-char state '() '() (+line (+field field line)
                                                   lines)))
                 (else
                  (parse-char state (+ch ch field) line lines))))
          ((in-string)                  ; Within string.
           (cond ((escape? ch)
                  (parse-char state (+ch (get-char port) field) line lines))
                 ((quote? ch)
                  (parse-char 'in-field field line lines))
                 (else
                  (parse-char state (+ch ch field) line lines))))))))))


;; Return list of parsed CSV-file lines.
;;
;; Positional arguments:
;;     filename          Filename
;;
;; Keyword arguments:
;;     file-encoding     Character encoding, default "binary" (options: binary, UTF-16LE)
;;     field-separator   CSV file field separator as char (default: ";").
;;
(define* (read-csv-file-with-options filename
                                     #:key
                                     (file-encoding "binary")
                                     (field-separator #\;))

  (apply call-with-input-file
         (append (list filename
                       (lambda (port)
                         (when (string=? file-encoding "UTF-16LE")
                           ;; Eat out the header chars.
                           (get-char port))
                         (csv-parse-from-port port #:separator field-separator)))
                 (cond
                  ((string=? "binary"   file-encoding)
                   (list #:binary #t))
                  ((string=? "UTF-8" file-encoding)
                   (list #:encoding "UTF-8"))
                  ((string=? "UTF-16LE" file-encoding)
                   (list #:encoding "UTF-16LE"))
                  ))))
