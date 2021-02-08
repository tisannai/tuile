(define-module (tuile csv-util)
  #:export
  (
   csv-parse-line
   read-csv-file-with-options
   ))

(use-modules (tuile utils))
(use-modules (srfi srfi-43))
(use-modules (srfi srfi-19))
(use-modules (ice-9 textual-ports))


;; Return one parsed line as list of field from port. Stop if EOF is
;; encountered.
;;
;; CSV-line is typically separated into fields with semicolon. String may
;; contain newline and double-quotes. Double-quotes are escaped by
;; having two consecutive double-quotes act as one.
;;
;; Positional arguments:
;;     port        File port.
;;
;; Keyword arguments:
;;     separator   CSV file field separator as char (default: ";").
(define* (csv-parse-line port #:key (separator #\;))

  ;; Get char from port.
  (define (get-ch port)
    (get-char port))


  ;; Peek char from port.
  (define (peek-ch port)
    (lookahead-char port))

  (let loop ((fields '())
             (chars '())
             (in-str #f))

    (cond

     ((eof-object? (peek-ch port))
      (append fields (list (apply string chars))))

     ((and in-str
           (char=? #\" (peek-ch port)))
      (get-ch port)
      (if (or (eof-object? (peek-ch port))
              (not (char=? #\" (peek-ch port))))
          (loop fields (append chars (list #\")) #f)
          (loop fields (append chars (list #\" (get-ch port))) in-str)))

     ((and (not in-str)
           (char=? #\" (peek-ch port)))
      (loop fields (append chars (list (get-ch port))) #t))

     ((char=? #\return (peek-ch port))
      ;; Ignore RETURN chars.
      (get-ch port)
      (loop fields
            chars
            in-str))
     
     ((char=? #\newline (peek-ch port))
      (get-ch port)
      (append fields (list (apply string chars))))

     ((char=? separator (peek-ch port))
      (get-ch port)
      (loop (append fields (list (apply string chars)))
            '()
            in-str))

     (else
      (loop fields
            (append chars (list (get-ch port)))
            in-str)))))


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
                         (let loop ((lines '()))
                           (if (eof-object? (lookahead-char port))
                               lines
                               (loop (append lines (list (csv-parse-line port
                                                                         #:separator field-separator))))))))
                 (cond
                  ((string=? "binary"   file-encoding)
                   (list #:binary #t))
                  ((string=? "UTF-8" file-encoding)
                   (list #:encoding "UTF-8"))
                  ((string=? "UTF-16LE" file-encoding)
                   (list #:encoding "UTF-16LE"))
                  ))))
