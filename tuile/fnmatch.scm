(define-module (tuile fnmatch)
  #:use-module ((tuile basic) #:select (dir-list))
  #:export
  (
   fnmatch
   fnmatch-for-glob
   fnmatch-dir-glob
   ))

(define* (fnmatch pat
                  str
                  #:key
                  (pathname #f)
                  (extglob #f)
                  (noescape #f)
                  (dotmatch #f)
                  (casefold #f))

  ;;   (ppr (list "fnmatch : " pat " : " str))

  (let ((str (if casefold (string-downcase str) str))
        (pat (if casefold (string-downcase pat) pat))
        (limiter-string (list->string (append (if pathname '(#\/) '())
                                              (if dotmatch '() '(#\.))))))

    (define (limiter? si)
      (string-index limiter-string (string-ref str si)))

    (define (pathchar? si)
      (string-index "/." (string-ref str si)))

    ;; Parse from brackets range and selection.
    (define (parse-sel-ran pi)
      (let ((inverted? (string-index "^!" (string-ref pat pi))))
        (let lp ((pi (if inverted? (1+ pi) pi))
                 (type 'sel)
                 (ret '()))
          (case (string-ref pat pi)
            ((#\\) (if noescape
                       (lp (1+ pi)
                           type
                           (cons (string-ref pat pi) ret))
                       (lp (+ pi 2)
                           type
                           (cons (string-ref pat (1+ pi)) ret))))
            ((#\]) (values (1+ pi)
                           type
                           (if inverted? negate identity)
                           (list->string (reverse ret))))
            ((#\-) (lp (1+ pi) 'ran (cons (string-ref pat pi) ret)))
            (else (lp (1+ pi)
                      type
                      (cons (string-ref pat pi) ret)))))))

    ;; Parse from braces the options.
    (define (parse-opts pi)
      (define (->opt-list lst opt)
        (if (pair? opt)
            (cons (list->string (reverse opt)) lst)
            lst))
      (let lp ((pi pi)
               (opt '())
               (ret '()))
        (let ((ch (string-ref pat pi)))
          (case ch
            ((#\\) (if noescape
                       (lp (1+ pi) (cons ch opt) ret)
                       (lp (+ pi 2) (cons (string-ref pat (1+ pi)) opt) ret)))
            ((#\}) (values (1+ pi) (reverse (->opt-list ret opt))))
            ((#\,) (lp (1+ pi) '() (->opt-list ret opt)))
            (else (lp (1+ pi) (cons ch opt) ret))))))

    ;; ?
    (define (one pi si)
      (if (limiter? si)
          (values #f pi si)
          (values #t pi (1+ si))))

    ;; * or **
    (define (any pi si)
      ;;       (ppr (list "any : " (substring pat pi) " : " (substring str si)))
      (let lp ((pi pi)
               (si si)
               (non-first #f))
        (cond
         ((= si (string-length str))
          (values #t pi si))
         ((= pi (string-length pat))
          (if non-first
              (lp pi (1+ si) #t)
              (if (limiter? si)
                  (values #f pi si)
                  (lp pi (1+ si) #t))))
         ((string-index "*?" (string-ref pat pi))
          (case (string-ref str si)
            ((#\/) (if (fnmatch (substring pat (1+ pi))
                                (substring str si)
                                #:pathname pathname
                                #:extglob extglob
                                #:noescape noescape
                                #:dotmatch dotmatch
                                #:casefold #f)
                       (values 'exit pi si)
                       (lp pi (1+ si) #f)))
            ((#\.) (if dotmatch
                       (lp pi (1+ si) #t)
                       (values #f pi si)))
            (else (lp pi (1+ si) #t))))
         ((limiter? si)
          (if (< pi (string-length pat))
              (if non-first
                  (values #t pi si)
                  (values #f pi si))
              (if non-first
                  (lp pi (1+ si) #t)
                  (values #f pi si))))
         ((and (< si (string-length str))
               (fnmatch (substring pat pi)
                        (substring str si)
                        #:pathname pathname
                        #:extglob extglob
                        #:noescape noescape
                        #:dotmatch dotmatch
                        #:casefold #f))
          ;; No need to continue.
          ;;       /
          (values 'exit pi si))
         (else
          (lp pi (1+ si) (not (char=? (string-ref str si) #\/)))))))

    ;; a
    (define (lit pi si)
      (values (char=? (string-ref pat pi)
                      (string-ref str si))
              (1+ pi)
              (1+ si)))

    ;; [abc]
    (define (sel pi si fn-polarity options)
      (if (and (pathchar? si) (limiter? si))
          (values (fn-polarity #f) pi si)
          (values (fn-polarity (string-index options (string-ref str si)))
                  pi
                  (1+ si))))

    ;; [a-c] or [a-bc]
    (define (ran pi si fn-polarity limits)
      (let ((ch (string-ref str si)))
        (let lp ((li 0))
          (if (< li (string-length limits))
              (let ((ch0 (string-ref limits li))
                    (ch1 (if (< (1+ li) (string-length limits))
                             (string-ref limits (1+ li))
                             #f)))
                (if (and ch1
                         (char=? ch1 #\-)
                         (< (+ li 2) (string-length limits)))
                    (if (and (char<=? ch0 ch)
                             (char>=? (string-ref limits (+ li 2)) ch))
                        (values (fn-polarity #t) pi (1+ si))
                        (lp (+ li 3)))
                    (if (char=? ch0 ch)
                        (if (and (pathchar? si) (limiter? si))
                            (values (fn-polarity #f) pi si)
                            (values (fn-polarity #t) pi (1+ si)))
                        (lp (1+ li)))))
              (values (fn-polarity #f) pi si)))))

    ;; {ab,bc}
    (define (opt pi si options)
      (let lp ((options options))
        (if (pair? options)
            (if (string-contains (substring str si) (car options))
                (values #t pi (+ si (string-length (car options))))
                (lp (cdr options)))
            (values #f pi si))))


    ;; Loop until result is clear, and return #t or #f.
    (let lp ((pi 0)
             (si 0))

      (cond

       ((= pi (string-length pat))
        (= si (string-length str)))

       ((= si (string-length str))
        (= pi (string-length pat)))

       (else
        (case (string-ref pat pi)
          ((#\*) (call-with-values (lambda () (any (1+ pi) si))
                   (lambda (res pi si)
                     (case res
                       ((exit) #t)
                       (else (lp pi si))))))
          ((#\?) (call-with-values (lambda () (one (1+ pi) si))
                   (lambda (res pi si)
                     (lp pi si))))
          ((#\[) (call-with-values (lambda () (parse-sel-ran (1+ pi)))
                   (lambda (pi type fn-polarity sel-ran)
                     (call-with-values (lambda ()
                                         (case type
                                           ((sel) (sel pi si fn-polarity sel-ran))
                                           (else (ran pi si fn-polarity sel-ran))))
                       (lambda (res pi si) (if res (lp pi si) #f))))))
          ((#\{) (if extglob
                     (call-with-values (lambda () (parse-opts (1+ pi)))
                       (lambda (pi opts)
                         (call-with-values (lambda () (opt pi si opts))
                           (lambda (res pi si) (if res (lp pi si) #f)))))
                     (call-with-values (lambda () (lit pi si))
                       (lambda (res pi si) (if res (lp pi si) #f)))))
          ((#\\) (if noescape
                     (call-with-values (lambda () (lit pi si))
                       (lambda (res pi si) (if res (lp pi si) #f)))
                     (call-with-values (lambda () (lit (1+ pi) si))
                       (lambda (res pi si) (if res (lp pi si) #f)))))
          (else (call-with-values (lambda () (lit pi si))
                  (lambda (res pi si) (if res (lp pi si) #f))))))))))


(define* (fnmatch-for-glob pat str #:key (dotmatch #f))
  (fnmatch pat
           str
           #:pathname #t
           #:extglob #t
           #:dotmatch dotmatch))

;; Match glob pattern for dir entries.
;;
;;     (fmmatch-dir-glob "foo/bar" "*.mp3")
;;
(define (fnmatch-dir-glob dir pat)
  (filter (lambda (str) (fnmatch-for-glob pat str)) (dir-list dir)))


;; (use-modules (tuile pr))
;; (ppr (fnmatch "ca[a-z]" "cat"))
;; (ppr (fnmatch "**/*.rb" "lib/song.rb"))
