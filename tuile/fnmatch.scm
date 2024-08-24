(define-module (tuile fnmatch)
  #:export
  (
   fnmatch
   ))


(define (fnmatch pat str)

  (define (limiter? si) (string-index "/." (string-ref str si)))

  ;; Parse from brackets range and selection.
  (define (parse-sel-ran pi)
    (let lp ((pi pi)
             (type 'sel)
             (ret '()))
      (case (string-ref pat pi)
        ((#\\) (lp (+ pi 2) type (cons (string-ref pat (1+ pi)) ret)))
        ((#\]) (values (1+ pi) type (list->string (reverse ret))))
        ((#\-) (lp (1+ pi) 'ran ret))
        (else (lp (1+ pi)
                  type
                  (cons (string-ref pat pi) ret))))))

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
          ((#\\) (lp (+ pi 2) (cons (string-ref pat (1+ pi)) opt) ret))
          ((#\}) (values (1+ pi) (reverse (->opt-list ret opt))))
          ((#\,) (lp (1+ pi) '() (->opt-list ret opt)))
          (else (lp (1+ pi) (cons ch opt) ret))))))

  ;; ?
  (define (one pi si)
    (if (not (limiter? si))
        (values #t pi (1+ si))
        (values #f pi si)))

  ;; *
  (define (any pi si)
    (let lp ((pi pi)
             (si si))
      (cond
       ((= si (string-length str))
        (values #t pi si))
       ((and (< pi (string-length pat))
             (string-index "*?" (string-ref pat pi)))
        (lp (1+ pi) si))
       ((limiter? si)
        (if (< pi (string-length pat))
            (values #t pi si)
            (lp pi (1+ si))))
       ((and (< si (string-length str))
             (fnmatch (substring pat pi)
                      (substring str si (1+ si))))
        ;; No need to continue.
        ;;       /
        (values 'exit pi si))
       (else
        (lp pi (1+ si))))))

  ;; a
  (define (lit pi si)
    (values (char=? (string-ref pat pi)
                    (string-ref str si))
            (1+ pi)
            (1+ si)))

  ;; [abc]
  (define (sel pi si options)
    (values (string-index options (string-ref str si))
            pi
            (1+ si)))

  ;; [a-c]
  (define (ran pi si limits)
    (let ((ch (string-ref str si))
          (ch-s (string-ref limits 0))
          (ch-e (string-ref limits 1)))
      (values (and (char<=? ch-s ch)
                   (char>=? ch-e ch))
              pi
              (1+ si))))

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
                 (lambda (pi type sel-ran)
                   (call-with-values (lambda ()
                                       (case type
                                         ((sel) (sel pi si sel-ran))
                                         (else (ran pi si sel-ran))))
                     (lambda (res pi si) (if res (lp pi si) #f))))))
        ((#\{) (call-with-values (lambda () (parse-opts (1+ pi)))
                 (lambda (pi opts)
                   (call-with-values (lambda () (opt pi si opts))
                     (lambda (res pi si) (if res (lp pi si) #f))))))
        ((#\\) (call-with-values (lambda () (lit (1+ pi) si))
                 (lambda (res pi si) (if res (lp pi si) #f))))
        (else (call-with-values (lambda () (lit pi si))
                (lambda (res pi si) (if res (lp pi si) #f)))))))))



;; (use-modules (tuile fmt))
;; 
;; (define (test-fnmatch pat str)
;;   (display (fmt `(lal 14 ,pat) " : " `(lal 14 ,str) " : " (fnmatch pat str) "\n")))
;; 
;; (test-fnmatch "*" "foo")
;; (test-fnmatch "*" "foo.mp3")
;; (test-fnmatch "*.mp3" "foo.mp3")
;; (test-fnmatch "*.mp" "foo.mp3")
;; (test-fnmatch "foo" "foo")
;; (test-fnmatch "fo*" "foo")
;; (test-fnmatch "bo*" "foo")
;; (test-fnmatch "*o" "foo")
;; (test-fnmatch "*a" "bar")
;; (test-fnmatch "*ar" "bar")
;; (test-fnmatch "b[a-b]r" "bar")
;; (test-fnmatch "*[a-b]r" "bar")
;; (test-fnmatch "*.sh" "foobar.sh")
;; (test-fnmatch "*.sh" "foobar.bash")
;; (test-fnmatch "b[bac]r" "bar")
;; (test-fnmatch "b[bfc]r" "bar")
;; (test-fnmatch "foo.*" "foo.mp3")
;; (test-fnmatch "bar.*" "foo.mp3")
;; (test-fnmatch "\\a\\\\" "a\\")
;; (test-fnmatch "{ba,fo}r" "bar")
