(define-module (tuile strline)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile pr)
  #:use-module ((tuile utils) #:select (char-separator? char-nonseparator?))
  #:use-module (tuile strpos)
  #:use-module ((srfi srfi-1) #:select (first second third drop drop-right last))
  #:export
  (
   strline-render-lines
   strline-decorate-lines
   ))


;; Render text to given line width.
(define (strline-render-lines width text)

  (define (create-lines specs)
    (map (lambda (spec)
           (let ((la (car spec))
                 (lb (cdr spec)))
             (substring text la lb)))
         specs))

  (define (indeces sa sb)
    (cons (strpos-index sa)
          (strpos-index sb)))

  ;;      
  ;;     ,sa    ,sb       ,sc
  ;;     foo bar foobardii dii duu jii haa juu hii"
  (let lp ((sa (strpos-create text))    ; Line start.
           (sb (strpos-create text))    ; Fits to line.
           (sc (strpos-find (strpos-create text) char-separator?)) ; Next separator.
           (ret '()))                   ; List of start-last pairs.
    ;; (pd (map strpos-index (list sa sb sc)))
    (if (strpos-char sa)
        (cond
         ((< (strpos-distance sa sc) width)
          (if (strpos-char sc)
              ;; Add segment to line.
              (lp sa
                  sc
                  (strpos-find (strpos-find sc char-nonseparator?) char-separator?)
                  ret)
              ;; Add tail segment.
              (create-lines (reverse (cons (indeces sa sc) ret)))))
         (else
          (cond
           ((or (= (strpos-distance sa sc) width)
                (= (strpos-distance sa sb) 0))
            ;; sa-sc segment, i.e. exact width or too long.
            (let ((start (strpos-find sc char-nonseparator?)))
              (lp start
                  start
                  (strpos-find start char-separator?)
                  (cons (indeces sa sc) ret))))
           (else
            ;; sa-sb segment, i.e. some segments fit.
            (let ((start (strpos-find sb char-nonseparator?)))
              (lp start
                  start
                  sc
                  (cons (indeces sa sb) ret)))))))
        (create-lines (reverse ret)))))


;; Decorate each line with style.
;;
;; style: (pre <prefix>)
;;        (pre-first-tail <first-prefix> <tail-prefix>)
;;        (pre-head-last <head-prefix> <last-prefix>)
;;        (post <postfix>)
;;        (post-first-tail <first-postfix> <tail-postfix>)
;;        (post-head-last <head-postfix> <last-postfix>)
;;
(define (strline-decorate-lines style lines)
  (case (first style)

    ((pre)
     (map (lambda (line) (string-append (second style) line)) lines))

    ((pre-first-tail)
     (cons (string-append (second style) (first lines))
           (map (lambda (line) (string-append (third style) line)) (cdr lines))))

    ((pre-head-last)
     (append (map (lambda (line) (string-append (second style) line)) (drop-right lines 1))
             (list (string-append (third style) (last lines)))))

    ((post)
     (map (lambda (line) (string-append line (second style))) lines))

    ((post-first-tail)
     (cons (string-append (first lines) (second style))
           (map (lambda (line) (string-append line (third style))) (cdr lines))))

    ((post-head-last)
     (append (map (lambda (line) (string-append line (second style))) (drop-right lines 1))
             (list (string-append (last lines) (third style)))))

    (else lines)))


;; (pd (strline-render-lines 7 "foo bar foobardii dii duuu jii haa juu hii"))
;; (pd (strline-render-lines 7 "foo bar foobardii dii duuu jii haa juuhuuuuuu hii"))
;; (pd (strline-render-lines 7 "foo bar foobardii dii duuu jii haa juu hiihuuuuuuu"))
;; (pd (strline-decorate-lines '(pre-head-last "++" "--")
;;                             (strline-render-lines 7 "foo bar foobardii dii duuu jii haa juu hii")))
;; (pd (strline-decorate-lines '(post-head-last "," "")
;;                             (strline-render-lines 7 "foo bar foobardii dii duuu jii haa juu hii")))
