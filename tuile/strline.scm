(define-module (tuile strline)
  #:use-module (tuile record-r6rs)
  #:use-module (tuile pr)
  #:use-module ((tuile utils) #:select (char-separator? char-nonseparator?))
  #:use-module (tuile strpos)
  #:use-module ((srfi srfi-1) #:select (first second third))
  #:export
  (
   strline-render-lines
   strline-prefix-lines
   ))


(define (strline-render-lines text width)

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


(define (strline-prefix-lines lines style)
  (case (car style)
    ((all)
     (map (lambda (line) (string-append (second style) line)) lines))
    ((first-rest)
     (cons (string-append (second style) (first lines))
           (map (lambda (line) (string-append (third style) line)) (cdr lines))))
    (else lines)))

#;
(define (strline-render-lines-opt text width)
  ;; NOTE: This does not support multiple spaces!
  (define (create-lines specs)
    (map (lambda (spec)
           (let ((la (car spec))
                 (lb (cdr spec)))
             (substring text la lb)))
         specs))

  ;;      
  ;;     ,la    ,lb       ,i
  ;;     foo bar foobardii dii duu jii haa juu hii"
  (let lp ((la 0)                       ; Line start.
           (lb #f)                      ; Last whitespace, if any.
           (i 0)                        ; Current char.
           (ret '()))                   ; List of start-last pairs.
    (cond
     ((>= i (string-length text))
      (create-lines (reverse (if (not (= la i))
                                 (if lb
                                     (if (> (- i la) width)
                                         (cons (cons (1+ lb) i)
                                               (cons (cons la lb) ret))
                                         (cons (cons la i) ret))
                                     (cons (cons la i) ret))
                                 ret))))
     ((char-whitespace? (string-ref text i))
      (if (> (- i la) width)
          (if lb
              (lp (1+ lb) #f (1+ lb) (cons (cons la lb) ret))
              (lp (1+ i) #f (1+ i) (cons (cons la i) ret)))
          (lp la i (1+ i) ret)))
     (else (lp la lb (1+ i) ret)))))



;; (pd (strline-render-lines "foo bar foobardii dii duuu jii haa juu hii" 7))
;; (pd (strline-render-lines "foo bar foobardii dii duuu jii haa juuhuuuuuu hii" 7))
;; (pd (strline-render-lines "foo bar foobardii dii duuu jii haa juu hiihuuuuuuu" 7))
