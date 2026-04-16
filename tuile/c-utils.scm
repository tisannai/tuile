(define-module (tuile c-utils)
  #:use-module ((tuile edit) #:prefix #{eu:}#)
  #:use-module ((tuile utils))
  #:use-module ((tuile re))
  #:use-module ((tuile pr))
  #:export
  (
   get-function-prototypes
   get-markdown-summary
   ))


;; Example:
;;
;;     int* ilog2( int )
;;
;;                  int           *         ilog2              (  int  )
;;                  \\             \     \   \                  \  \    \     \
(define re-fn-start "^([a-zA-Z0-9_]+[*]*)([ ]+[a-zA-Z0-9_]+[*]*)+\\(.*(,|\\));?$")
(define re-fn-end "\\);?$")

(define re-fn-start-with-name "^([a-zA-Z0-9_]+[*]*[ ]+)([a-zA-Z0-9_]+[*]*[ ]+)*([a-zA-Z0-9_]+[*]*)\\(.*(,|\\));?$")


(define (get-function-prototypes filename static)
  (let* ((fn-start? (lambda (eu)
                      ((if static identity not) (eu:match-re? eu "^static"))))
         (eu (eu:read filename)))
    (let loop-lines ((protos '()))
      (if (eu:search-re eu re-fn-start)
          (if (fn-start? eu)
              (loop-lines (append protos
                                  (let loop-proto ((lines '()))
                                    (let ((cur-line (eu:get eu)))
                                      (if (eu:match-re? eu re-fn-end)
                                          (begin
                                            (eu:step eu)
                                            (append lines (list (string-append cur-line ";"))))
                                          (begin
                                            (eu:step eu)
                                            (loop-proto (append lines
                                                                (list cur-line)))))))))
              (begin
                (eu:step eu)
                (loop-lines protos)))
          protos))))


(define (get-markdown-summary filename)
  (let* ((lines (file->lines filename))
         (re-markdown-pre-brief "^/\\*\\*[ ]*$")
         (re-markdown-brief "^ \\* @brief (.*)$"))
    (let loop-lines ((lines lines)
                     (state 'pre-brief)
                     (briefs '())
                     (protos '()))
      (define-syntax-rule (continue) (loop-lines (cdr lines) state briefs protos))
      (if (pair? lines)
          (let ((line (car lines)))
            (case state
              ((pre-brief) (if (re-match? re-markdown-pre-brief line)
                               (loop-lines (cdr lines)
                                           'brief
                                           briefs
                                           protos)
                               (continue)))
              ((brief) (aif (re-matches re-markdown-brief line)
                            (loop-lines (cdr lines)
                                        'proto
                                        (cons (lr1 it) briefs)
                                        protos)
                            (loop-lines (cdr lines)
                                        'pre-brief
                                        briefs
                                        protos)))
              ((proto) (if (re-match? re-fn-start line)
                           (let ((name (lr3 (re-matches re-fn-start-with-name line))))
                             (loop-lines (cdr lines)
                                         'pre-brief
                                         briefs
                                         (cons name protos)))
                           (continue)))))
          (map (lambda (proto brief)
                 (si "* `#{proto}` : #{brief}"))
               (reverse protos) (reverse briefs))))))


;; (ppre (get-markdown-summary "plinth.h"))
