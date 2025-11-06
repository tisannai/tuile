(define-module (tuile config)
  #:use-module (tuile basic)
  #:export
  (
   config-name
   config-body
   config-entry
   config-key
   config-value
   config-get
   config-set!
   config-atom?
   config_list?
   ))


;; ------------------------------------------------------------
;; Internal:

(define (config-cassoc body key)

  (define (cassoc alist key)
    (let lp ((alist alist))
      (if (and (pair? alist)
               (pair? (car alist)))
          (if (eq? (caar alist) key)
              (car alist)
              (lp (cdr alist)))
          *unspecified*)))
  
  (define (cassoc-hier alist keys)
    (let lp ((alist alist)
             (keys keys)
             (entry *unspecified*))
      (if (and (pair? keys)
               (pair? alist))
          (let ((result (cassoc alist (car keys))))
            (if (unspecified? result)
                *unspecified*
                (lp (config-value result) (cdr keys) result)))
          entry)))
  
  (if (list? key)
      (cassoc-hier body key)
      (cassoc body key)))


;; Internal:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; External:

;; Return config name.
(define (config-name config)
  (cadr config))

;; Return config body.
(define (config-body config)
  (cddr config))

;; Return entry (key-value pair), from key.
(define (config-entry body key)
  (let ((entry (config-cassoc body key)))
    (if (unspecified? entry)
        entry
        (cons (config-key entry) (config-value entry)))))

(define (config-key entry)
  (car entry))

(define (config-value entry)
  (cadr entry))

;; Return value, from key.
(define (config-get body key)
  (let ((entry (config-cassoc body key)))
    (if (unspecified? entry)
        *unspecified*
        (if (pair? (cdr entry))
            (config-value entry)
            *unspecified*))))

;; Set value of entry, for key.
(define (config-set! body key value)
  (let ((entry (config-cassoc body key)))
    (if (unspecified? entry)
        #f
        (begin
          (set-cdr! entry (list value))
          #t))))

;; Set value, if missing, of entry, for key.
(define (config-set-missing! body key value)
  ;;   (define (add-missing! lst key value)
  ;;     (set-cdr! lst (list-copy lst))
  ;;     (set-car! lst (list key value)))
  (let* ((parts (list-split-tail key 1))
         (host-entry (config-cassoc body (car parts))))
    ;; (ppre host-entry)
    (if (unspecified? host-entry)
        #f
        (let ((entry (config-cassoc (config-value host-entry) (cadr parts))))
          ;; (ppre entry)
          (if (unspecified? entry)
              (begin
                ;; (add-missing! host-entry key value)
                ;; (ppre (config-value host-entry))
                (set-cdr! host-entry (list (cons (list (cadr parts) value) (config-value host-entry))))
                #t)
              #f)))))

;; Is value an atom, from key?
(define (config-atom? body key)
 (let ((entry (config-cassoc body key)))
   (if (unspecified? entry)
       *unspecified*
       (not (list? (config-value entry))))))

;; Is value a list, from key?
(define (config-list? body key)
 (let ((entry (config-cassoc body key)))
   (if (unspecified? entry)
       *unspecified*
       (list? (config-value entry)))))


;; External:
;; ------------------------------------------------------------


;; (use-modules (tuile pr))
#;
(let ()

  (define config '(project foobar
                           (file "diiduu.txt")
                           (numbers (1 2 3 4))
                           (tree ((a 1)
                                  (b (1 2))
                                  (c 2)))))
  (define body (config-body config))

  ;;   (ppre body)

  ;;   (ppr (config-name config))
  ;;   (ppr (config-get body 'file))
  ;;   (ppr (config-get body 'numbers))
  ;;   (ppr (config-get body '(tree a)))
  ;;   (ppr (config-get body '(tree b)))
  ;;   (config-set! body '(tree a) '(3 2 1))
  ;;   (ppr (config-get body '(tree a)))
  ;;   (config-set! body '(tree b) 12)
  ;;   (ppr (config-get body '(tree b)))
  ;;   (ppr (config-get body '(tree a z)))

  (ppr (config-set-missing! body '(tree b) 12))
  (ppr (config-get body '(tree b)))
  (ppr (config-get body '(tree d)))
  (ppr (config-set-missing! body '(tree d) 49))
  (ppr (config-get body '(tree d)))

  )
