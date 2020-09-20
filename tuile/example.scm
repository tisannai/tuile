(define-module (tuile example)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (tuile pr)
  #:use-module (tuile utils)
  )


;; ------------------------------------------------------------
;; Two counter examples:
;;     counter-1: Counter with raw syntax.
;;     counter-2: Counter with "this" syntax.



;; ------------------------------------------------------------
;; Counter-1 class (object)
;; ------------------------------------------------------------

;; Counter-1 class (object)
;;
;;            name      inheritance list
(define-class <counter-1> ()
  ;; Slot list.
  (value #:init-keyword #:value         ; Slot called "value", with
                                        ; initialization keyword
                                        ; "#:value".

         #:init-form 0)                 ; Initial value when not
                                        ; explicitly set.
  (step  #:init-keyword #:step
         #:init-form 1))


;; Increment method for <counter> with optional step-size argument.
(define-method (incr (this <counter-1>) . rest)
  (let ((step (if (pair? rest)
                  (car rest)
                  (slot-ref this 'step))))
    ;; "slot-set!" is used to give slots new values.
    ;;
    ;;         obj  slot (as symbol)
    ;;                        slot value reference
    (slot-set! this 'value (+ (slot-ref this 'value)
                              step))))

(define-method (reset (this <counter-1>))
  (slot-set! this 'value 0))

(define-method (get-value (this <counter-1>))
  (slot-ref this 'value))

;; Create counter.
;;
;;         make class     keyword value (for "value").
(define c (make <counter-1> #:value 1))

;; Display the complete object.
(describe c)

;; Get initial value of counter.
(get-value c)

;; Increment with default increment.
(incr c)

;; Increment with 2.
(incr c)

(get-value c)

;; Reset counter.
(reset c)



;; ------------------------------------------------------------
;; counter-2 class (object)
;; ------------------------------------------------------------

(define-this-class <counter-2> ()
  (value 0)
  (step 1))

(define-this-method <counter-2> (incr . rest)
  (let ((step (if (pair? rest)
                  (car rest)
                  (this-ref step))))
    (this-set! value (+ (slot-ref this 'value)
                        step))))

(define-this-method <counter-2> (reset)
  (this-set! value 0))

(define-this-method <counter-2> (get-value)
  (this-ref value))


;; Create counter.
;;
;;         make class     keyword value (for "value").
(define c (make <counter-2> #:value 1))

;; Display the complete object.
(describe c)

;; Get initial value of counter.
(get-value c)

;; Increment with default increment.
(incr c)

;; Increment with 2.
(incr c)

(get-value c)

;; Reset counter.
(reset c)

