(define-module (tuile timer)
  #:use-module (srfi srfi-9)
  #:export
  (
   timer-create
   timer-start
   timer-end
   timer-value
   timer-values
   timer-open
   timer-time
   timer-close
   timer-reduce
   timer->string
   timer->display))


;;
;; Timer: Record timestamps from program run.
;;

;; ------------------------------------------------------------
;; Internal functions:

(define-record-type timer
  (make-timer name start-time end-time)
  timer?
  (name       timer-name)
  (start-time timer-start-time set-timer-start-time!)
  (end-time   timer-end-time   set-timer-end-time!))

(define (to-scalar val)
  (+ (* 1000000 (car val))
     (cdr val)))

(define (to-pair val)
  (cons (quotient val 1000000)
        (remainder val 1000000)))

(define (rest->name rest)
  (if (pair? rest)
      (car rest)
      #f))


;; ------------------------------------------------------------
;; External functions:

;; Create timer record.
(define (timer-create . rest)
  (make-timer (rest->name rest) #f '()))

;; Set start time.
(define (timer-start timer)
  (set-timer-start-time! timer (gettimeofday)))


;; Set end time.
(define (timer-end timer)
  (set-timer-end-time! timer (cons (gettimeofday)
                                   (timer-end-time timer))))
;; Get timer value (main).
(define (timer-value timer)
  (if (and (pair? (timer-start-time timer))
           (pair? (timer-end-time timer)))
      (to-pair (- (to-scalar (car (timer-end-time timer)))
                  (to-scalar (timer-start-time timer))))
      #f))


;; Get timer values.
(define (timer-values timer)
  (map (lambda (time)
         (to-pair (- (to-scalar time)
                     (to-scalar (timer-start-time timer)))))
       (reverse (timer-end-time timer))))


;; Create time and set start time.
(define (timer-open . rest)
  (make-timer (rest->name rest)
              (gettimeofday)
              '()))

;; Add timer time.
(define (timer-time timer)
  (timer-end timer))

;; CLose timer and return main time.
(define (timer-close timer)
  (timer-end timer)
  (timer-value timer))

;; Reduce multi-time timer to single-time timer.
(define (timer-reduce timer . rest)
  (if (pair? rest)
      (make-timer (rest->name rest)
                  (timer-start-time timer)
                  (list (car (timer-end-time timer))))
      (make-timer (timer-name timer)
                  (timer-start-time timer)
                  (list (car (timer-end-time timer))))))

;; Convert timer to timer value(s).
(define (timer->string timer)
  (define (val->str val)
    (format #f "~a.~a" (car val) (cdr val)))
  (if (= 1 (length (timer-end-time timer)))
      (val->str (timer-value timer))
      (map val->str
           (timer-values timer))))

(define (timer-display timer)
  (display (timer-name timer))
  (display ": ")
  (display (timer->value timer))
  (newline))
