(define-module (tuile timer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-43)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module ((ice-9 control) #:select (% abort))
  #:use-module ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
  #:use-module ((srfi srfi-19) #:prefix srfi:)
  #:use-module ((srfi srfi-88) #:select (string->keyword))
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
   ))


;;
;; Timer: Record timestamps from program run.
;;

;; ------------------------------------------------------------
;; Internal functions:

(define-record-type timer
  (make-timer start-time end-time)
  timer?
  (start-time timer-start-time set-timer-start-time!)
  (end-time   timer-end-time   set-timer-end-time!))

(define (to-scalar val)
  (+ (* 1000000 (car val))
     (cdr val)))

(define (to-pair val)
  (cons (quotient val 1000000)
        (remainder val 1000000)))


;; ------------------------------------------------------------
;; External functions:

;; Create timer record.
(define (timer-create)
  (make-timer #f '()))

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
(define (timer-open)
  (make-timer (gettimeofday)
              '()))

;; Add timer time.
(define (timer-time timer)
  (timer-end timer))

;; CLose timer and return main time.
(define (timer-close timer)
  (timer-end timer)
  (timer-value timer))

;; Reduce multi-time timer to single-time timer.
(define (timer-reduce timer)
  (make-timer (timer-start-time timer)
              (list (car (timer-end-time timer)))))

;; Convert timer to timer value(s).
(define (timer->string timer)
  (define (val->str val)
    (format #f "~a.~a" (car val) (cdr val)))
  (if (= 1 (length (timer-end-time timer)))
      (val->str (timer-value timer))
      (map val->str
           (timer-values timer))))
