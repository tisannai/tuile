;; -*-scheme-*-

;; See rllib.

(define-module (tuile heap)
;;   #:use-module (rllib base)
  #:use-module (tuile pr)
  #:use-module (tuile record-r6rs)
  #:use-module ((srfi srfi-43) #:select (vector-swap! vector-copy!))
  #:use-module ((tuile utils) #:select (vector-range vector-head->list))
  #:use-module (ice-9 match)
  #:export
  (
   heap-create
   heap-copy
   heap-root
   heap-insert
   heap-remove-top!
   heap-sort!
   heap-sort-list
   heap->list

   heap-compare
   heap-store
   heap-count
   ))


(define-record-type heap
  (fields compare                       ; Heap order type (max / min).
          (mutable store)               ; Heap storage.
          (mutable count)               ; Storage used count.
          ))


(define (up index) (1- (quotient (1+ index) 2)))
(define (left index) (1- (* 2 (1+ index))))
(define (right index) (* 2 (1+ index)))
(define (heap-ref heap index) (vector-ref (heap-store heap) index))
(define (heap-swap heap a b) (vector-swap! (heap-store heap) a b))

;;;
;;
;; 'heap-create' creates an heap object which can be used for building
;; the heap.
;;
;; params:
;;
;;     compare     Ordering classifier (string<? for ordered strings)
;;     .
;;     opt         Optional heap array size (default 128).
;;
(define (heap-create compare . opt)
  (let ((size (match opt
                ((size) size)
                (else 128))))
    (make-heap compare
               (make-vector size)
               0)))


;;;
;;
;; Return a copy of the heap (before sorting, for example)
;;
;; params:
;;
;;     heap        Heap object.
;;
(define (heap-copy heap)
  (make-heap (heap-compare heap)
             (vector-copy (heap-store heap))
             (heap-count heap)))

;;;
;;
;; Return heap root item
;;
;; params:
;;
;;     heap        Heap object.
;;
(define (heap-root heap)
  (vector-ref (heap-store heap) 0))


;; Heapify upwards (for insert).
(define (heap-sift-upwards heap index)
  (define compare (heap-compare heap))
  (define (value index) (heap-ref heap index))
  (define (swap a b) (heap-swap heap a b))

  (let lp ((i index))
    (let ((upper (up i)))
      (when (and (>= upper 0)
                 (compare (value upper)
                          (value i)))
        (swap i upper)
        (lp upper)))))


;; Heapify downwards (for sorting).
(define (heap-sift-downwards heap index)

  (define compare (heap-compare heap))
  (define (value index) (heap-ref heap index))
  (define (swap a b) (heap-swap heap a b))
  (define size (heap-count heap))

  (let lp ((i index))
    (let* ((l (left i))
           (r (right i))
           (iv (value i))
           (biggest #f))

      (when (and (< l size)
                 (compare iv
                          (value l)))
        (set! iv (value l))
        (set! biggest l))

      (when (and (< r size)
                 (compare iv
                          (value r)))
        (set! iv (value r))
        (set! biggest r))

      (when biggest
        (swap i biggest)
        (lp biggest)))))


;;;
;;
;; Add object to heap and heapify (upwards).
;;
;; params:
;;
;;     heap        Heap object.
;;     obj         Item to insert.
;;
(define (heap-insert heap obj)
  (heap-add heap obj)
  (heap-sift-upwards heap (1- (heap-count heap))))


;; Add object, but no heapify.
(define (heap-add heap obj)

  ;; Resize store, if needed.
  (when (>= (heap-count heap)
            (vector-length (heap-store heap)))
    (let ((new-store (make-vector (* 2 (vector-length (heap-store heap))))))
      (vector-copy! new-store 0 (heap-store heap))
      (heap-store-set! heap new-store)))

  ;; Store new object to last position in heap.
  (vector-set! (heap-store heap) (heap-count heap) obj)

  ;; Update used count.
  (heap-count-set! heap (1+ (heap-count heap))))


;;;
;;
;; Remove the root from heap and heapify.
;;
;; Return the removed root.
;;
;; params:
;;
;;     heap        Heap object.
;;
(define (heap-remove-top! heap)

  (define (last) (1- (heap-count heap)))

  (cond
   ((> (heap-count heap) 1)
    (begin
      (let ((biggest (heap-root heap)))
        (heap-swap heap 0 (last))
        (heap-count-set! heap (1- (heap-count heap)))
        (heap-sift-downwards heap 0)
        biggest)))
   ((= (heap-count heap) 1)
    (begin
      (let ((biggest (heap-root heap)))
        (heap-count-set! heap (1- (heap-count heap)))
        biggest)))
   (else #f)))

;;;
;;
;; Sort the heap (in place).
;;
;; Copy the heap before sorting, if you need the original heap.
;;
;; params:
;;
;;     heap        Heap object.
;;
(define (heap-sort! heap)
  (let ((org-count (heap-count heap)))
    (let lp ()
      (if (heap-remove-top! heap)
          (lp)
          (begin
            (heap-count-set! heap org-count)
            (vector-range (heap-store heap) 0 (heap-count heap)))))))


(define (heap-sort-list lst proc)
  (let ((h (heap-create 'sorting proc (length lst))))
    (for-each (lambda (item) (heap-insert h item)) lst)
    (heap-sort! h)
    (heap->list h)))



(define (heap->list heap)
  (vector-head->list (heap-store heap) (heap-count heap)))


;; (define h (heap-create 'max identity 12))
;; (for-each (lambda (i) (heap-insert h i)) (list 13 4 87 123 1321 21 2 145 1940))
;; ;; (for-each (lambda (i) (heap-insert h i)) (reverse (list 13 4 87 123 1321 21 2 145 1940)))
;; (ppr (vector-head->list (heap-store h) (heap-count h)))
;; (ppr (heap-sort! h))

;; (define h (heap-create 'max identity 4))
;; (for-each (lambda (i) (heap-insert h i)) (list 13 4 87 123))
;; (ppr (vector-head->list (heap-store h) (heap-count h)))
;; (ppr (heap-remove-top! h))
;; (ppr (heap-remove-top! h))
;; (ppr (heap-remove-top! h))
;; (ppr (heap-remove-top! h))
