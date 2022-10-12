(define-module (tuile oak)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (tuile pr)
  #:use-module (tuile log)
  #:use-module (tuile compatible)
  #:use-module (ice-9 match)
  #:export (
            oak-node-build
            oak-node-create
            oak-node-subs
            oak-node-data
            oak-node-data-set!
            oak-node-subs-set!
            oak-node-subs-add!

            oak-node-connected?

            oak-depth-first
            oak-breadth-first
            oak-find            

            oak-node-count
            oak-node-leafs

            oak-build

            oak-node-update-subs!
            oak-merge-by-pred

            oak-render

            oak-node-add-sub
            oak-node-root
            oak-node-find-by-pred
            oak-node-find-by-name

            oak-merge-by-name

            oak-render-by-name
            ))



;; ------------------------------------------------------------
;; Primitives:

;; Hierarchy node:
;; (list <data> <subs>)

(define (oak-node-build data subs)
  (cons data (apply list subs)))

(define (oak-node-create . opt-data)
  (let ((data (match opt-data
                ((data ...) (car data))
                (else #f))))
    (list data)))

(define (oak-node-data node)
  (car node))

(define (oak-node-subs node)
  (cdr node))

(define (oak-node-data-set! node data)
  (set-car! node data))

(define (oak-node-subs-set! node subs)
  (set-cdr! node subs))

(define (oak-node-subs-add! node sub)
  (set-cdr! node (append (oak-node-subs node) (list sub))))

(define (oak-node-connected? node)
  (pair? (cdr node)))

;; Primitives:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Visitors:

;; Perform depth-first-search through node oak.
;;
;;     node    Tree node.
;;     arg     Argument for "ifn" and "ofn"
;;     ifn     Node input (enter) function with two args (node arg), if any.
;;     ofn     Node output (exit) function with two args (node arg), if any.
;;
(define (oak-depth-first node arg ifn ofn)
  (when ifn
    (apply ifn (list node arg)))
  (for-each (lambda (node)
              (oak-depth-first node
                               arg
                               ifn
                               ofn))
            (oak-node-subs node))
  (when ofn
    (apply ofn (list node arg))))


;; Perform breadth-first-search through node oak.
;;
;;     node    Tree node.
;;     arg     Argument for "ifn" and "ofn"
;;     ifn     Node input (enter) function with two args (node arg), if any.
;;     ofn     Node output (exit) function with two args (node arg), if any.
;;
(define (oak-breadth-first node arg ifn ofn)

  (define (travel-level level-list arg ifn ofn)

    (when (pair? level-list)

      ;; Apply input function.
      (for-each (lambda (node)
                  (when ifn
                    (apply ifn (list node arg))))
                level-list)

      ;; Continue with nodes that have
      (travel-level (concatenate (map oak-node-subs
                                      level-list))
                    arg
                    ifn
                    ofn)

    (when ofn
      (apply ofn (list node arg)))))

  (travel-level (list node) arg ifn ofn))


;; Find node from the tree using predicate function ("pred"). Return
;; "unspecified" if match is not found.
;;
;;     node    Tree node.
;;     pred    Find function with "node" as parameter.
;;
(define (oak-find node pred)
  (call/cc
   (lambda (exit-cc)
     (let ((exit-prog (lambda (node arg)
                        ;; "pred" in car position.
                        (when ((car arg) node)
                          ;; "exit-cc" in cdr position.
                          ((cdr arg) node)))))
       (oak-depth-first node
                        ;; The "arg".
                        (cons pred exit-cc)
                        ;; Use exit-prog as ifn.
                        exit-prog
                        ;; No ofn.
                        #f))
     ;; Nothing was found.
     *unspecified*)))


;; Visitors:
;; ------------------------------------------------------------



;; ------------------------------------------------------------
;; Queries:

;; Return node count for oak.
(define (oak-node-count node)
  (let* ((count 0)
         (counter (lambda (node)
                    (set! count (+ count 1)))))
    (oak-depth-first node #f counter #f)
    count))


;; Return all leaf nodes.
(define (oak-node-leafs node)
  (let ((leafs '()))
    (let ((collect-leaf (lambda (node arg)
                          (when (not (oak-node-connected? node))
                            (set! leafs (cons node leafs))))))
      (oak-depth-first node #f collect-leaf #f))
    (reverse leafs)))


;; Queries:
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; Builders:


(define (oak-build node data-fn subs-fn)
  (if (pair? (subs-fn node))
      (oak-node-build (data-fn node)
                      (map (lambda (sub) (oak-build sub data-fn subs-fn))
                           (subs-fn node)))
      (oak-node-create (data-fn node))))


;; Builders:
;; ------------------------------------------------------------

;; ------------------------------------------------------------
;; Mutators:

;; Update current node subs by new.
(define (oak-node-update-subs! node subs)
  (oak-node-subs-set! node subs)
  #;
  (for-each (lambda (sub)
              (oak-node-host-set! sub node))
            subs)
  node)

;; Merge oaks and return new root.
;;
;; Merge is performed by t2 to t1 or t1 to t2, depending on which oak
;; has a reference to the root of the other.
;;
(define (oak-merge-by-pred t1 t2 p1 p2)

  (let* ((t1-leaf-host (oak-node-find-by-pred t1 p1))
         (t2-leaf-host (oak-node-find-by-pred t2 p2)))
    (cond
     (t1-leaf-host
      (oak-node-update-subs! t1-leaf-host (oak-node-subs t2))
      t1)
     (t2-leaf-host
      (oak-node-update-subs! t2-leaf-host (oak-node-subs t1))
      t2)
     (else
      #f))))

;; Mutators:
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Renderers:

;; Render node hierarchy as left-to-right oak.
;;
;; 0000000001111111111222222222233333333334
;; 1234567890123456789012345678901234567890
;;
;;     root---+----b1-1
;;            |
;;            +----b1-2---+---b1-2-1
;;            |           |
;;            |           +---b1-2-2
;;            |
;;            +----b1-3
;;
;; Render hierarchy with "(proc node)" as label selector.
;;
(define (oak-render node proc indent gap)

  (define line "")
  (define lines '())

  (define (prp . args)
    (set! line (string-append line (apply ss args))))

  (define (newline)
    (set! lines (append lines (list line)))
    (set! line ""))

  (define (render-indented-bar levels)
    (prp (string-join (map (lambda (level)
                             (make-string (1- level) #\ ))
                           (drop-right levels 1))
                      "|")))

  (define (node-name-len node)
    (string-length (proc node)))

  (define (this-level-width nodes)
    (let ((width (+ (* 2 indent)
                    (apply max (map node-name-len nodes)))))
      width))

  (define (render-level nodes
                        first
                        indent
                        gap
                        levels)

    (for-each (lambda (node)

                (let* ((l-dash-pad indent)
                       (r-dash-pad (- (last levels) indent (node-name-len node) 1)))

                  ;; Render bars for vertical gapping.
                  (when (not first)
                    (let loop-gaps ((i 0))
                      (when (< i gap)
                        (render-indented-bar levels)
                        (prp "|")
                        (newline)
                        (loop-gaps (1+ i))))
                    (render-indented-bar levels)
                    (prp "+" (make-string l-dash-pad #\-) (proc node)))

                  ;; Render level first with spaces (for root) and with "+" for all
                  ;; other first in level.
                  (when first
                    (if (pair? (cdr levels))
                        (prp "+" (make-string l-dash-pad #\-) (proc node))
                        (prp (make-string l-dash-pad #\ ) (proc node))))

                  (if (oak-node-connected? node)
                      (begin
                        (when (> r-dash-pad 0)
                          (prp (make-string r-dash-pad #\-)))
                        (let ((new-width (this-level-width (oak-node-subs node))))
                          (render-level (oak-node-subs node)
                                        #t
                                        indent
                                        gap
                                        (append levels (list new-width)))))
                      (newline))

                  (set! first #f)))

              nodes))

  (render-level (list node)
                #t
                indent
                gap
                (list (this-level-width (list node))))
  lines)


;;
;;
;;     root
;;         b1-1
;;             b1-1-1
;;         b1-2
;;
(define* (oak-render-clean node proc #:key (indent-step 4))
  (define step (if (>= indent-step 1) indent-step 1))
  (define (render-level node level)
    (if (oak-node-connected? node)
        (cons (ss (:in (* level step)) (proc node))
              (apply append (map (lambda (sub) (render-level sub (1+ level)))
                                 (oak-node-subs node))))
        (list (ss (:in (* level step)) (proc node)))))
  (render-level node 1))


;;
;;
;;     root
;;     |
;;     + -- b1-1
;;     |    |
;;     |    + -- b1-1-1
;;     |
;;     + -- b1-2
;;
(define* (oak-render-compact node proc #:key (indent-step 5))
  (define step (if (>= indent-step 4) indent-step 4))
  (define (render-level node level)
    (let ((leader (ss (:in step) (apply ss (make-list (1- level) (ss "|" (:in (1- step))))))))
      (cons (ss leader "|")
            (let ((leader (ss leader (ss "+ " (make-string (- step 3) #\-) " "))))
              (if (oak-node-connected? node)
                  (cons (ss leader (proc node))
                        (apply append (map (lambda (sub) (render-level sub (1+ level)))
                                           (oak-node-subs node))))
                  (list (ss leader (proc node))))))))
  (cons (ss (:in step) (proc node))
        (apply append (map (lambda (node) (render-level node 1))
                           (oak-node-subs node)))))

;; Renderers:
;; ------------------------------------------------------------


;;(define root (oak-node-create "root"))
;;(define b1-1 (oak-node-create "b1-1"))
;;(define b1-2 (oak-node-create "b1-2"))
;;(define b1-1-1 (oak-node-create "b1-1-1"))
;;
;;(oak-node-subs-add! root b1-1)
;;(oak-node-subs-add! root b1-2)
;;(oak-node-subs-add! b1-1 b1-1-1)
;;(pd (oak-node-data root))
;;
;;(pl (oak-render root oak-node-data 4 1))
;;(pl (oak-render-clean root oak-node-data))
;;(pl (oak-render-compact root oak-node-data))


;;(pl (oak-render-compact (oak-build (list "root" (list "b1-1" "b1-1-1") "b1-2")
;;                                   (lambda (node) (if (pair? node) (car node) node))
;;                                   (lambda (node) (if (pair? node) (cdr node) '())))
;;                        oak-node-data))
