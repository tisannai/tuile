(define-module (tuile tree)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (tuile pr)
  #:use-module (tuile log)
  #:use-module (tuile compatible)
  #:export (
            tree-node-create
            tree-node-create-with-name
            tree-node
            tree-node-name
            tree-node-host
            tree-node-subs
            tree-node-data
            tree-node-set-host
            tree-node-name-set!
            tree-node-host-set!
            tree-node-subs-set!
            tree-node-data-set!

            tree-node-add-sub
            tree-node-root
            tree-node-has-subs?
            tree-node-connected?
            tree-node-count
            tree-node-leafs
            tree-node-update-subs
            tree-node-find-by-pred
            tree-node-find-by-name

            tree-depth-first
            tree-breadth-first
            tree-merge-by-pred
            tree-merge-by-name

            tree-render
            tree-render-by-name
            ))


;; Hierarchy node:
(define-record-type tree-node
  (fields
   (mutable name)                       ; Name.
   (mutable host)                       ; Host node (parent).
   (mutable subs)                       ; List of sub-nodes.
   (mutable data)                       ; External data.
   )) 


(define tree-node-create make-tree-node)

;; Make node with given name, others are defaults.
(define (tree-node-create-with-name name)
  (tree-node-create name #f '() #f))


(define (tree-node-set-host node host)
  (tree-node-host-set! node host))

;; Add sub-node to node (host).
(define (tree-node-add-sub node sub)
  (tree-node-subs-set! node
                       (append (tree-node-subs node)
                               (list sub)))
  (tree-node-host-set! sub node)
  node)


;; Find hierarcy root, from given node.
(define (tree-node-root node)
  (if (tree-node-host node)
      (tree-node-root (tree-node-host node))
      node))


;; Does node have subs?
(define (tree-node-has-subs? node)
  (pair? (tree-node-subs node)))


;; Is node connected?
(define (tree-node-connected? node)
  (or (tree-node-host node)
      (tree-node-has-subs? node)))


;; Return node count for tree.
(define (tree-node-count node)
  (let* ((count 0)
         (counter (lambda (node)
                    (set! count (+ count 1)))))
    (tree-depth-first node #f counter #f)
    count))


;; Return all leaf nodes.
(define (tree-node-leafs node)
  (let ((leafs '()))
    (let ((collect-leaf (lambda (node arg)
                          (when (not (tree-node-has-subs? node))
                            (set! leafs (cons node leafs))))))
      (tree-depth-first node #f collect-leaf #f))
    (reverse leafs)))


;; Update current node subs by new.
(define (tree-node-update-subs node subs)
  (tree-node-subs-set! node subs)
  (for-each (lambda (sub)
              (tree-node-host-set! sub node))
            subs)
  node)


;; Find node by pred.
;;
;; Pred is a one argument proc that returns node if find predicate is
;; a success.
(define (tree-node-find-by-pred node pred)
  (call/cc
   (lambda (exit-cc)
     (let ((exit-prog (lambda (node arg)
                        (when ((car arg) node)
                          ((cdr arg) node)))))
       (tree-depth-first node
                         (cons pred exit-cc)
                         exit-prog
                         #f))
     #f)))


;; Find node by name.
(define (tree-node-find-by-name node name)
  (let ((pred (lambda (node)
                (string=? name
                          (tree-node-name node)))))
    (tree-node-find-by-pred node pred)))


;; Perform depth-first-search through node tree.
;;
;; ifn : node input (enter) function with two args (node arg), if any.
;; ofn : node output (exit) function with two args (node arg), if any.
(define (tree-depth-first node arg ifn ofn)
  (when ifn
    (apply ifn (list node arg)))
  (for-each (lambda (node)
              (tree-depth-first node
                                     arg
                                     ifn
                                     ofn))
            (tree-node-subs node))
  (when ofn
    (apply ofn (list node arg))))


#;
(tree-depth-first hn
                       #f
                       (lambda (node arg)
                         (format #t "Node name: ~a\n" (tree-node-name node)))
                       #f)


;; Perform breadth-first-search through node tree.
;;
;; ifn : node input (enter) function with two args (node arg), if any.
;; ofn : node output (exit) function with two args (node arg), if any.
(define (tree-breadth-first node arg ifn ofn)

  (define (travel-level level-list arg ifn ofn)

    (when (pair? level-list)

      ;; Apply input function.
      (for-each (lambda (node)
                  (when ifn
                    (apply ifn (list node arg))))
                level-list)

      ;; Continue with nodes that have
      (travel-level (concatenate (map tree-node-subs
                                      level-list))
                    arg
                    ifn
                    ofn)

    (when ofn
      (apply ofn (list node arg)))))

  (travel-level (list node) arg ifn ofn))


#;
(tree-breadth-first hn
                       #f
                       (lambda (node arg)
                         (format #t "Node name: ~a\n" (tree-node-name node)))
                       #f)


;; Merge trees and return new root.
;;
;; Merge is performed by t2 to t1 or t1 to t2, depending on which tree
;; has a reference to the root of the other.
;;
(define (tree-merge-by-pred t1 t2 p1 p2)

  (let* ((t1-leaf-host (tree-node-find-by-pred t1 p1))
         (t2-leaf-host (tree-node-find-by-pred t2 p2)))
    (cond
     (t1-leaf-host
      (tree-node-update-subs t1-leaf-host (tree-node-subs t2))
      t1)
     (t2-leaf-host
      (tree-node-update-subs t2-leaf-host (tree-node-subs t1))
      t2)
     (else
      #f))))


;; Merge trees and return new root.
;;
;; Merge is performed by t2 to t1 or t1 to t2, depending on which tree
;; has a reference to the root of the other.
;;
(define (tree-merge-by-name t1 t2)

  (let* ((t1-leaf-host (tree-node-find-by-name t1 (tree-node-name t2)))
         (t2-leaf-host (tree-node-find-by-name t2 (tree-node-name t1))))
    (cond
     (t1-leaf-host
      (tree-node-update-subs t1-leaf-host (tree-node-subs t2))
      t1)
     (t2-leaf-host
      (tree-node-update-subs t2-leaf-host (tree-node-subs t1))
      t2)
     (else
      #f))))



;; Render node hierarchy as left-to-right tree.
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
(define (tree-render node indent gap proc)

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

                  (if (tree-node-has-subs? node)
                      (begin
                        (when (> r-dash-pad 0)
                          (prp (make-string r-dash-pad #\-)))
                        (let ((new-width (this-level-width (tree-node-subs node))))
                          (render-level (tree-node-subs node)
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

;; Render hierarchy using node name as label.
(define (tree-render-by-name node indent gap)
  (tree-render node indent gap tree-node-name))


;;(define hn (tree-node-create-with-name "root"))
;;(define b1-1 (tree-node-create-with-name "b1-1"))
;;(define b1-2 (tree-node-create-with-name "b1-2"))
;;(define b1-3 (tree-node-create-with-name "b1-3"))
;;
;;(define b1-1-1 (tree-node-create-with-name "b1-1-1"))
;;(define b1-1-2 (tree-node-create-with-name "b1-1-2"))
;;(define b1-1-2-1 (tree-node-create-with-name "b1-1-2-1"))
;;(define b1-1-3 (tree-node-create-with-name "b1-1-3"))
;;(define b1-2-1 (tree-node-create-with-name "b1-2-1"))
;;(define b1-2-2 (tree-node-create-with-name "b1-2-2"))
;;
;;(tree-node-add-sub hn b1-1)
;;(tree-node-add-sub hn b1-2)
;;(tree-node-add-sub hn b1-3)
;;
;;(tree-node-add-sub b1-1 b1-1-1)
;;(tree-node-add-sub b1-1 b1-1-2)
;;(tree-node-add-sub b1-1-2 b1-1-2-1)
;;
;;(tree-node-add-sub b1-1 b1-1-3)
;;(tree-node-add-sub b1-2 b1-2-1)
;;(tree-node-add-sub b1-2 b1-2-2)
;;
;;(pl (tree-render-by-name hn 4 1))


;;(define hn (tree-node-create-with-name "root"))
;;(define b1-1 (tree-node-create-with-name "b1-1"))
;;(define b1-2 (tree-node-create-with-name "b1-2"))
;;(define b1-3 (tree-node-create-with-name "b1-3"))
;;
;;(tree-node-add-sub hn b1-1)
;;(tree-node-add-sub hn b1-2)
;;(tree-node-add-sub hn b1-3)
;;
;;(define b1-1 (tree-node-create-with-name "b1-1"))
;;(define b1-1-1 (tree-node-create-with-name "b1-1-1"))
;;(define b1-1-2 (tree-node-create-with-name "b1-1-2"))
;;(define b1-1-2-1 (tree-node-create-with-name "b1-1-2-1"))
;;(tree-node-add-sub b1-1 b1-1-1)
;;(tree-node-add-sub b1-1 b1-1-2)
;;(tree-node-add-sub b1-1-2 b1-1-2-1)
;;
;;(tree-merge-by-name hn b1-1)
;;
;;(tree-render-by-name hn 4 1)
;;
;;(pr "length: " (length (tree-node-leafs hn)))
