;; Module for Graph (DAG) based operations.

(define-module (tuile graph)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module ((srfi srfi-1) #:select (first second))
  #:use-module (srfi srfi-111)
  #:use-module (tuile pr)
  #:use-module (tuile utils)
  #:use-module (tuile oop)
;;   #:use-module (tuile record)
  #:use-module (tuile record-r6rs)

  #:export
  (
   <graph>
   graph-add-node
   graph-first-node
   graph-ref
   graph-add-node-with-data
   graph-connect-nodes
   graph-connect-nodes-with-data
   graph-each-node
   graph-unvisit-all-nodes

   node-name
   node-olink
   node-data
   node-new
   node-new-with-data
   node-visit
   node-unvisit
   node-visited?
   edge-node
   edge-data

   dfs-common
   dfs
   dfs-with-env

   bfs-common
   bfs
   bfs-with-env

   topological-sort
   shortest-path-between-nodes
   longest-path-between-nodes
   ))



;; ------------------------------------------------------------
;; Helpers:

(define empty '())


;; ------------------------------------------------------------
;; Nodes and edges:

;; Edge definition.
;; (define-mu-record edge
;;   node       ; Associated node
;;   data       ; Edge value
;;   )

(define-record-type edge
  (fields
   (mutable node)                       ; Associated node
   (mutable data)                       ; Edge value
   ))

;; Node definition.
;;
;; Data is collection of user data.
;; (define-mu-record node
;;   name       ; Node name
;;   visited    ; Node visited (support for algos)
;;   ilink      ; Links left:  ( (<node> value) ... )
;;   olink      ; Links right: ( (<node> value) ... )
;;   data       ; Generic data storage.
;;   )

(define-record-type node
  (fields
   (mutable name)                          ; Node name
   (mutable visited)                       ; Node visited (support for algos)
   (mutable ilink)                         ; Links left:  ( (<node> value) ... )
   (mutable olink)                         ; Links right: ( (<node> value) ... )
   (mutable data)                          ; Generic data storage.
   ))

;; Create new node with name.
(define (node-new name)
  (make-node name #f empty empty #f))

;; Create new node with name and data.
(define (node-new-with-data name data)
  (make-node name #f empty empty data))

;; Visit node.
(define (node-visit node)
  (node-visited-set! node #t))

;; Unvisit node.
(define (node-unvisit node)
  (node-visited-set! node #f))

;; Return visit status.
(define (node-visited? node)
  (node-visited node))

;; Unvisit all nodes.
(define (node-unvisit-all nodes)
  (for ((node nodes))
    (node-unvisit node)))


;; ------------------------------------------------------------
;; Graph:


(define-class <graph> ()
  (:name      #:init-keyword #:name
              #:init-form #f)
  (:node-list #:init-keyword #:node-list
              #:init-form '())
  (:node-hash #:init-keyword #:node-hash
              #:init-form (make-hash-table))
  )

(define-this-method <graph> (graph-add-node id . rest)
  (for ((id (append (list id)
                    rest)))
    (let ((node (node-new id)))
      (this-set! :node-list (append (this-ref :node-list) (list node)))
      (hash-set! (this-ref :node-hash) id node))))

(define-this-method <graph> (graph-first-node)
  (car (this-ref :node-list)))

(define-this-method <graph> (graph-ref id)
  (hash-ref (this-ref :node-hash) id))

(define-this-method <graph> (graph-add-node-with-data id data . rest)
  (let each ((spec (append (list id data)
                           rest)))
    (when (pair? spec)
      (let ((node (node-new-with-data (car spec)
                                      (cadr spec))))
        (this-set! :node-list (append (this-ref :node-list) (list node)))
        (hash-set! (this-ref :node-hash) (car spec) node))
      (each (cddr spec)))))

(define-this-method <graph> (graph-connect-nodes a b . rest)
  (let each ((pair (append (list a b) rest)))
    (when (pair? pair)
      (let ((a (graph-ref this (first pair)))
            (b (graph-ref this (second pair))))
        (node-olink-set! a (append (node-olink a) (list (make-edge b 0))))
        (node-ilink-set! b (append (node-ilink b) (list (make-edge a 0)))))
      (each (cddr pair)))))

(define-this-method <graph> (graph-connect-nodes-with-data a b data . rest)
  (let each ((pair (append (list a b data) rest)))
    (when (pair? pair)
      (let ((a (graph-ref this (car pair)))
            (b (graph-ref this (cadr pair)))
            (data (caddr pair)))
        (node-olink-set! a (append (node-olink a) (list (make-edge b data))))
        (node-ilink-set! b (append (node-ilink b) (list (make-edge a data)))))
      (each (cdddr pair)))))

(define-this-method <graph> (graph-each-node fn)
  (for ((node (this-ref :node-list)))
    (apply fn (list node))))

(define-this-method <graph> (graph-unvisit-all-nodes)
  (graph-each-node
   (lambda (node)
     (node-unvisit node))))



;; ------------------------------------------------------------
;; Depth First Search:

;; Depth first travel of graph with starting node as "node". "ifn" is
;; executed at node entry and "ofn" is executed at node
;; exit. Functions can return 'exit, 'stop, or 'continue as orders
;; (commands) for defining the next steps.
;;
;; Args:
;;     node: Node.
;;     ifn:  Node input action.
;;     ofn:  Node output action.
;;     env:  Action environment.
;;
;; Example:
;;     (dfs-common node (lambda (node env) (displayln (node-name node))) #f #f)
;;
(define (dfs-common node ifn ofn env)
  (when node

    (let ((visited empty))

      (define (visit node)
        (node-visit node)
        (set! visited (cons node visited)))

      (call/cc
       (lambda (exit-cc)

         (define (dfs+ exit-cc node ifn ofn env)
           (call/cc
            (lambda (stop-cc)

              (when ifn
                (case (ifn node env)
                  ((exit) (exit-cc #f))
                  ((stop) (stop-cc #f))))

              (let each-olink ((olink (node-olink node)))
                (when (pair? olink)
                  (when (not (node-visited? (edge-node (car olink))))
                    (dfs+ exit-cc (edge-node (car olink)) ifn ofn env))
                  (visit (edge-node (car olink)))
                  (each-olink (cdr olink))))

              (when ofn
                (case (ofn node env)
                  ((exit) (exit-cc #f))
                  ((stop) (stop-cc #f)))))))


         (dfs+ exit-cc node ifn ofn env)))

      (node-unvisit-all visited))))


;; Depth-first-search from "node" with action "fn" on input.
(define (dfs node fn)
  (dfs-common node fn #f #f))


;; Depth-first-search from "node" with action "fn" on input with
;; "env".
(define (dfs-with-env node fn env)
  (dfs-common node fn #f env))


;; ------------------------------------------------------------
;; Breath First Search:

;; Breath first travel of graph with starting node as "node".
;;
;; Args:
;;     node: Node.
;;     ifn:  Node input action.
;;     ofn:  Node output action.
;;     env:  Action environment.
;;
;; Example:
;;     (bfs-common node (lambda (node) (displayln (node-name node))) #f #f)
;;
(define (bfs-common node ifn ofn env)
  (when node

    (let ((visited empty))

      (define (visit node)
        (node-visit node)
        (set! visited (cons node visited)))

      (call/cc
       (lambda (exit-cc)

         ;; Process nodes and build list of next-nodes for the next
         ;; round of bfs+.
         (define (bfs+ exit-cc nodes ifn ofn env)
           (let ((next-nodes empty))

             (let each-node ((nodes nodes))
               (when (pair? nodes)

                 ;; Process all output links.
                 (let each-olink ((olink (node-olink (car nodes))))
                   (when (pair? olink)
                     (let ((probe-node (edge-node (car olink))))
                       (when (not (node-visited? probe-node))
                         (visit probe-node)
                         (call/cc
                          (lambda (stop-cc)

                            (when ifn
                              (case (ifn probe-node env)
                                ((exit) (exit-cc #f))
                                ((stop) (stop-cc #f))))

                            (set! next-nodes
                              (append next-nodes (list probe-node)))))))

                     (each-olink (cdr olink))))

                 (when ofn
                   (case (ofn (car nodes) env)
                     ((exit) (exit-cc #f))))

                 (each-node (cdr nodes))))

             (when (pair? next-nodes)
               (bfs+ exit-cc next-nodes ifn ofn env))))

         ;; Visit first node as special case.
         (when ifn
           (case (ifn node env)
             ((exit stop) (exit-cc #f))))

         (visit node)

         (bfs+ exit-cc (list node) ifn ofn env)))

      ;; Clean-up before returning.
      (node-unvisit-all visited))))


;; Breath-first-search from "node" with action "fn" on input.
(define (bfs node fn)
  (bfs-common node fn #f #f))


;; Breath-first-search from "node" with action "fn" on input with
;; "env".
(define (bfs-with-env node fn env)
  (bfs-common node fn #f env))



;; ------------------------------------------------------------
;; Topological sort:

(define (topological-sort node)

  ;; Input function.
  (define (topo-sort-ifn node env)
    (if (not (node-visited? node))
        (begin
          (node-visit node)
          'continue)
        'stop))

  ;; Output function.
  (define (topo-sort-ofn node env)
    (set-box! env (cons node (unbox env))))

  ;; DFS based ordering using stack.
  (let ((ordered (box empty)))
    (dfs-common node topo-sort-ifn topo-sort-ofn ordered)
    (for ((node (unbox ordered)))
      (node-unvisit node))
    (unbox ordered)))


;; ------------------------------------------------------------
;; Shortest and longest path:

;; Return shortest or longest path depending on "mode". Edge values
;; define the distance.
;;
;; Return list of (node . length) pairs looking from "a".
(define (shortest-or-longest-path-common a mode) ; INTERNAL

  (let ((inf #f)
        (compare #f))

    (case mode
      ((shortest-path)
       (set! inf +inf.0)
       (set! compare >))
      ((longest-path)
       (set! inf -inf.0)
       (set! compare <)))

    (define (get-distance node)
      (unbox (car (node-data node))))

    (define (set-distance! node val)
      (set-box! (car (node-data node)) val))

    ;; Mark distances for sorted list of nodes.
    (let ((sorted-nodes (topological-sort a)))

      ;; Set all nodes to infinity.
      (for ((node sorted-nodes))
        (node-data-set! node (cons (box inf) (node-data node))))
      (set-distance! a 0)

      (let each-node ((sorted sorted-nodes))
        (when (pair? sorted)
          (let each-olink ((olink (node-olink (car sorted))))
            (when (pair? olink)
              (let ((node (edge-node (car olink)))
                    (new-value (+ (get-distance (car sorted)) (edge-data (car olink)))))
                (when (compare (get-distance node) new-value)
                  (set-distance! node new-value)))
              (each-olink (cdr olink))))
          (each-node (cdr sorted))))

      (let ((dist (map
                   (lambda (i)
                     (cons i (get-distance i)))
                   sorted-nodes)))
        (for ((node sorted-nodes))
          (node-data-set! node (cdr (node-data node))))
        dist))))


;; Return shortest distance between nodes "a" and "b". Edge values
;; define the distance.
(define (shortest-path-between-nodes a b)
  (let ((dist (shortest-or-longest-path-common a 'shortest-path)))
    (cdr (find-first (lambda (i)
                       (eqv? b
                             (car i)))
                     dist))))


;; Return longest distance between nodes "a" and "b". Edge values
;; define the distance.
(define (longest-path-between-nodes a b)
  (let ((dist (shortest-or-longest-path-common a 'longest-path)))
    (cdr (find-first (lambda (i)
                       (eqv? b (car i)))
                     dist))))
