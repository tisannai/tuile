;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2025
;;;; All rights reserved.


;;; module:
;;
;; 'hdl-in-memory' contains features for representing synthesizable
;; subset of HDL (Hardware Description Language) as a data structure
;; and features for transforming the format to (primarily) Verilog.
;;
;; The most abstract format is a full AST of the HDL. Each element is
;; prefixed with type information followed by the element data. For
;; example:
;;
;;     (stmt-if ((varref "init") (stmt-nas (strexpr "count")
;;                                         (strexpr "count + 4")))
;;              (#f (stmt-nas (strexpr "count")
;;                            (strexpr "count + 1"))))
;;
;; has 'stmt-if' as the top level element and the corresponding
;; Verilog output would be:
;;
;;           ...
;;           if ( init ) begin
;;              count <= count + 4;
;;           end else begin
;;              count <= count + 1;
;;           end
;;           ...
;;
;; The structure for 'stmt-if' is:
;;
;;     (stmt-if (<if-condition>|#f <stmt> ...) ...)
;;
;; and the structure for 'stmt-nas' is:
;;
;;     (stmt-nas <lvalue> <rvalue-expr>)
;;
;; In other words, if-statement has prefix 'stmt-if' followed by a
;; list of pairs. Each pair has condition for taking the corresponding
;; if-branch and the tail contains all the statements to execute for
;; true condition. If the condition is literal '#f', the pair
;; represents the 'else' branch.
;;
;; 'varref' is variable reference. 'stmt-nas' represents non-blocking
;; assignment. 'strexpr' elements are literal target strings, and they
;; are used as is. If multiple language outputs (Verilog and VHDL, for
;; example) are targeted, then the output must be described as
;; detailed AST and 'strexpr' elements are not allowed. Note that in
;; this particular case, both Verilog and VHDL would be possible.
;;
;; User can prepare the AST as complete structure in one go, or it can
;; be build using the 'create' API. 'create' API allows the user to
;; build the AST piece-by-piece. Each addition to the AST is labeled
;; with the structure type and this allows 'create' API to place the
;; addition to the correct place in the AST.
;;
(define-module (tuile hdl-in-memory)
;;   #:use-module (rllib base)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (tuile basic)
  #:use-module (tuile vlog-number)
  #:use-module (tuile issues)
  #:use-module (tuile pr)
  #:use-module (tuile fmt)
  #:use-module (tuile re)
  #:export (
            him-render
            him-render-name

            him-numstr
            him-numint
            him-bitdef
            him-bitvec
            him-hexvec
            him-decvec
            him-template
            him-create

            him-number-value
            him-number-sign
            him-number-width
            him-number-base
            him-number->vlog
            him-number-create
            him-number-from-int
            him-number-set-value
            him-number?

            him-name
            him-header
            him-params
            him-locals
            him-clock
            him-reset
            him-iports
            him-oports
            him-zports
            him-vars
            him-procs

            him-clock-def
            him-reset-def
            him-iport-defs
            him-oport-defs
            him-zport-defs
            him-input-defs
            him-port-defs

            him-vardef-vtype
            him-vardef-valdef
            him-vardef-width
            him-vardef-sign
            him-vardef-value
            him-vardef-depth

            him-port-def

            him-port-name
            him-port-dir
            him-port-type
            him-port-width
            him-port-sign
            him-port-value
            him-port-depth

            him-resolve-params
            him-resolve
            him-complete
            him-separate-clkrst
            him-swap-port-direction
            him-oports-as-regs
            him-to-monitor
            ))


;; TODO : Replace these "local" funcs with library funcs.

(define (list-flat lst)
  (let loop ((lst lst)
             (sub #f)
             (ret (list)))
    (if (pair? lst)
        (cond (sub
               (if (pair? sub)
                   (loop lst
                         (cdr sub)
                         (if (unspecified? (car sub))
                             ret
                             (cons (car sub) ret)))
                   (loop (cdr lst)
                         #f
                         ret)))
              ((pair? (car lst))
               (loop lst
                     (car lst)
                     ret))
              (else
               (loop (cdr lst)
                     #f
                     (if (unspecified? (car lst))
                         ret
                         (cons (car lst) ret)))))
        (reverse ret))))

(define (make-list-flat . elems)
  (list-flat elems))

(define (missing->unspecified value)
  (if (or (not value) (null? value))
      *unspecified*
      value))

(define (assoc-hier keys alist)
  (let lp ((alist (cons 'dummy alist))
           (keys keys))
    (if (and (pair? alist)
             (pair? keys))
        (aif (assoc (car keys) (cdr alist))
             (lp it (cdr keys))
             #f)
        alist)))

(define (assoc-copy alist)
  (map (lambda (keva) (cons (car keva) (cdr keva)))
       alist))

(define (assoc-set-value! alist key value)
  (if (list? key)
      (begin
        ;; (ppre alist)
        (set-cdr! (assoc-hier key alist) value))
      (set-cdr! (assoc key alist) value)))

;; Perform "map" and also inform the "proc" about the item position
;; with 'head, 'middle, 'tail. Tail is used for a list with only one
;; item.
(define (map-with-position proc lst)
  (if (null? lst)
      '()
      (let lp ((lst lst)
               (head #t)
               (ret '()))
        (if (pair? lst)
            (lp (cdr lst)
                #f
                (cons (proc (car lst) (cond
                                       ((null? (cdr lst)) 'tail)
                                       (head 'head)
                                       (else 'middle)))
                      ret))
            (reverse ret)))))

;; Perform "map" and also inform the "proc" about current index.
(define (map-with-index proc lst)
  (if (null? lst)
      '()
      (let lp ((lst lst)
               (i 0)
               (ret '()))
        (if (pair? lst)
            (lp (cdr lst)
                (1+ i)
                (cons (proc (car lst) i)
                      ret))
            (reverse ret)))))


;; him-number:
;;
;;     (12 unsigned 4 2)
;;
(define him-number-value first)
(define him-number-sign  second)
(define him-number-width third)
(define him-number-base  fourth)
(define (him-number->vlog him-number)
  (vlog-number-to-quoted-numstr (vlog-number-from-him him-number) #t))
(define him-number-create list)
;; (define (him-number-from-int num)
;;   (him-number-create num #f #f 10))
(define (him-number-set-value num val)
  (him-number-create val
                     (him-number-sign num)
                     (him-number-width num)
                     (him-number-base num)))
(define (him-number? value)
  (and (pair? value)
       (= (length value) 4)))

(define (valid? item) (and (not (unspecified? item))
                           (not (null? item))))

(define (invalid? item) (not (valid? item)))

(define (assoc-ref-single alist tag)
  (let ((val (assoc-ref alist tag)))
    (if (pair? val)
        (car val)
        *unspecified*)))

(define (him-name him) (assoc-ref-single him 'name))
(define (him-header him) (assoc-ref him 'header))
(define (him-params him) (assoc-ref him 'params))
(define (him-locals him) (assoc-ref him 'locals))
(define (him-clock him) (assoc-ref-single him 'clock))
(define (him-reset him) (assoc-ref-single him 'reset))
(define (him-iports him) (assoc-ref him 'iports))
(define (him-oports him) (assoc-ref him 'oports))
(define (him-zports him) (assoc-ref him 'zports))
(define (him-vars him) (assoc-ref him 'vars))
(define (him-procs him) (assoc-ref him 'procs))

(define (him-clock-def him)
  (if (valid? (him-clock him))
      (him-port-def him (him-clock him) 'clock)
      *unspecified*))

(define (him-reset-def him)
  (if (valid? (him-reset him))
      (him-port-def him (him-reset him) 'reset)
      *unspecified*))

(define (him-iport-defs him)
  (map (lambda (name) (him-port-def him name 'iport)) (him-iports him)))

(define (him-oport-defs him)
  (map (lambda (name) (him-port-def him name 'oport)) (him-oports him)))

(define (him-zport-defs him)
  (map (lambda (name) (him-port-def him name 'zport)) (him-zports him)))

(define (him-input-defs him)
  (append (awhen (him-clock-def him) (if (valid? it) (list it) '()))
          (awhen (him-reset-def him) (if (valid? it) (list it) '()))
          (him-iport-defs him)))

(define (him-port-defs him)
  (append (him-input-defs him)
          (him-oport-defs him)
          (him-zport-defs him)))


;; vardef:
;;
;;        vtype  valdef
;;       /      /
;;     (port   (12 unsigned 4 2))
;;
(define him-vardef-vtype  first)
(define him-vardef-valdef second)
(define (him-vardef-width vardef) (him-number-width (him-vardef-valdef vardef)))
(define (him-vardef-sign vardef) (him-number-sign (him-vardef-valdef vardef)))
(define (him-vardef-value vardef)
;;   (aif (him-number-value (him-vardef-valdef vardef))
;;        it
;;        *unspecified*)
  (him-number-value (him-vardef-valdef vardef))
  )
(define (him-vardef-depth vardef)
  (if (and (> (length vardef) 2)
           (number? (third vardef)))
      (third vardef)
      #f))

(define (him-get-vardef varname module)
  (aif (assoc-ref (assoc-ref module 'vars) varname)
       it
       (issue-fatal (ss "Unknown variable: " varname))))

(define (him-get-vardef-valdef varname module)
  (him-vardef-valdef (him-get-vardef varname module)))

(define (him-get-vardef-vtype varname module)
  (him-vardef-vtype (him-get-vardef varname module)))

(define (him-get-vardef-width varname module)
  (him-vardef-width (him-get-vardef varname module)))

(define (him-get-vardef-sign varname module)
  (him-vardef-sign (him-get-vardef varname module)))

(define (him-get-vardef-value varname module)
  (him-vardef-value (him-get-vardef varname module)))

(define (him-get-vardef-depth varname module)
  (him-vardef-depth (him-get-vardef varname module)))


;; Return port definition.
(define (him-port-def him name ptype)
  (let ((vardef (him-get-vardef name him)))
    (list (cons 'name name)
          (cons 'dir (case ptype
                       ((clock) 'input)
                       ((reset) 'input)
                       ((iport) 'input)
                       ((oport) 'output)
                       ((zport) 'inout)))
          (cons 'type  (him-vardef-vtype vardef))
          (cons 'width (him-vardef-width vardef))
          (cons 'sign  (him-vardef-sign vardef))
          (cons 'value (him-vardef-value vardef))
          (cons 'depth (him-vardef-depth vardef)))))


(define (him-port-name port)
  (assoc-ref port 'name))

(define (him-port-dir port)
  (assoc-ref port 'dir))

(define (him-port-type port)
  (assoc-ref port 'type))

(define (him-port-width port)
  (assoc-ref port 'width))

(define (him-port-sign port)
  (assoc-ref port 'sign))

(define (him-port-value port)
  (assoc-ref port 'value))

(define (him-port-depth port)
  (assoc-ref port 'depth))



;; Resolve parameter values.
(define (him-resolve-params params)

  (define (value-set! table var value)
    ;; (set-cdr! (assoc var table) value)
    (set-cdr! (assoc var table) (him-number-value value))
    )

  (define (eval-expr expr param-table value-table reduce)
    (case (first expr)
      ;; TODO: error with: ((strexpr)      (second expr))
      ((number)       (let ((number (second expr)))
                        (if reduce
                            (him-number-value number)
                            number)))
      ((parref)       (if (assoc-ref value-table (second expr))
                          (assoc-ref value-table (second expr))
                          (let ((value (eval-expr (car (assoc-ref param-table (second expr)))
                                                  param-table
                                                  value-table
                                                  reduce)))
                            (value-set! value-table (second expr) value)
                            value)))
      ((op-add)       (apply + (map (lambda (op) (eval-expr op param-table value-table #t)) (cdr expr))))
      ((op-sub)       (apply - (map (lambda (op) (eval-expr op param-table value-table #t)) (cdr expr))))
      ((op-mul)       (apply * (map (lambda (op) (eval-expr op param-table value-table #t)) (cdr expr))))
      ((op-div)       (apply quotient (map (lambda (op) (eval-expr op param-table value-table #t)) (cdr expr))))
      ((op-neg)       (- (eval-expr (second expr) param-table value-table #t)))
      ((op-pos)       (+ (eval-expr (second expr) param-table value-table #t)))
      ))

  (define (get-values param-table)
    (let ((value-table (map (lambda (pair) (cons (first pair) #f)) param-table)))
      (for-each (lambda (pair)
                  (when (not (assoc-ref value-table (first pair)))
                    (value-set! value-table (first pair) (eval-expr (second pair) param-table value-table #f))))
                param-table)
      value-table))

;;   (ppre params)

  ;;(get-values (him-params him))
  (get-values params))



(define (him-numstr numstr)
  (vlog-number-to-him (vlog-number-from-numstr numstr)))

(define (him-numint int)
  (him-number-create int #f #f 10))

(define (him-bitdef . opt-val)
  (let ((val (match opt-val
               ((val) val)
               (else #f))))
    (him-number-create val 'unsigned 1 2)))

(define (him-bitvec width . opt-val)
  (let ((val (match opt-val
               ((val) val)
               (else #f))))
    (him-number-create val 'unsigned width 2)))

(define (him-hexvec width . opt-val)
  (let ((val (match opt-val
               ((val) val)
               (else #f))))
    (him-number-create val 'unsigned width 16)))

(define (him-decvec width . opt-val)
  (let ((val (match opt-val
               ((val) val)
               (else #f))))
    (him-number-create val 'unsigned width 10)))

;; (define indent-step 3)
(define indent-step 2)

(define (inc indent)
  (+ indent indent-step))

(define (dec indent)
  (- indent indent-step))

(define (inc-case indent)
  (+ indent (1- indent-step)))

(define (dec-case indent)
  (- indent (1- indent-step)))


;; Reference key from alist called "item".
(define-syntax ref
  (lambda (x)
    (syntax-case x ()
      ((_ key)
       (with-syntax ((item (datum->syntax x 'item)))
         #`(assoc-ref item key))))))

(define-syntax ref-single
  (lambda (x)
    (syntax-case x ()
      ((_ key)
       (with-syntax ((item (datum->syntax x 'item)))
         #`(car (assoc-ref item key)))))))

(define-syntax ref-if
  (lambda (x)
    (syntax-case x ()
      ((_ key)
       (with-syntax ((item (datum->syntax x 'item)))
         #`(missing->unspecified (assoc-ref item key)))))))

;; Start line with indent and add rest as tail.
(define-syntax line
  (lambda (x)
    (syntax-case x ()
      ((_ arg ...)
       (with-syntax ((indent (datum->syntax x 'indent)))
         ;; #`(ss (:in indent) arg ...)
         #`(ss (fmt 'ind indent) arg ...)
         )))))


;; Number renderer.
;;
;;     (parref "WIDTH")
;;     12
;;     (123 unsigned 4 16)
;;
(define (render-number number)
  (if (number? number)
      (vlog-number-to-quoted-numstr (vlog-number-from-int number))
      (case (car number)
        ((parref) (cdr number))
        ;; TI 251102_1927: Looks like a typo in the string?
        ;; ((parnum) (si "{#{(second number)}{1#{(render-number (third number))}}}"))
        ((parnum) (si "{#{(second number)}{#{(render-number (third number))}}}"))
        (else (vlog-number-to-quoted-numstr (vlog-number-from-him number))))))


;; Number renderer for padded numbers.
(define (render-numpad number)
  (vlog-number-to-quoted-numstr (vlog-number-from-him number) #t))


(define (render-port-list item indent)
  (let ((ports (map render-name (make-list-flat (ref-if 'clock)
                                                (ref-if 'reset)
                                                (ref-if 'iports)
                                                (ref-if 'oports)
                                                (ref-if 'zports)))))
    (if (null? ports)
        *unspecified*
        (let lp ((ports ports)
                 (ret '()))
          (map-with-position (lambda (port pos)
                               (line port (if (eq? pos 'tail) "" ",")))
                             ports)))))


(define (render-module-parameters params indent keyword)
  ;;   (ppe params)
  (if (invalid? params)
      *unspecified*
      (let lp ((params params)
               (ret '()))
        (if (pair? params)
            (lp (cdr params)
                (cons (line keyword " " (first (car params)) " = " (render-expr-top (second (car params))) ";")
                      ret))
            (reverse ret)))))

(define (render-params params indent)
  (render-module-parameters params indent "parameter"))

(define (render-locals locals indent)
  (render-module-parameters locals indent "localparam"))


(define (render-name name)
;;   (ppr name)
  (re-gsub "-" name "_"))

(define (render-variable vtype vardef name)
  ;;   (pd name)
  ;;   (ppr (list vtype vardef name))
  ;;   (pde (him-vardef-width vardef))

  (define (create-msb-str width)
    (cond
     ((number? width) (1- width))
     ((and (eq? (car width) 'op-add)
           (eq? (first (last width)) 'number)
           (= (first (second (last width))) 1))
      ;; Expression with "+1" in the end. Take it out.
      (render-expr-top (second width)))
     (else
      (ss (render-expr-top width) "-1"))))

  (define (signed? vardef)
    ;; (ppr vardef)
    (eq? (if (not (him-vardef-sign vardef))
             (him-number-sign (him-vardef-value vardef))
             (him-vardef-sign vardef))
         'signed))

  ;; NOTE: iport, oport and zport are internal types, not from him.
  (ss (case vtype
        ((iport)  "input ")
        ((oport)  "output")
        ((zport)  "inout ")
        ((sync)   "reg   ")
        ((comb)   "reg   ")
        ((wire)   "wire  "))
      (let ((width (him-vardef-width vardef)))
        (cond
         ((and (number? width) (= width 1))  "")
         (width (ss (if (signed? vardef) " signed" "")
                    " [" (create-msb-str width) ":0]"))
         (else (issue-fatal (si "Variable has no width: \"#{name}\"")))))
      " "
      (render-name name)
      (let ((depth (him-vardef-depth vardef)))
        (if depth
            (begin
              ;; (ppre depth)
              (ss " [0:" (create-msb-str depth) "]"))
            ""))
      ";"
      ))


(define (render-ports ports item indent vtype)
  (if (invalid? ports)
      *unspecified*
      (let lp ((ports ports)
               (ret '()))
        (if (pair? ports)
            (let ((vardef (him-get-vardef (car ports) item)))
              (lp (cdr ports)
                  (cons (line (render-variable vtype vardef (car ports)))
                        ret)))
            (reverse ret)))))

(define (render-inputs item indent)
  (let ((inputs (make-list-flat (ref-if 'clock)
                           (ref-if 'reset)
                           (ref-if 'iports))))
    (render-ports inputs item indent 'iport)))

(define (render-outputs item indent)
  (render-ports (ref 'oports) item indent 'oport))

(define (render-inouts item indent)
  (render-ports (ref 'zports) item indent 'zport))


;;
;; vars:
;;
;;     (("count" .  (sync "WIDTH" unsigned))
;;      ...)
;;
(define (render-vars vars indent)
  (let ((ret (let lp ((vars vars)
                      (ret '()))
               (if (pair? vars)
                   (let ((vardef (cdr (car vars))))
                     (lp (cdr vars)
                         (if (eq? (him-vardef-vtype vardef) 'port)
                             ret
                             (cons (line
                                    (render-variable (him-vardef-vtype vardef)
                                                     vardef
                                                     (first (car vars))))
                                   ret))))
                   (reverse ret)))))
    (if (invalid? ret)
        *unspecified*
        ret)))


(define (render-varref item)
  (render-name item))

(define (render-parref item)
  (render-name item))


(define (render-op-many ops op-symbol)
  (ss (render-expr-sub (first ops))
      (map (lambda (expr) (ss " " op-symbol " " (render-expr-sub expr))) (cdr ops))))

(define (render-op-unary op op-symbol)
  (ss op-symbol (render-expr-sub op)))

(define (render-op-system op op-symbol)
  (ss op-symbol "(" (render-expr-sub op) ")"))


(define (render-op-concat ops)
;;   (ppe ops)
  (ss "{ "
      (ss (render-expr-top (first ops))
          (map (lambda (expr) (ss ", " (render-expr-top expr))) (cdr ops)))
      " }"))

(define (render-varran item)
  (ss
;;    (render-varref (second (first item)))
   (render-expr (first item))
   "[" (render-expr-top (second item)) ":" (render-expr-top (third item)) "]"))

(define (render-varidx item)
  (ss
;;    (render-varref (second (first item)))
   (render-expr (first item))
   "[" (render-expr-top (second item)) "]"))

(define (render-op-condition item)
  (let ((condition (render-expr (first item)))
        (option-true (render-expr (second item)))
        (option-false (render-expr (third item))))
    (ss condition " ? " option-true " : " option-false)))


(define (render-op-add ops) (render-op-many ops "+"))
(define (render-op-sub ops) (render-op-many ops "-"))
(define (render-op-mul ops) (render-op-many ops "*"))
(define (render-op-div ops) (render-op-many ops "/"))

(define (render-op-eq ops) (render-op-many ops "=="))
(define (render-op-ne ops) (render-op-many ops "!="))
(define (render-op-gt ops) (render-op-many ops ">"))
(define (render-op-lt ops) (render-op-many ops "<"))
(define (render-op-ge ops) (render-op-many ops ">="))
(define (render-op-le ops) (render-op-many ops "<="))

(define (render-op-logneg op) (render-op-unary op "!"))
(define (render-op-logand ops) (render-op-many ops "&&"))
(define (render-op-logori ops) (render-op-many ops "||"))

(define (render-op-case-eq ops) (render-op-many ops "==="))
(define (render-op-case-ne ops) (render-op-many ops "!=="))

(define (render-op-bitneg op) (render-op-unary op "~"))
(define (render-op-bitand ops) (render-op-many ops "&"))
(define (render-op-bitori ops) (render-op-many ops "|"))
(define (render-op-bitxor ops) (render-op-many ops "^"))
(define (render-op-bitxnr ops) (render-op-many ops "^~"))

(define (render-op-redand op) (render-op-unary op "&"))
(define (render-op-redori op) (render-op-unary op "|"))
(define (render-op-rednnd op) (render-op-unary op "~&"))
(define (render-op-redxor op) (render-op-unary op "^"))
(define (render-op-redxnr op) (render-op-unary op "~^"))

(define (render-op-log-lshift ops) (render-op-many ops "<<"))
(define (render-op-log-rshift ops) (render-op-many ops ">>"))

(define (render-op-pos op) (render-op-unary op "+"))
(define (render-op-neg op) (render-op-unary op "-"))
(define (render-op-ari-lshift ops) (render-op-many ops "<<<"))
(define (render-op-ari-rshift ops) (render-op-many ops ">>>"))

(define (render-op-cast-unsigned op) (render-op-system op "$unsigned"))
(define (render-op-cast-signed op) (render-op-system op "$signed"))




;; + logneg
;;
;; (op-add args ...)
;;
;;
(define (render-expr expr)
  (if (string? expr)
      ;; expr
      (render-name expr)
      (case (first expr)
        ((strexpr ->s)  (second expr))
        ((number)       (render-number (second expr)))
        ((numpad)       (render-numpad (second expr)))
        ((varref)       (render-varref (second expr)))
        ((parref)       (render-parref (second expr)))

        ((op-concat)    (render-op-concat (cdr expr)))
        ((varran)       (render-varran (cdr expr)))
        ((varidx)       (render-varidx (cdr expr)))

        ((op-add)       (render-op-add (cdr expr)))
        ((op-sub)       (render-op-sub (cdr expr)))
        ((op-mul)       (render-op-mul (cdr expr)))
        ((op-div)       (render-op-div (cdr expr)))

        ((op-eq)        (render-op-eq (cdr expr)))
        ((op-ne)        (render-op-ne (cdr expr)))
        ((op-gt)        (render-op-gt (cdr expr)))
        ((op-lt)        (render-op-lt (cdr expr)))
        ((op-ge)        (render-op-ge (cdr expr)))
        ((op-le)        (render-op-le (cdr expr)))

        ((op-logneg)    (render-op-logneg (second expr)))
        ((op-logand)    (render-op-logand (cdr expr)))
        ((op-logori)    (render-op-logori (cdr expr)))

        ((op-case-eq)   (render-op-case-eq (cdr expr)))
        ((op-case-ne)   (render-op-case-ne (cdr expr)))

        ((op-bitneg)    (render-op-bitneg (second expr)))
        ((op-bitand)    (render-op-bitand (cdr expr)))
        ((op-bitori)    (render-op-bitori (cdr expr)))
        ((op-bitxor)    (render-op-bitxor (cdr expr)))
        ((op-bitxnr)    (render-op-bitxnr (cdr expr)))

        ((op-redand)    (render-op-redand (second expr)))
        ((op-redori)    (render-op-redori (second expr)))
        ((op-rednnd)    (render-op-rednnd (second expr)))
        ((op-redxor)    (render-op-redxor (second expr)))
        ((op-redxnr)    (render-op-redxnr (second expr)))

        ((op-log-lshift)    (render-op-log-lshift (cdr expr)))
        ((op-log-rshift)    (render-op-log-rshift (cdr expr)))

        ((op-pos)    (render-op-pos (second expr)))
        ((op-neg)    (render-op-neg (second expr)))
        ((op-ari-lshift)    (render-op-ari-lshift (cdr expr)))
        ((op-ari-rshift)    (render-op-ari-rshift (cdr expr)))

        ((op-cast-unsigned)  (render-op-cast-unsigned (second expr)))
        ((op-cast-signed)    (render-op-cast-signed (second expr)))

        ((op-condition) (render-op-condition (cdr expr)))

        (else (issue-fatal (ss "Unknown operation: " (first expr))))
        )))


(define (render-expr-sub expr)
  (case (first expr)
    ((number numpad varref parref varran varidx) (render-expr expr))
    (else (ss "(" (render-expr expr) ")"))))


(define (render-expr-top expr)
  (render-expr expr))


(define (render-stmt-ass stmt-ass indent proc)
;;   (ppe stmt-ass)
  (line (render-expr-top (first stmt-ass))
        " "
        (case proc
          ((sync) "<=")
          ((comb) "=")
          ((init) "="))
        " "
        (render-expr-top (second stmt-ass))
        ";"))

(define (render-stmt-nas stmt-nas indent)
  (render-stmt-ass stmt-nas indent 'sync))

(define (render-stmt-bas stmt-bas indent)
  (render-stmt-ass stmt-bas indent 'comb))


;;
;; '(stmt-if ( (expr @) (stmt @) ...)
;;           ( (expr @) (stmt @) ...)
;;           ...
;;           )
;;
(define (render-stmt-if stmt-if indent proc)
;;   (pp stmt-if)
  (let lp ((brs stmt-if)
           (head #t)
           (ret '()))
    (if (pair? brs)
        (lp (cdr brs)
            #f
            (append (reverse
                     (cons
                      (cond (head (line "if ( " (render-expr-top (first (car brs))) " ) begin"))
                            ((first (car brs)) (line "end else if ( " (render-expr-top (first (car brs))) " ) begin"))
                            (else (line "end else begin")))
                      (render-stmts (cdr (car brs)) (inc indent) proc)))
                    ret))
        (reverse (cons (line "end") ret)))))


;;
;; '(stmt-case varref
;;             ( (expr @) (stmt @) ...)
;;             ( (expr @) (stmt @) ...)
;;             ...
;;             )
;;
(define (render-stmt-case-common type stmt-case indent proc)
;;   (ppre stmt-case)
  (let ((type-label (case type
                      ((stmt-case stmt-case-str) "case")
                      ((stmt-casex stmt-casex-str) "casex"))))
    (let lp ((brs (cdr stmt-case))
             (head #t)
             (ret (list (line (ss type-label " ( " (render-name (first stmt-case)) " )")))))
      (if (pair? brs)
          (lp (cdr brs)
              #f
              (append (cons
                       (let ((indent (inc-case indent)))
                         (line "end"))
                       (reverse
                        (cons
                         (let ((indent (inc-case indent)))
                           ;; Use intermedite indent for line-macro.
                           (if (first (car brs))
                               (let ((consts (first (car brs))))
                                 (line (string-join (map render-expr consts) ", ") ": begin")
                                 ;; (line (ss (render-expr (first (car brs))) ": begin"))
                                 )
                               (line (ss "default: begin"))))
                         (render-stmts (cdr (car brs)) (inc-case (inc indent)) proc))))
                      ret))
          (reverse (cons (line "endcase") ret))))))

(define (render-stmt-case stmt-case indent proc)
  (render-stmt-case-common 'stmt-case stmt-case indent proc))

(define (render-stmt-casex stmt-case indent proc)
  (render-stmt-case-common 'stmt-casex stmt-case indent proc))


(define (render-stmt stmt indent proc)
;;   (ppr stmt)
  (define (->str expr)
    (if (string? expr)
        (list 'strexpr expr)
        expr))
  (case (first stmt)
    ((stmt-nas)     (render-stmt-nas (cdr stmt) indent))
    ((stmt-bas)     (render-stmt-bas (cdr stmt) indent))
    ((stmt-ass)     (render-stmt-ass (cdr stmt) indent proc))
    ((stmt-ass-set) (map (lambda (stmt) (render-stmt-ass (cdr stmt) indent proc)) (cdr stmt)))
    ((stmt-if)      (render-stmt-if (cdr stmt) indent proc))
    ((stmt-case)    (render-stmt-case  (cdr stmt) indent proc))
    ((stmt-casex)   (render-stmt-casex (cdr stmt) indent proc))
    ((strexpr ->s)  (line (ss (render-expr-top stmt) ";")))
    ((line)         (line (second stmt)))
    ((code)         (map (lambda (i) (line i)) (cdr stmt)))
    ((stmt-nas-str) (render-stmt-nas (list (list 'varref (first (cdr stmt)))
                                           (list 'strexpr (second (cdr stmt)))) indent))
    ((stmt-bas-str) (render-stmt-bas (list (list 'varref (first (cdr stmt)))
                                           (list 'strexpr (second (cdr stmt)))) indent))
    ((stmt-ass-str) (render-stmt-ass (list (list 'varref (first (cdr stmt)))
                                           (list 'strexpr (second (cdr stmt)))) indent proc))
    ((stmt-if-str)  (render-stmt-if (map (lambda (br) (map ->str br)) (cdr stmt)) indent proc))
    ((stmt-case-str stmt-casex-str)
     (render-stmt-case-common (first stmt)
                              (cons (second stmt)
                                    (map (lambda (br)
                                           (cons (if (first br)
                                                     (map ->str (first br))
                                                     (first br))
                                                 (map ->str (cdr br))))
                                         (cddr stmt)))
                              indent
                              proc))
    ((stmt-idle) *unspecified*)))


;; (define (render-stmts stmts indent proc)
;;   (list-specified
;;    (list-flat (map (lambda (stmt) (render-stmt stmt indent proc))
;;                    stmts))))


(define (render-stmts stmts indent proc)
  (list-flat (map (lambda (stmt) (render-stmt stmt indent proc))
                  stmts)))


(define (render-sync sync item indent)

  ;;     '(stmt-nas (varref <name>) (number @))
  (define (var-resets-branch resets)

    (define (render-rvalue var)
      (list 'number
            (let* ((valdef (him-get-vardef-valdef var item))
                   (value (him-number-value valdef)))
              (cond
               ((not value)
                (issue-fatal (si "Non reset variable: \"#{(car resets)}\"..."))
                ;; (issue-warning (si "Non reset variable: \"#{(car resets)}\"..."))
                )
               ((number? value)
                ;; (ppre valdef)
                (let ((width (him-number-width valdef)))
                  (cond
                   ((number? width) valdef)
                   ((or (= value 0) (= value 1))
                    (list 'parnum
                          (second width)
                          (him-number-create (him-number-value valdef)
                                             (him-number-sign valdef)
                                             #f
                                             (him-number-base valdef))))
                   (else (him-number-create (him-number-value valdef)
                                            (him-number-sign valdef)
                                            #f
                                            (him-number-base valdef))))))
               (else value)))))

    (let lp ((resets resets)
             (ret '()))
      (if (pair? resets)
          (let* ((var (car resets))
                 (vardef (him-get-vardef var item)))
            (lp (cdr resets)
                (cond
                 ((him-vardef-depth vardef)
                  ;; (ppre vardef)
                  (let ((depth (him-vardef-depth vardef)))
                    (append (let lp ((i 0)
                                     (ret '()))
                              (if (< i depth)
                                  (lp (1+ i)
                                      (cons (list 'stmt-nas
                                                  (list 'varidx var (list 'number (him-numint i)))
                                                  (render-rvalue var))
                                            ret))
                                  ret))
                            ret)))
                 (else
                  (cons (list 'stmt-nas
                              (list 'varref var)
                              (render-rvalue var))
                        ret)))))
          (reverse ret))))

  ;; '(stmt-if ( (expr @) (stmt @) ...)
  ;;           ( (expr @) (stmt @) ...)
  ;;           ( #f       (stmt @) ...)
  ;;           ))
  (define (sync-to-stmt-if sync)
    (list (cons (list 'op-logneg (list 'varref (ref-single 'reset)))
                (var-resets-branch (first sync)))
          (cons #f (cdr sync))))

  (make-list-flat (line "always @( posedge " (ref-single 'clock) " or negedge " (ref-single 'reset) " ) begin")
             (render-stmt-if (sync-to-stmt-if sync) (inc indent) 'sync)
             (line "end")))


(define (render-comb comb item indent)
  (make-list-flat (line "always @* begin")
             (render-stmts comb (inc indent) 'comb)
             (line "end")))


(define (render-init init item indent)
  (make-list-flat (line "initial begin")
             (render-stmts init (inc indent) 'init)
             (line "end")))


(define (render-wire wire item indent)
  (list (line (ss "assign "
                  (render-expr-top (first wire))
                  " = "
                  (render-expr-top (second wire))
                  ";"))))


(define (render-note note item indent)
  (map (lambda (i) (line (ss "// " i))) note))


(define (render-code code item indent)
  (map (lambda (i) (line i)) code))


(define (render-line line item indent)
  line)


;; Render instance w/o parameters.
;;
;;    my_mod my_mod
;;      (
;;       .clk( clk ),
;;       .rstn( rstn ),
;;       .init( init ),
;;       .count( count_2 )
;;       );
;;
(define (render-inst inst item indent)

  (define (render-inst-complex item)

    (define (render-param param)
      (if (list? param)
          (case (car param)
            ((parref) (render-name (second param)))
            (else
             ;;              (si ".#{(render-name (first param))}( #{(render-name (him-number-value (second param)))} )")
             (si ".#{(render-name (first param))}( #{(him-number-value (second param))} )")
             ))
          param))

    (let ((mod-name (render-name (first item)))
          (inst-name (render-name (second item)))
          (params (cddr item)))
      ;;       (line (:jn " " (make-list-flat mod-name
      ;;                                 (if (null? params)
      ;;                                     *unspecified*
      ;;                                     (ss "#( " (apply :jn (cons ", "
      ;;                                                                (map render-param params))) " )"))
      ;;                                 inst-name)))
      (line (fmt 'gap 1 (make-list-flat mod-name
                                        (if (null? params)
                                            *unspecified*
                                            (ss "#( " (apply fmt (append (list 'gap '(1 ", "))
                                                                         (map render-param params))) " )"))
                                        inst-name)))))

  (define (render-inst-simple item)
    (line (ss (render-name item) " " (render-name item))))

  (define (render-pair item indent)
    (if (list? item)
        (if (= (length item) 1)
            (line (ss "." (render-name (first item)) "( )"))
            (line (ss "." (render-name (first item)) "( " (render-name (second item)) " )")))
        (line (ss "." item "( " item " )"))))

  (cons (if (list? (car inst))
            (render-inst-complex (car inst))
            (render-inst-simple (car inst)))
        (append (let ((indent (inc-case indent)))
                  (list (line "(")))
                (let ((indent (inc indent)))
                  (map-with-position (lambda (item pos)
                                       (ss (render-pair item indent)
                                           (if (eq? pos 'tail) "" ",")))
                                     (cdr inst)))
                (let ((indent (inc indent)))
                  (list (line ");"))))))


(define (render-procs item indent)
  (let ((procs (ref 'procs)))
    ;;     (ppe procs)
    (if (invalid? procs)
        *unspecified*
        (let lp ((procs procs)
                 (ret '()))
          (if (pair? procs)
              (lp (cdr procs)
                  ;;                      ,add empty line before proc.
                  (append (reverse (cons (ss) ((case (first (car procs))
                                                 ((inst) render-inst)
                                                 ((sync) render-sync)
                                                 ((comb) render-comb)
                                                 ((wire) render-wire)
                                                 ((init) render-init)
                                                 ((note) render-note)
                                                 ((code) render-code)
                                                 ((line) render-line)
                                                 )
                                               (cdr (car procs))
                                               item
                                               indent)))
                          ret))
              ;;     ,add extra space for first proc.
              (cons (ss) (reverse ret)))))))


;; Resolve parameter references to values.
(define (him-resolve item)
  (define (resolve level table)
    (let lp ((items level)
             (table table)
             (ret '()))
      (if (pair? items)
          (let ((item (car items)))
            (cond
             ((pair? item) (case (car item)
                             ((params) (lp (cdr items)
                                           (him-resolve-params (cdr item))
                                           ret))
                             ((parref) (lp (cdr items)
                                           table
                                           (cons (assoc-ref table (second item))
                                                 ret)))
                             (else (lp (cdr items)
                                       table
                                       (cons (resolve item table) ret)))))
             (else (lp (cdr items)
                       table
                       (cons item ret)))))
          (reverse ret))))
  (resolve item #f))


;; Complete him with missing information fields.
(define (him-complete item)
  (let ((schema (him-template)))
    (let lp ((schema schema)
             (ret '()))
      (if (pair? schema)
          (let ((keva (assoc (caar schema) item)))
            (if keva
                (lp (cdr schema)
                    (cons keva ret))
                (lp (cdr schema)
                    (cons (list (caar schema)) ret))))
          (reverse ret)))))


;; Separate (extract) clock and reset away from normal input
;; ports. Separation is based on default regexps ("clk.*", "rst.*").
;;
;; UPDATE: Regexps can be given in "rest": (<clock-re> <reset-re>).
;;
(define (him-separate-clkrst item . rest)
  (let ((clock-re (if (pair? rest) (si "^#{(first rest)}") "^clk"))
        (reset-re (if (pair? rest) (si "^#{(second rest)}") "^rst")))
    (let lp ((iports (assoc-ref item 'iports))
             (clock #f)
             (reset #f)
             (ret '()))
      (if (pair? iports)
          (cond
           ((and (not clock) (re-match? clock-re (car iports)))
            (lp (cdr iports)
                (car iports)
                reset
                ret))
           ((and (not reset) (re-match? reset-re (car iports)))
            (lp (cdr iports)
                clock
                (car iports)
                ret))
           (else (lp (cdr iports)
                     clock
                     reset
                     (cons (car iports) ret))))
          (if (and clock reset)
              (let ((modified (assoc-copy item)))
                (assoc-set-value! modified 'clock (list clock))
                (assoc-set-value! modified 'reset (list reset))
                (assoc-set-value! modified 'iports (reverse ret))
                modified)
              #f)))))


;; Swap the iports to oports and oports to iports. Sync ports are
;; reset to wire ports.
(define (him-swap-port-direction item)
  (define (strip-port port iports oports)
    (cond
     ((member (first port) iports)
      (list (first port)
            'port
            (third port)))
     ((member (first port) oports)
      (list (first port)
            'port
            (third port)))
     (else port)))
  (let ((iports (assoc-ref item 'iports))
        (oports (assoc-ref item 'oports))
        (modified (assoc-copy item)))
    (assoc-set-value! modified 'iports oports)
    (assoc-set-value! modified 'oports iports)
    (assoc-set-value! modified 'vars (map (lambda (port) (strip-port port oports iports)) (assoc-ref modified 'vars)))
    modified))


;; Convert oports to sync ports (without init values).
(define (him-oports-as-regs item)
  (define (regify-port port oports)
    (cond
     ((member (first port) oports)
      (list (first port)
            'sync
            (third port)))
     (else port)))
  (let ((modified (assoc-copy item)))
    (assoc-set-value! modified 'vars (map (lambda (port)
                                            (regify-port port (assoc-ref item 'oports)))
                                          (assoc-ref modified 'vars)))
    modified))


;; Convert module to a monitor module, i.e. all original ports appear
;; as iports.
(define (him-to-monitor item)
  (let ((iports (assoc-ref item 'iports))
        (oports (assoc-ref item 'oports))
        (modified (assoc-copy item)))
    (assoc-set-value! modified 'iports (append iports oports))
    (assoc-set-value! modified 'oports (list))
    modified))


(define (him-render-name name)
  (render-name name))

(define (him-render item)
  (make-list-flat (awhen (ref-if 'header) it)
             (let ((mod (ss "module " (render-name (first (ref 'name)))))
                   (port-list (render-port-list item (1+ indent-step))))
               (if (valid? port-list)
                   (make-list-flat mod
                              (ss (fmt 'ind indent-step) "(")
                              port-list
                              (ss (fmt 'ind (1+ indent-step)) ");"))
                   (ss mod ";")))
             (awhen (render-params (ref 'params) indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (awhen (render-locals (ref 'locals) indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (awhen (render-inputs item indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (awhen (render-outputs item indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (awhen (render-inouts item indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (awhen (render-vars (ref 'vars) indent-step)
                  (if (valid? it) (make-list-flat (ss) it)))
             (render-procs item indent-step)
             (ss)
             (ss "endmodule")))


(define (him-template)
  (list (list 'name   )
        (list 'header )
        (list 'params )
        (list 'locals )
        (list 'clock  )
        (list 'reset  )
        (list 'iports )
        (list 'oports )
        (list 'zports )
        (list 'vars   )
        (list 'procs  )))


;; Return him creator proc.
;;
;; The returned proc is called with arguments that specify parts of
;; the module and the proc accumulates the parts to proper locations.
;;
(define (him-create name)

  (let* ((module (assoc-set! (him-template) 'name (list name)))
         (key-add (lambda (key item)
                    (set-cdr! (assoc key module)
                              (cons item (cdr (assoc key module))))))
         (key-set (lambda (key item)
                    (set-cdr! (assoc key module)
                              (list item))))
         (import-param (lambda (param)
                         (cond
                          ((number? param) (list 'number (him-numint param)))
                          ((string? param) (list 'number (him-numstr param)))
                          (else param))))
         ;;       (v 'var "pdi_state" 'sync 3 (him-hexvec 3 0))
         ;;       (v 'var "pdi_state" 'sync 3 '(parref "pdi_state_head"))
         (import-reset-value (lambda (width sign value)
                               (match value
                                 (('parref parname) (him-number-create value sign width 2))
                                 (else (him-number-create (him-number-value value) sign width (him-number-base value))))))
         (finalize (lambda (mod)
                     (let lp ((mod mod)
                              (ret '()))
                       (if (pair? mod)
                           (if (list? (cdr (car mod)))
                               (lp (cdr mod)
                                   (cons (cons (first (car mod)) (reverse (cdr (car mod))))
                                         ret))
                               (lp (cdr mod)
                                   (cons (car mod)
                                         ret)))
                           (reverse ret))))))
    (let ((procs `((var . ,(lambda args
                             ;; Args:
                             ;;     name type|(type sign) [width] [value]
                             (call-with-values (lambda ()
                                                 (match args
                                                   ((name type) (values name type 1 'unsigned #f))
                                                   ((name (type sign) width) (values name type width sign #f))
                                                   ((name (type sign) width value) (values name type width sign value))
                                                   ((name type width) (values name type width 'unsigned #f))
                                                   ((name type width value) (values name type width 'unsigned value))
                                                   ))
                               (lambda (name type width sign value)
                                 (let ((width (if (string? width) (list 'varref width) width))
                                       (value (if (number? value) (him-numint value) value)))
                                   (case type
                                     ((clock reset)
                                      (key-set type name)
                                      (key-add 'vars (list name 'port (him-number-create #f sign width 2))))
                                     ((iport)
                                      (key-add 'iports name)
                                      (key-add 'vars (list name 'port (him-number-create #f sign width 2))))
                                     ((oport)
                                      (key-add 'oports name)
                                      (if value
                                          (key-add 'vars (list name 'sync (import-reset-value width sign value)))
                                          (key-add 'vars (list name 'port  (him-number-create #f sign width 2)))))
                                     ((zport)
                                      (key-add 'zports name)
                                      (if value
                                          (key-add 'vars (list name 'sync (import-reset-value width sign value)))
                                          (key-add 'vars (list name 'port (him-number-create #f sign width 2)))))
                                     ((sync)
                                      (key-add 'vars (list name 'sync (import-reset-value width sign value))))
                                     ((comb)
                                      (key-add 'vars (list name type (him-number-create #f sign width 2))))
                                     ((wire)
                                      (key-add 'vars (list name type (him-number-create #f sign width 2))))
                                     ))))))
                   (param  . ,(lambda args (key-add 'params (list (first args) (import-param (second args))))))
                   (local  . ,(lambda args (key-add 'locals (list (first args) (import-param (second args))))))
                   (inst   . ,(lambda args (key-add 'procs (cons 'inst args))))
                   (sync   . ,(lambda args (key-add 'procs (cons 'sync args))))
                   (comb   . ,(lambda args (key-add 'procs (cons 'comb args))))
                   (wire   . ,(lambda args (key-add 'procs (cons 'wire args))))
                   (init   . ,(lambda args (key-add 'procs (cons 'init args))))
                   (note   . ,(lambda args (key-add 'procs (cons 'note args))))
                   (show   . ,(lambda args (pp module)))
                   (get    . ,(lambda args (finalize module)))
                   (render . ,(lambda args (him-render module))))))
      (lambda args (apply (assoc-ref procs (car args)) (cdr args))))))


;; tests:
(define test-render   #f)
(define test-empty    #f)
(define test-create   #f)
(define test-trial    #f)


(when test-render
  (let ()
    (define module
      `((name   "counter")
        (header "// This was created by hdl-in-memory.scm.")
        (params ("WIDTH" (op-add (number ,(him-numint 4)) (number ,(him-numint 1))))
                ("VALUE" (number (12 signed 5 10))))
        (locals )
        (clock  "clk" "clk2")
        (reset  "rstn" "rstn2")
        (iports "init"
                "enable")
        (oports "count")
        (zports )
        (vars   ("clk"     port (#f unsigned 1 2) rise)
                ("clk2"    port (#f unsigned 1 2) fall)
                ("rstn"    port (#f unsigned 1 2))
                ("rstn2"   port (#f unsigned 1 2))
                ("init"    port (#f unsigned 1 2))
                ("enable"  port (#f unsigned 1 2))
                ("count"   sync (12 signed 5 10))
                ("fifo"    sync (12 signed 5 10) 10)
                ;; ("count"   sync (parref "WIDTH") #f (parref "VALUE"))
                ;; Symbolic reset value:
                ;; ("count"   sync ((parref "foobar") signed 5 10))
                )
        (procs  (inst "my_mod"
                      "clk"
                      "rstn"
                      ("init" "init")
                      ("count" "count_2"))
                (inst ("my_mod" "my_mod_2" (parref "WIDTH") 4 423)
                      "clk"
                      "rstn"
                      ("init" "init")
                      ("count" "count_3"))
                (init (stmt-ass (varref "count")
                                (op-add (varidx "count" (number ,(him-numint 2)))
                                        (number ,(him-numint 1)))))
                (sync ("count")
                      (stmt-nas (varref "count")
                                (op-add (varidx "count" (number ,(him-numint 2)))
                                        (number ,(him-numint 1))))
                      (stmt-if ((varref "init") (stmt-nas (strexpr "count") (strexpr "count + 4")))
                               (#f (stmt-nas (strexpr "count") (strexpr "count + 1")))))
                (sync ("count")
                      (stmt-nas (varref "count")
                                (op-add (varref "count")
                                        (number ,(him-numint 1))))
                      (stmt-case "count"
                                 (((number ,(him-numstr "'d0"))) (stmt-nas (strexpr "count") (strexpr "count + 1")))
                                 (((number ,(him-numstr "'d1"))
                                   (number ,(him-numstr "'d2"))) (stmt-nas (strexpr "count") (strexpr "count - 0")))))
                (comb (stmt-bas (varref "count")
                                (op-add (varref "count")
                                        (number ,(him-numint 1))))
                      (stmt-if ((varref "init") (stmt-bas (strexpr "count") (strexpr "count + 4"))))
                      (stmt-case-str "count"
                                     (("'d0") "count = count + 1")
                                     (("'d1" "'d2") "count = count - 1")
                                     (#f    "count = 0")))
                (wire (varref "count")
                      (op-add (varref "count")
                              (number ,(him-numint 1))))
                )))

    ;; (ppre (him-resolve-params (him-params module)))
    ;; (ppre (him-resolve module))
;;     (ppre module)
    (pl (him-render module))
    ;; (pl (him-render (him-resolve module)))
    ))

(when test-empty
  (let ()
    (define module (him-complete `((name   "empty"))))
    (pl (him-render module))
    ))


(when test-create
  (let ()
    (define v (him-create "counter-2"))
    (v 'var "clk" 'clock)
    (v 'var "rstn" 'reset)
    (v 'var "count" '(oport signed) "WIDTH" (him-numstr "sd0"))
    ;; (v 'var "count" '(oport signed) "WIDTH" (0 unsigned #f 10))
    (v 'var "init" 'iport)
    (v 'var "foobar" 'iport)
    (v 'param "WIDTH" 8)
    (v 'local "STATE" 1)
    (v 'inst
       "my_mod"
       "clk"
       "rstn"
       "init"
       "count_2")
    (v 'sync
       '("count")
       '(stmt-if ((strexpr "init") (stmt-nas-str "count" "count + 2") (stmt-nas-str "foo" "bar + 1"))
                 (#f (stmt-if-str ("init" "count <= count + 2" "foo <= bar + 1")
                                  (#f (stmt-nas-str "count" "count + 1"))))))
    (v 'sync
       '("count")
       '(stmt-if ((strexpr "init") (stmt-nas-str "count" "count + 2") (stmt-nas-str "foo" "bar + 1"))
                 (#f (stmt-if-str ("init" "count <= count + 2" "foo <= bar + 1")
                                  (#f (stmt-nas-str "count" "count + 1"))))))
    (v 'comb
       '(stmt-if ((strexpr "init") (stmt-nas-str "count" "count + 2") (stmt-ass-str "foo" "bar + 1"))
                 (#f (stmt-if-str ("init" "count = count + 2" "foo = bar + 1")
                                  (#f (stmt-ass-str "count" "count + 1"))))))

    (v 'note "Hello")
    (v 'wire "foo" "bar + 1")
    ;; (v 'show)
    (pl (him-render (v 'get)))
    ;;(pp (him-render (v 'get)))
    ;;(pp (v 'get))
    )
  )

(when test-trial
  (let ()
    (define module
      (him-complete
       `((name   "counter")
         (header "// This was created by hdl-in-memory.scm.")
         (params ("WIDTH" (op-add (number ,(him-numint 4)) (number ,(him-numint 1))))
                 ("VALUE" (number (12 signed 5 10))))
         (locals )
         (clock  "clk")
         (reset  "rstn")
         (iports "init"
                 "enable")
         (oports "count")
         (zports )
         (vars   ("clk"     port (#f unsigned 1 2))
                 ("rstn"    port (#f unsigned 1 2))
                 ("init"    port (#f unsigned 1 2))
                 ("enable"  port (#f unsigned 1 2))
                 ("count"   sync (12 signed 5 10))
                 ;; ("count"   sync (parref "WIDTH") #f (parref "VALUE"))
                 )
         (procs  (wire "count" "count + 1")
                 (comb
                  (stmt-if
                   ((strexpr "bus_tra")
                    (stmt-casex-str
                    "bus_adr[ 12:5 ]"
                    (("8'b00000000000xx")
                    "ram_a_tra = 1'b1"
                    "bus_hld = ram_a_hld")
                    (("8'b00000100000xx")
                    "ram_b_tra = 1'b1"
                    "bus_hld = ram_b_hld")
                    (("8'b0000100000xxx")
                    "ram_1_tra = 1'b1"
                    "bus_hld = ram_1_hld")
                    (("8'b0001000000000")
                    "csr_1_tra = 1'b1"
                    "bus_hld = csr_1_hld")
                    (("8'b0001000100000")
                    "csr_2_tra = 1'b1"
                    "bus_hld = csr_2_hld")
                    (("8'b0001001000000")
                    "csr_3_tra = 1'b1"
                    "bus_hld = csr_3_hld")))))
                 ))))

    (pl (him-render module))
    ))
