(define-module (tuile vs5mod)
  #:use-module (srfi srfi-1)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile codeprint)
  #:use-module (tuile record)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export
  (
   /create
   =header
   +header
   +body
   +var
   /output-vl
   /output-sv
   <input-port>                         ; This is needed for some reason?
   ))


;; vs5mod - Verilog, SystemVerilog, Verilog2005 module creator
;;
;; Procedural: Create module, add variables (ports, regs, etc.) and
;; body to the module, and finally display it with "codeprint".
;;


(define-mu-record vlogmod
  name                                  ; 0: module name
  clocks                                ; 1: clock list
  resets                                ; 2: reset list
  inputs                                ; 3: input list
  outputs                               ; 4: output list
  regs                                  ; 5: reg list
  wires                                 ; 6: wire list
  combs                                 ; 7: comb list
  ties                                  ; 8: tie list
  params                                ; 9: param list
  parals                                ; 10: localparam list
  header                                ; 11: header lines
  body                                  ; 12: body lines
  )

(define (inputs v)
  (list-compact (append (vlogmod-clocks v)
                        (vlogmod-resets v)
                        (vlogmod-inputs v))))

(define outputs vlogmod-outputs)

(define (ports v) (append (inputs v) (outputs v)))


;; ------------------------------------------------------------
;; Width

(define (width-create spec)
  spec)

(define (width-def w)
  (define (width-value-to-range value)
    (if (= value 1)
        ""
        (ss "[" (1- value) ":0]")))
  (cond
   ((integer? w)
    (width-value-to-range w))
   (else
    (width-value-to-range (string->number w)))))

(define (width-fulldef w)
  (width-def w))

(define (width->string w)
  (ss w))

;; Width
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Variable types:

(define-class <variable> ()
  name
  width
  signed
  types
  info
  comment
  )

;; This is initialize method with unique name.
(define-method (initialize-variable (self <variable>) initargs)
  (slot-set! self 'name (list-ref initargs 0))
  (slot-set! self 'width (list-ref initargs 1))
  (slot-set! self 'signed (list-ref initargs 2))
  (slot-set! self 'types (list-ref initargs 3))
  (let ((rest (drop initargs 4)))
    (let-optional rest ((info #f)
                        (comment #f))
                  (slot-set! self 'info info)
                  (slot-set! self 'comment comment))))

(define-method (initialize (self <variable>) initargs)
  (initialize-variable self initargs))

(define-method (sizedef (self <variable>))
  (ss (:lj 32 " " (width-fulldef (slot-ref self 'width)))
      (slot-ref self 'name)))

(define-method (vl-portdef (self <variable>))
  (slot-ref self 'name))

(define-method (vl-vardef (self <variable>))
  (ss (:lj 10 " " (vl-basetype self))
      (:lj 10 " " (if (slot-ref self 'signed) "signed" ""))
      (sizedef self)))

(define-method (sv-vardef (self <variable>))
  (define (sv-type-map type)
    (case type
      ((reg) "logic")
      ((comb) "logic")
      ((wire) "logic")
      ((signed) "signed")))
  (ss (:lj 10 " " (sv-basetype self))
      (:lj 16 " " (string-join (map sv-type-map (slot-ref self 'types))))
      (sizedef self)))

(define-class <input-port> (<variable>))
(define-method (vl-basetype (self <input-port>)) "input")
(define-method (sv-basetype (self <input-port>)) "input")

;;(define dummy-input-port (make <input-port> "foo" "bar"))

(define-class <clock> (<input-port>))
(define-class <reset> (<input-port>))
(define-class <input> (<input-port>))

(define-class <output> (<variable>))
(define-method (vl-basetype (self <output>)) "output")
(define-method (sv-basetype (self <output>)) "output")

(define-class <reg> (<variable>)
  value
  )
(define-method (initialize (self <reg>) initargs)
  (initialize-variable self (delete-nth initargs 4))
  (slot-set! self 'value (list-ref initargs 4)))
(define-method (vl-basetype (self <reg>)) "reg")
(define-method (sv-basetype (self <reg>)) "logic")


(define-class <wire> (<variable>))
(define-method (vl-basetype (self <wire>)) "wire")
(define-method (sv-basetype (self <wire>)) "logic")

(define-class <comb> (<variable>))
(define-method (vl-basetype (self <comb>)) "reg")
(define-method (sv-basetype (self <comb>)) "logic")

(define-class <param> (<variable>))
(define-method (vl-vardef (self <param>))
  (ss "parameter  " (slot-ref self 'name) " = " (width->string (slot-ref self 'width))))
(define-method (sv-vardef (self <param>))
  (ss (slot-ref self 'name) " = " (width->string (slot-ref self 'width))))

(define-class <paral> (<variable>))
(define-method (vl-vardef (self <paral>))
  (ss "localparam " (slot-ref self 'name) " = " (width->string (slot-ref self 'width))))
(define-method (sv-vardef (self <paral>))
  (ss "localparam " (slot-ref self 'name) " = " (width->string (slot-ref self 'width))))

(define-class <tie> (<reg>))
(define-method (vl-basetype (self <tie>)) "wire")
(define-method (sv-basetype (self <tie>)) "logic")

;; Variable types:
;; ------------------------------------------------------------


;; Create vlogmod with name.
(define (/create name)
  ;; Apply name, and for the rest of the fields, calculate the number
  ;; of non-name fields from <vlogmod> and use empty lists as init values.
  (apply make-vlogmod (cons name
                            (make-list (1- (/ (string-length
                                               (symbol->string
                                                (struct-ref <vlogmod>
                                                            vtable-index-layout)))
                                              2))
                                       (list)))))

;; Add variable with type(s) and optionally with width and value.
;;
;; Width is 1 by default.
;; value is 0 by default, but it is only needed by reg and tie.
(define* (+var v type-or-types name #:optional (width 1) (value 0))
  (define (add for this) (append (for v) (list this)))
  (let* ((width (width-create width))
         (klass (if (list? type-or-types) (car type-or-types) type-or-types))
         (types (if (list? type-or-types) (cdr type-or-types) '())))
    (case klass
      ((clock)  (set-vlogmod-clocks!  v (add vlogmod-clocks  (make <clock>  name width #f types))))
      ((reset)  (set-vlogmod-resets!  v (add vlogmod-resets  (make <reset>  name width #f types))))
      ((input)  (set-vlogmod-inputs!  v (add vlogmod-inputs  (make <input>  name width #f types))))
      ((output) (set-vlogmod-outputs! v (add vlogmod-outputs (make <output> name width #f types))))
      ((reg)    (set-vlogmod-regs!    v (add vlogmod-regs    (make <reg>    name width #f types value))))
      ((wire)   (set-vlogmod-wires!   v (add vlogmod-wires   (make <wire>   name width #f types))))
      ((comb)   (set-vlogmod-combs!   v (add vlogmod-combs   (make <comb>   name width #f types))))
      ((tie)    (set-vlogmod-ties!    v (add vlogmod-ties    (make <tie>    name width #f types value))))
      ((param)  (set-vlogmod-params!  v (add vlogmod-params  (make <param>  name width #f types))))
      ((paral)  (set-vlogmod-parals!  v (add vlogmod-parals  (make <paral>  name width #f types)))))))

;; Add line(s) to body.
(define (+body v line) (set-vlogmod-body! v (append (vlogmod-body v) (if (list? line) line (list line)))))

;; Add line(s) to header.
(define (+header v line) (set-vlogmod-header! v (append (vlogmod-header v) (if (list? line) line (list line)))))

;; Set header content.
(define (=header v lines) (set-vlogmod-header! v lines))


;; ------------------------------------------------------------
;; Common output methods.

(define (output-header v pp)
  (for-each (lambda (header-line)
              (pp 'p header-line))
            (vlogmod-header v)))

(define (output-ties v pp)
  (when (pair? (vlogmod-ties v))
    (pp 'p 'p)
    (for-each (lambda (tie)
                (pp 'p (ss "assign " (slot-ref tie 'name) " = " (slot-ref tie 'value) ";")))
              (vlogmod-ties v))))

(define (output-body v pp)
  (pp 'p 'p)
  (for-each (lambda (body-line)
              (pp 'p body-line))
            (vlogmod-body v)))


;; In Verilog we must expand output ports with multiple types to
;; multiple variables.
(define (vlog-expand-typeinfo v)
  (define (add for this) (append (for v) (list this)))
  (for-each
   (lambda (outp)
     (let ((types (slot-ref outp 'types)))
       (when (pair? types)
         (let ((signed (->bool (member 'signed types)))
               (name  (slot-ref outp 'name))
               (width (slot-ref outp 'width)))
           (when signed
             (slot-set! outp 'signed #t))
           (let loop ((rest types))
             (when (pair? rest)
               (case (car rest)
                 ((reg)  (set-vlogmod-regs! v (add vlogmod-regs (make <reg> name width signed #f #f))))
                 ((comb) (set-vlogmod-regs! v (add vlogmod-regs (make <reg> name width signed #f #f)))))
               (loop (cdr rest))))))))
   (outputs v)))


;; ------------------------------------------------------------
;; Output Verilog of <vlogmod> with codeprinter ("pp").
(define (/output-vl v pp)

  (define (output-vardefs lst)
    (pp 'p)
    (for-each (lambda (i) (pp 'p (ss (vl-vardef i) ";"))) lst))

  (vlog-expand-typeinfo v)

  (output-header v pp)

  (pp 'p (ss "module " (vlogmod-name v)))
  (pp 'p "  (")
  (pp 'i)
  (pp 's (map vl-portdef (ports v)) ",")
  (pp 'p ");")
  (output-vardefs (vlogmod-params v))
  (output-vardefs (vlogmod-parals v))
  (output-vardefs (inputs v))
  (output-vardefs (outputs v))
  (output-vardefs (vlogmod-regs v))
  (output-vardefs (vlogmod-combs v))
  (output-vardefs (vlogmod-wires v))
  (output-vardefs (vlogmod-ties v))

  (output-ties v pp)

  (output-body v pp)

  (pp 'd 'p)
  (pp 'p "endmodule"))


;; ------------------------------------------------------------
;; Output SystemVerilog of <vlogmod> with codeprinter ("pp").
(define (/output-sv v pp)

  (define (output-vardefs lst)
    (pp 'p)
    (for-each (lambda (i) (pp 'p (ss (sv-vardef i) ";"))) lst))

  (output-header v pp)

  (pp 'p (ss "module " (vlogmod-name v)))
  (when (pair? (vlogmod-params v))
    (pp 'p "  #(")
    (pp 'p (ss "   parameter " (string-join (map sv-vardef (vlogmod-params v)) ", ")))
    (pp 'p "   )"))
  (pp 'p "  (")
  (pp 'i)
  (pp 's (map sv-vardef (ports v)) ",")
  (pp 'p ");")
  (output-vardefs (vlogmod-parals v))
  (output-vardefs (vlogmod-regs v))
  (output-vardefs (vlogmod-combs v))
  (output-vardefs (vlogmod-wires v))
  (output-vardefs (vlogmod-ties v))

  (output-ties v pp)

  (output-body v pp)

  (pp 'd 'p)
  (pp 'p "endmodule"))
