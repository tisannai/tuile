(define-module (tuile vlogmod)
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
   /output
   <input-port>                         ; This is needed for some reason?
   ))


;; TODO
;;
;; * Query function API
;;


;; vlogmod - Verilog module creator
;;
;; Create module, add variables (ports, regs, etc.) and body to the
;; module, and finally display it with "codeprint".
;;
;;
;; Example:
;;
;;     (define v (/create "my_mod"))
;;     (=header v (list "// This is my module."
;;                      "`include \"my_inc.v\""
;;                      ""))
;;     
;;     (+var v 'clock "clk")
;;     (+var v 'reset "rstn")
;;     (+var v 'input "init")
;;     (+var v 'input "en")
;;     (+var v '(output reg) "count" 3)
;;     (+var v 'param "my_par" 13)
;;     (+var v 'comb "combi" 10)
;;     (+var v 'wire "wirei")
;;     (+var v 'tie "tiei" 10 12)
;;     
;;     (+body v "always @( posedge clk or negedge rstn ) begin" )
;;     (+body v "   if ( !rstn) begin")
;;     (+body v "      count <= 16'b0;")
;;     (+body v "   end else begin")
;;     (+body v "      if ( init ) begin")
;;     (+body v "         count <= 0;")
;;     (+body v "      end else if ( en ) begin")
;;     (+body v "         count <= count + 1;")
;;     (+body v "      end")
;;     (+body v "   end")
;;     (+body v "end" )
;;     
;;     (define pp (codeprint-open "<stdout>"))
;;     (/output v pp)


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
  header                                ; 10: header lines
  body                                  ; 11: body lines
  )

(define (inputs v)
  (list-compact (append (vlogmod-clocks v)
                        (vlogmod-resets v)
                        (vlogmod-inputs v))))

(define outputs vlogmod-outputs)

(define (ports v) (append (inputs v) (outputs v)))


;; ------------------------------------------------------------
;; Width

(define-mu-record width
  signed
  value
  )

(define (width-create spec)
  (cond
   ((string? spec)
    (if (eq? (string-ref spec 0) #\+)
        (make-width #t (substring spec 1))
        (make-width #f spec)))
   (else
    (make-width #f spec))))

(define (width-def w)
  (define (width-value-to-range value)
    (if (= value 1)
        ""
        (ss "[" (1- value) ":0]")))
  (cond
   ((integer? (width-value w))
    (width-value-to-range (width-value w)))
   (else
    (width-value-to-range (string->number (width-value w))))))

(define (width-fulldef w)
  (ss (if (width-signed w)
          "signed "
          "       ")
      (width-def w)))

(define (width->string w)
  (cond
   ((integer? (width-value w))
    (ss (width-value w)))
   (else
    (width-value w))))

;; Width
;; ------------------------------------------------------------


;; ------------------------------------------------------------
;; Variable types:

(define-class <variable> ()
  name
  width
  info
  comment
  )

;; This is initialize method with unique name.
(define-method (initialize-variable (self <variable>) initargs)
  (slot-set! self 'name (list-ref initargs 0))
  (slot-set! self 'width (list-ref initargs 1))
  (let ((rest (drop initargs 2)))
    (let-optional rest ((info #f)
                        (comment #f))
                  (slot-set! self 'info info)
                  (slot-set! self 'comment comment))))

(define-method (initialize (self <variable>) initargs)
  (initialize-variable self initargs))

(define-method (sizedef (self <variable>))
  (ss (:lj 32 " " (width-fulldef (slot-ref self 'width)))
      (slot-ref self 'name)))

(define-method (portdef (self <variable>))
  (slot-ref self 'name))

(define-class <input-port> (<variable>))
(define-method (vardef (self <input-port>)) (ss "input  " (sizedef self) ";"))

;;(define dummy-input-port (make <input-port> "foo" "bar"))

(define-class <clock> (<input-port>))
(define-class <reset> (<input-port>))
(define-class <input> (<input-port>))

(define-class <output> (<variable>))
(define-method (vardef (self <output>)) (ss "output " (sizedef self) ";"))

(define-class <reg> (<variable>)
  value
  )
(define-method (initialize (self <reg>) initargs)
  (initialize-variable self (delete-ref initargs 2))
  (slot-set! self 'value (list-ref initargs 2)))
(define-method (vardef (self <reg>)) (ss "reg    " (sizedef self) ";"))


(define-class <wire> (<variable>))
(define-method (vardef (self <wire>)) (ss "wire   " (sizedef self) ";"))

(define-class <comb> (<variable>))
(define-method (vardef (self <comb>)) (ss "reg    " (sizedef self) ";"))

(define-class <param> (<variable>))
(define-method (vardef (self <param>))
  (ss "parameter " (slot-ref self 'name) " = " (width->string (slot-ref self 'width)) ";"))

(define-class <tie> (<reg>))
(define-method (vardef (self <tie>)) (ss "wire   " (sizedef self) ";"))

;; Variable types:
;; ------------------------------------------------------------


;; Create vlogmod with name.
(define (/create name)
  ;; Apply name, and for the rest of the fields, calculate the number
  ;; of non-name fields from <vlogmod> and use empty lists as init values.
  (apply make-vlogmod (cons name
                            (make-list (1- (/ (string-length (symbol->string (struct-ref <vlogmod>
                                                                                         vtable-index-layout)))
                                              2))
                                       (list)))))

;; Add variable with type and optionally with width and value.
;;
;; Width is 1 by default.
;; value is 0 by default, but it is only needed by reg and tie.
(define* (+var v type-or-types name #:optional (width 1) (value 0))
  (define (add for this) (append (for v) (list this)))
  (let ((width (width-create width))
        (types (if (list? type-or-types) type-or-types (list type-or-types))))
    (let loop ((types types))
      (when (pair? types)
        (case (car types)
          ((clock) (set-vlogmod-clocks! v (add vlogmod-clocks (make <clock> name width))))
          ((reset) (set-vlogmod-resets! v (add vlogmod-resets (make <reset> name width))))
          ((input) (set-vlogmod-inputs! v (add vlogmod-inputs (make <input> name width))))
          ((output) (set-vlogmod-outputs! v (add vlogmod-outputs (make <output> name width))))
          ((reg) (set-vlogmod-regs! v (add vlogmod-regs (make <reg> name width value))))
          ((wire) (set-vlogmod-wires! v (add vlogmod-wires (make <wire> name width))))
          ((comb) (set-vlogmod-combs! v (add vlogmod-combs (make <comb> name width))))
          ((tie) (set-vlogmod-ties! v (add vlogmod-ties (make <tie> name width value))))
          ((param) (set-vlogmod-params! v (add vlogmod-params (make <param> name width)))))
        (loop (cdr types))))))

;; Add line(s) to body.
(define (+body v line) (set-vlogmod-body! v (append (vlogmod-body v) (if (list? line) line (list line)))))

;; Add line(s) to header.
(define (+header v line) (set-vlogmod-header! v (append (vlogmod-header v) (if (list? line) line (list line)))))

;; Set header content.
(define (=header v lines) (set-vlogmod-header! v lines))

;; Output <vlogmod> with codeprinter ("pp").
(define (/output v pp)
  (define (output-vardefs lst)
    (pp 'p)
    (for-each (lambda (i) (pp 'p (vardef i))) lst))
  (for-each (lambda (header-line)
              (pp 'p header-line))
            (vlogmod-header v))
  (pp 'p (ss "module " (vlogmod-name v)))
  (pp 'p "  (")
  (pp 'i)
  (pp 's (map portdef (ports v)) ",")
  (pp 'p ");")
  (output-vardefs (vlogmod-params v))
  (output-vardefs (inputs v))
  (output-vardefs (outputs v))
  (output-vardefs (vlogmod-regs v))
  (output-vardefs (vlogmod-combs v))
  (output-vardefs (vlogmod-wires v))
  (output-vardefs (vlogmod-ties v))

  (when (pair? (vlogmod-ties v))
    (pp 'p 'p)
    (for-each (lambda (tie)
                (pp 'p (ss "assign " (slot-ref tie 'name) " = " (slot-ref tie 'value) ";")))
              (vlogmod-ties v)))

  (pp 'p 'p)
  (for-each (lambda (body-line)
              (pp 'p body-line))
            (vlogmod-body v))

  (pp 'd 'p)
  (pp 'p "endmodule"))
