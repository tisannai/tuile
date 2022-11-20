(define-module (tuile vlogmod)
  #:use-module (srfi srfi-1)
  #:use-module (tuile utils)
  #:use-module (tuile pr)
  #:use-module (tuile re)
  #:use-module (tuile codeprint)
  #:use-module (tuile record-r6rs)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export
  (
   ;; Creation:
   /create
   =indent
   =header
   +header
   +body
   +var
   +sync
   +comb
;;   +stmt-if
;;   +stmt-case
   /output
   /output-file
   /build

   ;; Queries:
   ?clocks
   ?resets
   ?inputs
   ?inputs+
   ?outputs

   ))


;; vlogmod - Verilog module creator
;;
;; Procedural: Create module, add variables (ports, regs, etc.) and
;; body to the module, and finally display it with "codeprint".
;;
;; Declarational: Describe all module elements and perform module
;; build.
;;
;;
;; Procedural example:
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
;;     (+var v '(output reg) "count" '(signed . 3))
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
;;
;;
;; Declarational example:
;;
;;     (/build
;;      '((file   . "<stdout>")
;;        (module . "my_mod")
;;        (header . ("// This is my module."
;;                   "`include \"my_inc.v\""
;;                   ""))
;;        (vars . ((clock "clk")
;;                 (reset "rstn")
;;                 (input ("init" "en"))
;;                 ((output reg) (("count" 3)
;;                                ("foobar" 2)))
;;                 (param "my_par" 13)
;;                 (comb  "combi" 10)
;;                 (wire  "wirei")
;;                 (tie   "tiei" 10 12)
;;                 ))
;;        (body . ("always @( posedge clk or negedge rstn ) begin"
;;                 "   if ( !rstn) begin"
;;                 "      count <= 16'b0;"
;;                 "   end else begin"
;;                 "      if ( init ) begin"
;;                 "         count <= 0;"
;;                 "      end else if ( en ) begin"
;;                 "         count <= count + 1;"
;;                 "      end"
;;                 "   end"
;;                 "end"
;;            ))))


(define-record-type vlogmod
  (fields (mutable name)                ; 0: module name
          (mutable clocks)              ; 1: clock list
          (mutable resets)              ; 2: reset list
          (mutable inputs)              ; 3: input list
          (mutable outputs)             ; 4: output list
          (mutable regs)                ; 5: reg list
          (mutable wires)               ; 6: wire list
          (mutable combs)               ; 7: comb list
          (mutable ties)                ; 8: tie list
          (mutable params)              ; 9: param list
          (mutable header)              ; 10: header lines
          (mutable body)                ; 11: body lines
          (mutable indent-step)         ; 12: Code indent step (default: 3)
          ))

(define default-indent-step 3)

(define (inputs v)
  (list-compact (append (vlogmod-clocks v)
                        (vlogmod-resets v)
                        (vlogmod-inputs v))))

(define outputs vlogmod-outputs)

(define (ports v) (append (inputs v) (outputs v)))

(define ?clocks vlogmod-clocks)
(define ?resets vlogmod-resets)
(define ?inputs vlogmod-inputs)
(define ?inputs+ inputs)
(define ?outputs vlogmod-outputs)


;; ------------------------------------------------------------
;; Width

(define-record-type width
  (fields signed
          value
          ))

(define (width-create spec)
  (cond
   ((string? spec)
    (if (eq? (string-ref spec 0) #\+)
        (make-width #t (substring spec 1))
        (make-width #f spec)))
   ((pair? spec)
    (make-width (eq? (car spec) 'signed)
                (cdr spec)))
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

;; NOTE: "<input-port>" conflicts with goops, hence use "<v-input-port>".
(define-class <v-input-port> (<variable>))
(define-method (vardef (self <v-input-port>)) (ss "input  " (sizedef self) ";"))

(define-class <clock> (<v-input-port>))
(define-class <reset> (<v-input-port>))
(define-class <input> (<v-input-port>))

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
                            (append
                             (make-list (- (/ (string-length
                                                (symbol->string
                                                 (struct-ref vlogmod
                                                             vtable-index-layout)))
                                              2)
                                           2) ; Remove name and indent-step.
                                        (list))
                             (list default-indent-step)))))

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
          ((clock) (vlogmod-clocks-set! v (add vlogmod-clocks (make <clock> name width))))
          ((reset) (vlogmod-resets-set! v (add vlogmod-resets (make <reset> name width))))
          ((input) (vlogmod-inputs-set! v (add vlogmod-inputs (make <input> name width))))
          ((output) (vlogmod-outputs-set! v (add vlogmod-outputs (make <output> name width))))
          ((reg) (vlogmod-regs-set! v (add vlogmod-regs (make <reg> name width value))))
          ((wire) (vlogmod-wires-set! v (add vlogmod-wires (make <wire> name width))))
          ((comb) (vlogmod-combs-set! v (add vlogmod-combs (make <comb> name width))))
          ((tie) (vlogmod-ties-set! v (add vlogmod-ties (make <tie> name width value))))
          ((param) (vlogmod-params-set! v (add vlogmod-params (make <param> name width)))))
        (loop (cdr types))))))

;; Add line(s) to body.
(define (+body v line) (vlogmod-body-set! v (append (vlogmod-body v) (if (list? line) line (list line)))))


;; Indent of one step, macro.
;;
;;     (indent-1)
;;
(define-syntax indent-1
  (lambda (x)
    (with-syntax ((vlogmod-indent-step (datum->syntax x 'vlogmod-indent-step))
                  (v (datum->syntax x 'v)))
      #`(make-string (vlogmod-indent-step v) #\ ))))


;; Indent of n-steps, macro.
;;
;;     (indent-n 2)
;;
(define-syntax indent-n
  (lambda (x)
    (let ((stx (syntax->datum x)))
      (with-syntax ((vlogmod-indent-step (datum->syntax x 'vlogmod-indent-step))
                    (v (datum->syntax x 'v)))
        #`(make-string (* #,(datum->syntax x (cadr stx)) (vlogmod-indent-step v)) #\ )))))


;; Add clocked always-block.
;;
;; The sync-spec is an alist of clock, reset and regs (FFs) to include
;; to reset branch (reg-spec). When clock or reset is missing from the
;; sync-spec, the first clock and reset of the block is used,
;; respectively. reg-spec starts with either "inc" or "exc" symbol
;; which specifies the "polarity" of the spec. "inc" regs are included
;; and "exc" regs are excluded, i.e. removed from the list of all regs
;; in the module. If reg-spec is "#f", all module regs are used.
;;
;; sync-spec example:
;;
;;     ((clock . "clk")
;;      (reset . "rstn")
;;      (regs  . '(inc r1 r2)))
;;
;; "lines" are simply a list for code lines in the non-reset branch.
;;
;; Example:
;;
;;       (+sync v
;;              #f
;;              (list 'stmt-if
;;                    v
;;                    (list "init"
;;                          "count <= 0;")
;;                    (list "end"
;;                          "count <= count + 1;")))
;;
(define (+sync v sync-spec . parts)
  (define (signame s) (slot-ref s 'name))
  (let* ((clock (if (and sync-spec (assoc-ref sync-spec 'clock))
                    (assoc-ref sync-spec 'clock)
                    (signame (car (vlogmod-clocks v)))))
         (reset (if (and sync-spec (assoc-ref sync-spec 'reset))
                    (assoc-ref sync-spec 'reset)
                    (signame (car (vlogmod-resets v)))))
         (reg-spec (if sync-spec (assoc-ref sync-spec 'regs) #f))
         (all-reg-names (lambda () (map signame (vlogmod-regs v))))
         (get-reg-by-name (lambda (reg-name)
                            (find (lambda (reg) (string=? (signame reg) reg-name))
                                  (vlogmod-regs v))))
         (regs (cond
                ((not reg-spec) (all-reg-names))
                ((eq? (car reg-spec) 'inc) (cdr reg-spec))
                (else (lset-difference string=? (all-reg-names) (cdr reg-spec))))))
    (+body v "")
    (+body v (si "always @( posedge #{clock} or negedge #{reset} ) begin"))
    (+body v (si "   if ( !#{reset} ) begin"))
    (for-each (lambda (reg)
                (+body v (si "      #{reg} <= #{(slot-ref (get-reg-by-name reg) 'value)};")))
              regs)
    (+body v (si "   end else begin"))
    (for-each (lambda (line) (+body v (ss (indent-n 2) line)))
              (+stmt v 'sync parts))
    (+body v (si "   end"))
    (+body v (si "end"))))


;; Add combinational process.
(define (+comb v . parts)
  (define (signame s) (slot-ref s 'name))
  (+body v "")
  (+body v (si "always @* begin"))
  (for-each (lambda (line)
              (+body v (ss (indent-n 1) line)))
            (+stmt v 'comb parts))
  (+body v (si "end")))


;; Add if-stmt to sync/comb.
;;
;; Example:
;;
;;     (+stmt-if
;;      v
;;      '("init"
;;        "count <= 0;")
;;      '("end"
;;        "count <= count + 1;"))
;;
(define (obsolete-+stmt-if v . parts)
  (let loop ((parts parts)
             (first #t)
             (ret '()))
    (if (pair? parts)
        (loop (cdr parts)
              #f
              (append ret (cons (cond
                                 (first (ss "if ( " (caar parts) ") begin"))
                                 (else (if (caar parts)
                                           (ss "end " (caar parts) " begin")
                                           (ss "end else begin"))))
                                (map (lambda (stmt) (ss indent-1 stmt)) (cdar parts)))))
        (append ret (list "end")))))

;; Add case-stmt to sync/comb.
;;
;; Example:
;;
;;     (+stmt-case
;;      v
;;      "count"                            ; Case variable.
;;      `(("0"
;;         "count <= 0;")
;;        ("1"
;;         "count <= count + 1;")
;;        (#f
;;         "count <= count;")))
;;
(define (obsolete-+stmt-case v case-var parts)
  (let* ((case-indent (make-string (1- (vlogmod-indent-step v)) #\ ))
         (body-indent (make-string (1- (* 2 (vlogmod-indent-step v))) #\ ))
         (case-branch (lambda (condition stmts)
                        (append (list (ss case-indent condition ": begin"))
                                (map (lambda (stmt) (ss body-indent stmt)) stmts)
                                (list (ss case-indent "end"))))))
    (let loop ((parts parts)
               (ret (list (ss "case ( " case-var " )"))))
      (if (pair? parts)
          (if (caar parts)
              (loop (cdr parts)
                    (append ret (case-branch (caar parts) (cdar parts))))
              (loop (cdr parts)
                    (append ret (case-branch "default" (cdar parts)))))
          (append ret (list "endcase"))))))



;; Declarative statement handler for +sync and +comb.
(define (+stmt v context parts)

  (define (space-n n)
    (make-string n #\ ))

  (define (indent n lines)
    (map (lambda (line) (ss (space-n n) line)) lines))

  (define (stmt-if context parts)
    (let loop ((parts parts)
               (first #t)
               (ret '()))
      (if (pair? parts)
          (loop (cdr parts)
                #f
                (append ret (cons (cond
                                   (first (ss "if ( " (caar parts) " ) begin"))
                                   (else (if (caar parts)
                                             (ss "end else if ( " (caar parts) " ) begin")
                                             (ss "end else begin"))))
                                  (indent (vlogmod-indent-step v) (stmt context (cdar parts))))))
          (append ret (list "end")))))

  (define (stmt-case context case-var parts)
    (let* ((case-indent (space-n (1- (vlogmod-indent-step v))))
           (body-indent-cnt (1- (* 2 (vlogmod-indent-step v))))
           (expand-condition (lambda (condition)
                               (cond
                                ((list? condition) (string-join condition ", "))
                                (else condition))))
           (case-branch (lambda (condition stmts)
                          (append (list (ss case-indent (expand-condition condition) ": begin"))
                                  (indent body-indent-cnt (stmt context stmts))
                                  ;;                                (map (lambda (stmt) (ss body-indent stmt)) stmts)
                                  (list (ss case-indent "end"))))))
      (let loop ((parts parts)
                 (ret (list (ss "case ( " case-var " )"))))
        (if (pair? parts)
            (if (caar parts)
                (loop (cdr parts)
                      (append ret (case-branch (caar parts) (cdar parts))))
                (loop (cdr parts)
                      (append ret (case-branch "default" (cdar parts)))))
            (append ret (list (ss "endcase")))))))

  (define (stmt-assign context parts)
    (list (ss (first parts)
              (if (eq? context 'sync)
                  " <= "
                  " = ")
              (second parts)
              ";")))

  (define (stmt context parts)
    (let loop ((parts parts)
               (ret '()))
      (if (pair? parts)
          (loop (cdr parts)
                (append ret
                        (cond
                         ((string? (car parts)) (list (car parts)))
                         (else (if (symbol? (caar parts))
                                   (case (caar parts)
                                     ((stmt-if) (stmt-if context (cdar parts)))
                                     ((stmt-case) (stmt-case context (cadar parts) (cddar parts)))
                                     ((stmt-assign) (stmt-assign context (cdar parts))))
                                   (car parts))))))
          ret)))

  (stmt context parts))


;; Add line(s) to header.
(define (+header v line) (vlogmod-header-set! v (append (vlogmod-header v) (if (list? line) line (list line)))))

;; Set header content.
(define (=header v lines) (vlogmod-header-set! v lines))

;; Set indent.
(define (=indent v indent) (vlogmod-indent-step-set! v indent))

;; Output <vlogmod> with codeprinter ("pp").
(define (/output v pp)

  (define (output-vardefs lst)
    (when (pair? lst)
      (pp 'p)
      (for-each (lambda (i) (pp 'p (vardef i))) lst)))

  (define (re-indent-line line)
    (define (n-space n) (make-string n #\ ))
    (let ((space-body (re-matches "^([ ]+)([^ ]+.*)$" line)))
      (if space-body
          (let* ((space (list-ref space-body 1))
                 (body (list-ref space-body 2))
                 (n (quotient (string-length space) default-indent-step))
                 (r (remainder (string-length space) default-indent-step))
                 (new-space (string-append (n-space (* (vlogmod-indent-step v) n))
                                           (n-space r))))
            (string-append new-space body))
          line)))

  (pp 'm (vlogmod-indent-step v))

  (for-each (lambda (header-line)
              (pp 'p header-line))
            (vlogmod-header v))
  (pp 'p (ss "module " (vlogmod-name v)))
  (pp 'p (ss (make-string (1- (vlogmod-indent-step v)) #\ ) "("))
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
              (pp 'p (re-indent-line body-line)))
            (vlogmod-body v))

  (pp 'd 'p)
  (pp 'p "endmodule"))


(define (/output-file v filename)
  (let ((pp (codeprint-open filename)))
    (/output v pp)
    (codeprint-close pp)))


;; Build mode from declarion.
(define (/build spec)
  (let ((v (aif (assoc-ref spec 'module)
                (/create it)
                #f))
        (pp (aif (assoc-ref spec 'file)
                 (codeprint-open it)
                 #f)))
    (if (and v pp)
        (begin (for-each (lambda (entry)
                           (case (car entry)
                             ((file) #f)
                             ((module) #f)
                             ((header) (=header v (cdr entry)))
                             ((vars) (for-each (lambda (vardef)
                                                 (let ((tag (car vardef))
                                                       (data (cdr vardef)))
                                                   (for-each (lambda (data)
                                                               (if (list? data)
                                                                   (apply +var (append (list v tag) data))
                                                                   (apply +var (list v tag data))))
                                                             (if (list? (car data))
                                                                 (car data)
                                                                 (list data)))))
                                               (cdr entry)))
                             ((body) (for-each (lambda (item)
                                                 (cond
                                                  ((string? item) (apply +body (list v item)))
                                                  (else (case (car item)
                                                          ((sync) (apply +sync (cons v (cdr item))))
                                                          ((comb) (apply +comb (cons v (cdr item))))))))
                                               (cdr entry)))))
                         spec)
               (/output v pp)
               (codeprint-close pp))
        (error "vlogmod: Specification is missing file or module" spec))))


#;
(pr
 (with-output-to-string
   (lambda ()
     (let ((pp (codeprint-open "<stdout>")))
       (define v (/create "my_mod"))
       (=header v (list "// This is my module."
                        "`include \"my_inc.v\""
                        ""))

       (+var v 'clock "clk")
       (+var v 'reset "rstn")
       (+var v 'input "init")
       (+var v 'input "en")
       (+var v '(output reg) "count" 3)
       (+var v 'param "my_par" 13)
       (+var v 'comb "combi" 10)
       (+var v 'wire "wirei")
       (+var v 'tie "tiei" 10 12)

       (+comb v
              "foobar <= 1;"
              `(stmt-case "count"   ; Use default (as is the default).
                          ("0" "count <= 0;")
                          ("1" "count <= count + 1;")
                          (#f "count <= count;"))
              `(stmt-if ("init" (stmt-if ("init" "count <= 0;")
                                         (#f "count <= 0;")))
                        ("end" (stmt-case "count" ; Use default (as is the default).
                                          (("0" "2") "count <= 0;")
                                          ("1" "count <= count + 1;")
                                          (#f "count <= count;")))
                        (#f "count <= count + 1;"))
              `(stmt-if ("init" "count <= 0;")
                        (#f "count <= 1;"))
              )

       (+sync v
              #f
              `(stmt-if ("init" "count <= 0;")
                        ("end" "count <= count + 1;")))

       (/output v pp)
       (codeprint-close pp))
     )))


#;
(/build
 `((file   . "<stdout>")
   (module . "my_mod")
   (header . ("// This is my module."
              "`include \"my_inc.v\""
              ""))
   (vars . ((clock "clk")
            (reset "rstn")
            (input ("init" "en"))
            ((output reg) (("count" 3)
                           ("foobar" 2)))
            (param "my_par" 13)
            (comb  "combi" 10)
            (wire  "wirei")
            (tie   "tiei" 10 12)
            ))
   ,(cons 'body (append (list
                         (list 'sync
                               (list (cons 'clock "clk")
                                     (cons 'reset "rstn")
                                     (cons 'regs (list 'inc "count")))
                               (list 'stmt-if
                                     (list "init" '(stmt-assign "count" "0"))
                                     (list "en" '(stmt-assign "count" "count + 1")))
                               (list 'stmt-assign "foobar" "12"))
                         (list 'comb
                               (list 'stmt-if
                                     (list "init" '(stmt-assign "count" "0"))
                                     (list "en" '(stmt-assign "count" "count + 1")))
                               '(stmt-assign "foobar" "12")))
                      (list ""
                            "always @( posedge clk or negedge rstn ) begin"
                            "   if ( !rstn) begin"
                            "      count <= 16'b0;"
                            "   end else begin"
                            "      if ( init ) begin"
                            "         count <= 0;"
                            "      end else if ( en ) begin"
                            "         count <= count + 1;"
                            "      end"
                            "   end"
                            "end")))))
