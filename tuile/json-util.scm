(define-module (tuile json-util)
  #:use-module (tuile massoc)
  #:use-module (json)
  #:use-module ((srfi srfi-43) #:select (vector-map))
  #:use-module ((srfi srfi-1) #:select (first second third))
  #:re-export (scm->json-string json-string->scm)
  #:export
  (
   json-load
   json-dump

   scm-param->json-param
   json-param->scm-param

   json-rpc-notify
   json-rpc-request
   json-rpc-result
   json-rpc-decode
   json-rpc-decode-as-list

   ))


;; ------------------------------------------------------------
;; JSON file operations:

;; Load JSON file.
(define (json-load filename)
  (with-input-from-file filename
    (lambda ()
      (json->scm))))


;; Dump data to JSON file using pretty formatting.
(define (json-dump filename data)
  (with-output-to-file filename
    (lambda ()
      (scm->json data #:pretty #t))))


;; ------------------------------------------------------------
;; JSON RPC operations:

(define (scm-param->json-param param)
  (cond
   ((list?   param) (list->vector (map scm-param->json-param param)))
   ((vector? param) (vector-map (lambda (i p) (scm-param->json-param p)) param))
   (else param)))

(define (json-param->scm-param param)
  (cond
   ((vector? param) (map json-param->scm-param (vector->list param)))
   (else param)))

(define json-rpc-message
  (lambda (method params id)
    (cond
     ((and (pair? params) id) (list (cons "jsonrpc" "2.0")
                                    (cons "method" method)
                                    (cons "params" (car params))
                                    (cons "id" id)))
     ((pair? params) (list (cons "jsonrpc" "2.0")
                           (cons "method" method)
                           (cons "params" (car params))))
     (id (list (cons "jsonrpc" "2.0")
               (cons "method" method)
               (cons "id" id)))
     (else (list (cons "jsonrpc" "2.0")
                 (cons "method" method))))))

;; Send json-rpc notification, i.e. message without id and response request.
(define json-rpc-notify
  (lambda (method . params)
    (scm->json-string (json-rpc-message method params #f))))

;; Send json-rpc request, i.e. message with id and response request.
(define json-rpc-request
  (let ((id 0))
    (lambda (method . params)
      (let ((cur-id id))
        (set! id (if (< id 128) (1+ id) 0))
        (scm->json-string (json-rpc-message method params cur-id))))))

;; Send json-rpc result.
(define (json-rpc-result result id)
  (scm->json-string (list (cons "jsonrpc" "2.0")
                          (cons "result" result)
                          (cons "id" id))))

(define (json-rpc-decode msg)
  (let* ((data (json-string->scm msg)))
    ;; Drop the protocol label.
    (assoc-remove! data "jsonrpc")))

(define (json-rpc-decode-as-list msg)
  (let loop ((data (json-rpc-decode msg)))
    (if (pair? data)
        (cons (cond
               ((string=? (caar data) "result") (cons "result" (json-param->scm-param (cdar data))))
               ((string=? (caar data) "params") (cons "params" (json-param->scm-param (cdar data))))
               (else (car data)))
              (loop (cdr data)))
        '())))
