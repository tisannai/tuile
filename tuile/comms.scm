;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2024
;;;; All rights reserved.


;;; module:
;;
;; Provide server and client communications
;;
;;
(define-module (tuile comms)
  #:export (
            comms-default-addr
            comms-default-port
            comms-default-addr-port
            comms-default-addr-with-port

            comms-server-start
            comms-client-send
            comms-client-send-recv
            ))


(use-modules (tuile pr))
(use-modules (ice-9 threads))


;; ------------------------------------------------------------
;; Utilities:

(define (connect-to-server addr-port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (if sock
        (with-exception-handler (lambda (exn)
                                  (display (si "comms: No server!") (current-error-port))
                                  (newline (current-error-port))
                                  #f)
          (lambda ()
            (connect sock AF_INET (inet-pton AF_INET (car addr-port)) (cdr addr-port))
            sock)
          #:unwind? #t)
        #f)))


;; ------------------------------------------------------------
;; API:

(define (comms-default-addr) "127.0.0.1")
(define (comms-default-port) 41140)
(define (comms-default-addr-port) (cons "127.0.0.1" 41140))
(define (comms-default-addr-with-port port) (cons "127.0.0.1" port))


(define (comms-server-start addr-port client-fn client-data)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET (logior SO_REUSEADDR SO_REUSEPORT) 1)
    (bind sock AF_INET (inet-pton AF_INET (car addr-port)) (cdr addr-port))
;;     (fcntl sock F_SETFL (logior O_NONBLOCK
;;                                 (fcntl sock F_GETFL)))
    (listen sock 5)
    ;; (ppr "server started")
    (let lp ()
      (let* ((client-connection (accept sock)))
        (if client-connection
            (let* ((client-details (cdr client-connection))
                   (client (car client-connection)))
              (let ((ret (client-fn client client-data)))
                (close client)
                (when ret
                  (lp))))
            (lp))))))


(define (comms-client-send addr-port message)
  (let ((sock (connect-to-server addr-port)))
    (if sock
        (begin
          ;; (ppr (list "client-send" message))
          (write message sock)
          (close-port sock)
          #t)
        #f)))


(define (comms-client-send-recv addr-port message)
  (let ((sock (connect-to-server addr-port)))
    (if sock
        (begin
          (write message sock)
          (let ((ret (read sock)))
            (close-port sock)
            ret))
        #f)))
