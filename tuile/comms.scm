;;;; Author: Tero Isannainen, Siruco Oy
;;;;
;;;; Copyright (c) Siruco Oy, 2024
;;;; All rights reserved.


;;; module:
;;
;; Provide server and client communications.
;;
;; Note: Sending and receiving is performed with write/read scheme
;; functions. This means that the communication channel is carrying
;; scheme objects and not (just) strings. Since strings are also a
;; scheme objects, we are effectively using a superset of what is
;; possible with just strings. Therefore, don't send data with
;; "display", but with "write", or simply use comms-port-send and
;; comms-port-recv.
;;
;;
;; Port reservations:
;; * 41120 : comms default
;; * 41130 : giru
;; * 41140 : gump3
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

            comms-port-send
            comms-port-recv

            comms-make-descriptor

            comms-generic-server-start
            comms-generic-client-send
            comms-generic-client-send-recv
            ))


(use-modules (tuile pr))
(use-modules (ice-9 threads))


;; ------------------------------------------------------------
;; Utilities:

;; INET connection to server. Return socket, or #f for failure.
(define (connect-to-server addr-port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (if sock
        (with-exception-handler (lambda (exn)
                                  ;; (display (si "comms: No server!") (current-error-port))
                                  ;; (newline (current-error-port))
                                  #f)
          (lambda ()
            (connect sock AF_INET (inet-pton AF_INET (car addr-port)) (cdr addr-port))
            sock)
          #:unwind? #t)
        #f)))


(define (port->socket-path port)
  (si "/dev/shm/tuile-comms-socket-#{port}"))


(define (generic-connect-to-server comms-descriptor)
  (let ((sock-connect-fn
         (case (assoc-ref comms-descriptor 'protocol)
           ((internet)
            (let ((sock (socket PF_INET SOCK_STREAM 0)))
              (if sock
                  (cons sock
                        (lambda ()
                          (connect sock
                                   AF_INET
                                   (inet-pton AF_INET
                                              (assoc-ref comms-descriptor 'address))
                                   (assoc-ref comms-descriptor 'port))
                          sock))
                  #f)))
           ((unix)
            (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
              (if sock
                  (cons sock
                        (lambda ()
                          (connect sock
                                   AF_UNIX
                                   (port->socket-path (assoc-ref comms-descriptor 'port)))
                          sock))
                  #f))))))
    (if sock-connect-fn
        (with-exception-handler (lambda (exn)
                                  ;; (display (si "comms: No server!") (current-error-port))
                                  ;; (newline (current-error-port))
                                  #f)
          (lambda ()
            ((cdr sock-connect-fn))
            (car sock-connect-fn))
          #:unwind? #t)
        #f)))


;; ------------------------------------------------------------
;; API:

(define (comms-default-addr) "127.0.0.1")
(define (comms-default-port) 41120)
(define (comms-default-addr-port) (cons (comms-default-addr) (comms-default-port)))
(define (comms-default-addr-with-port port) (cons (comms-default-addr) port))


(define* (comms-make-descriptor #:key
                                (protocol 'unix)
                                (address (comms-default-addr))
                                (port (comms-default-port)))
  (list (cons 'protocol protocol)
        (cons 'address address)
        (cons 'port    port)))


;; Start internet-server service.
;;
;;     addd-port         Address and port specification.
;;
;;     comms-fn          Server service function (_ <client-port> <server-data>).
;;                       Return #t to continue and #f to close the
;;                       connection.
;;
;;     other-fn          Function for non-server activity (_ <server-data>), or
;;                       #f if not needed. Return #t to continue and #f to
;;                       close the connection.
;;
;;     server-data       Context data for comms-fn.
;;
(define (comms-server-start addr-port comms-fn other-fn server-data)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET (logior SO_REUSEADDR SO_REUSEPORT) 1)
    (bind sock AF_INET (inet-pton AF_INET (car addr-port)) (cdr addr-port))
    (when other-fn
      (fcntl sock F_SETFL (logior O_NONBLOCK
                                  (fcntl sock F_GETFL))))
    (listen sock 5)
    ;; (ppr "server started")
    (let lp ()
      (let* ((server-connection (accept sock)))
        (if server-connection
            (let* ((server-details (cdr server-connection))
                   (server (car server-connection)))
              (let ((ret (comms-fn server server-data)))
                (close server)
                (when ret
                  (lp))))
            (if other-fn
                (let ((ret (other-fn server-data)))
                  (when ret
                    (lp)))
                (lp)))))))


(define (comms-client-send addr-port message)
  (let ((sock (connect-to-server addr-port)))
    (if sock
        (begin
          ;; (ppr (list "client-send" message))
          (comms-port-send sock message)
          (close-port sock)
          #t)
        #f)))


(define (comms-client-send-recv addr-port message)
  (let ((sock (connect-to-server addr-port)))
    (if sock
        (begin
          (comms-port-send sock message)
          (let ((ret (comms-port-recv sock)))
            (close-port sock)
            ret))
        #f)))


(define (comms-port-send port msg)
  (write msg port))


(define (comms-port-recv port)
  (read port))


;; Start a generic-server service.
;;
;;     comms-descriptor  Specification of the generic communication.
;;
;;     comms-fn          Server service function (_ <client-port> <server-data>).
;;                       Return #t to continue and #f to close the
;;                       connection.
;;
;;     other-fn          Function for non-server activity (_ <server-data>), or
;;                       #f if not needed. Return #t to continue and #f to
;;                       close the connection.
;;
;;     server-data       Context data for comms-fn.
;;
(define (comms-generic-server-start comms-descriptor comms-fn other-fn server-data)

  (define (server-sock-cleanup)
    (when (eq? (assoc-ref comms-descriptor 'protocol) 'unix)
      (delete-file (port->socket-path (assoc-ref comms-descriptor 'port)))))

  (let ((sock (case (assoc-ref comms-descriptor 'protocol)
                ((internet) (let ((sock (socket PF_INET SOCK_STREAM 0)))
                              (setsockopt sock SOL_SOCKET (logior SO_REUSEADDR SO_REUSEPORT) 1)
                              (bind sock
                                    AF_INET
                                    (inet-pton AF_INET
                                               (assoc-ref comms-descriptor 'address))
                                    (assoc-ref comms-descriptor 'port))
                              sock))
                ((unix) (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                          (with-exception-handler (lambda (exn) #f)
                            (lambda ()
                              (server-sock-cleanup))
                            #:unwind? #t)
                          (bind sock
                                AF_UNIX
                                (port->socket-path (assoc-ref comms-descriptor 'port)))
                          sock)))))
    (when other-fn
      (fcntl sock F_SETFL (logior O_NONBLOCK
                                  (fcntl sock F_GETFL))))
    (listen sock 5)
    ;; (ppr "server started")
    (let lp ()
      (let* ((client-connection (accept sock)))
        (if client-connection
            (let* ((client-details (cdr client-connection))
                   (client-port (car client-connection)))
              (let ((ret (comms-fn client-port server-data)))
                (close client-port)
                (if ret
                    (lp)
                    (server-sock-cleanup))))
            (if other-fn
                (let ((ret (other-fn server-data)))
                  (if ret
                      (lp)
                      (server-sock-cleanup)))
                (lp)))))))


(define (comms-generic-client-send comms-descriptor message)
  (let ((sock (generic-connect-to-server comms-descriptor)))
    (if sock
        (begin
          ;; (ppr (list "client-send" message))
          (comms-port-send sock message)
          (close-port sock)
          #t)
        #f)))


(define (comms-generic-client-send-recv comms-descriptor message)
  (let ((sock (generic-connect-to-server comms-descriptor)))
    (if sock
        (begin
          (comms-port-send sock message)
          (let ((ret (comms-port-recv sock)))
            (close-port sock)
            ret))
        #f)))
