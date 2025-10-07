(define-module (tuile base64)
  #:export
  (
   base64-encode
   base64-decode
   ))


(use-modules (rnrs bytevectors))
(use-modules (srfi srfi-1))


;; (define codec-list (map integer->char
;;                         (append (iota 26 (char->integer #\A))
;;                                 (iota 26 (char->integer #\a))
;;                                 (iota 10 (char->integer #\0))
;;                                 (map char->integer (list #\+ #\/)))))
;;
;; (define (print-encode-table)
;;   (ppr (list->vector codec-list)))
;;
;;
;; (define (print-decode-table)
;;   (let ((table (make-vector 128 #f)))
;;     (let lp ((chars codec-list)
;;              (index 0))
;;       (when (pair? chars)
;;         (let ((ch (car chars)))
;;           (vector-set! table (char->integer ch) index)
;;           (lp (cdr chars)
;;               (1+ index)))))
;;     (ppr table)))


;; Convert from 6 bits to char (8 bits) (note: update with print-encode-table).
(define encode-table #(#\A #\B #\C #\D #\E #\F #\G #\H
                       #\I #\J #\K #\L #\M #\N #\O #\P
                       #\Q #\R #\S #\T #\U #\V #\W #\X
                       #\Y #\Z #\a #\b #\c #\d #\e #\f
                       #\g #\h #\i #\j #\k #\l #\m #\n
                       #\o #\p #\q #\r #\s #\t #\u #\v
                       #\w #\x #\y #\z #\0 #\1 #\2 #\3
                       #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

;; Convert from char (8 bits) to 6 bits (note: update with print-decode-table).
(define decode-table #(#f #f #f #f #f #f #f #f
                          #f #f #f #f #f #f #f #f
                          #f #f #f #f #f #f #f #f
                          #f #f #f #f #f #f #f #f
                          #f #f #f #f #f #f #f #f
                          #f #f #f 62 #f #f #f 63
                          52 53 54 55 56 57 58 59
                          60 61 #f #f #f #f #f #f
                          #f 0 1 2 3 4 5 6
                          7 8 9 10 11 12 13 14
                          15 16 17 18 19 20 21 22
                          23 24 25 #f #f #f #f #f
                          #f 26 27 28 29 30 31 32
                          33 34 35 36 37 38 39 40
                          41 42 43 44 45 46 47 48
                          49 50 51 #f #f #f #f #f))



(define (bex b msb lsb)
  (bit-extract b lsb (1+ msb)))
(define (get-ch value)
  (vector-ref encode-table value))

(define (encode-full-group bytes)
  (string (get-ch (bex (first bytes) 7 2))
          (get-ch (+ (ash (bex (first bytes) 1 0) 4)
                     (bex (second bytes) 7 4)))
          (get-ch (+ (ash (bex (second bytes) 3 0) 2)
                     (bex (third bytes) 7 6)))
          (get-ch (bex (third bytes) 5 0))))


(define (encode-group bytes)
  (case (length bytes)
    ;; A        B        C
    ;; 76543210 76543210 76543210
    ;; 54321|54 321|5432 1|543210
    ;; a     b      c      d
    ;;
    ;; A        B        C
    ;; 76543210 76543210 76543210
    ;; 54321054 32105432 10543210
    ;; a     b      c      d
    ((3) (encode-full-group bytes))
    ((2) (string (get-ch (bex (first bytes) 7 2))
                 (get-ch (+ (ash (bex (first bytes) 1 0) 4)
                            (bex (second bytes) 7 4)))
                 (get-ch (+ (ash (bex (second bytes) 3 0) 2)
                            0))
                 #\=))
    ((1) (string (get-ch (bex (first bytes) 7 2))
                 (get-ch (+ (ash (bex (first bytes) 1 0) 4)
                            0))
                 #\=
                 #\=))))


;; Encode u8 bytevector worth of data.
(define (base64-encode bvec)

  (define br bytevector-u8-ref)

  (define (rounded-quotient count modulo)
    (if (= count 0)
        0
        (1+ (quotient (1- count) modulo))))

  (define (string-write-4! str oi sub)
    (string-set! str (+ oi 0) (string-ref sub 0))
    (string-set! str (+ oi 1) (string-ref sub 1))
    (string-set! str (+ oi 2) (string-ref sub 2))
    (string-set! str (+ oi 3) (string-ref sub 3)))

  (let* ((blen (bytevector-length bvec))
         (encoded-bytes (* 4 (rounded-quotient blen 3)))
         (encoded-lines (rounded-quotient encoded-bytes 76))
         (newline-count (1- encoded-lines))
         (encoded-size (+ encoded-bytes newline-count))
         (output (make-string encoded-size))
         (blimit (* 3 (quotient blen 3))))
    (let lp ((bi 0)
             (oi 0))
      (if (< bi blimit)
          (let ((dump (encode-full-group (list (br bvec (+ bi 0))
                                               (br bvec (+ bi 1))
                                               (br bvec (+ bi 2))))))
            (string-write-4! output oi dump)
            (if (= (remainder (+ (* (/ bi 3) 4) 4) 76) 0)
                (begin
                  (string-set! output (+ oi 4) #\newline)
                  (lp (+ bi 3)
                      (+ oi 5)))
                (lp (+ bi 3)
                    (+ oi 4))))
          (if (= blimit blen)
              #t
              (let ((dump (encode-group (if (= (+ blimit 1) blen)
                                            (list (br bvec (+ bi 0)))
                                            (list (br bvec (+ bi 0))
                                                  (br bvec (+ bi 1)))))))
                (string-write-4! output oi dump)))))
    output))


(define (base64-decode str)

  (define (decode ch)
    (case ch
      ((#\=) #t)
      ((#\newline) #f)
      ((#\space) #f)
      (else (vector-ref decode-table (char->integer ch)))))

  ;; v    nib
  ;; ai   index of a to v
  ;; bi   index of b to v
  ;; am   msb of a
  ;; al   lsb of a
  ;; bm   msb of b
  ;; bl   lsb of b
  (define (bsel v ai am al bi bm bl)
    (logior (ash (bit-extract (vector-ref v ai) al (1+ am))
                 (1+ (- bm bl)))
            (bit-extract (vector-ref v bi) bl (1+ bm))))

  (let* ((slen (string-length str))
         ;; Make temporary bvec size sufficient.
         (bvec (make-bytevector slen))
         (fill 0))

    ;; Fill bvec with decoded data and copy it correctly sized
    ;; bytevector as result to be returned.
    (let lp ((si 0)
             (bi 0))

      (define (bs! i value)
        (bytevector-u8-set! bvec (+ bi i) value))

      (if (< si slen)
          (let ((next-si
                 (let ((nib (make-vector 4)))
                   ;; Fill 3-byte segment with decoded data.
                   (let lp2 ((si si)
                             (ni 0))
                     (if (< ni 4)
                         (let ((val (decode (string-ref str si))))
                           (cond
                            ((number? val)
                             (vector-set! nib ni val)
                             (lp2 (1+ si) (1+ ni)))
                            (val
                             (set! fill (1+ fill))
                             (vector-set! nib ni 0)
                             (lp2 (1+ si) (1+ ni)))
                            (else
                             (lp2 (1+ si) ni))))
                         (begin
                           ;; A        B        C
                           ;; 76543210 76543210 76543210
                           ;; 54321054 32105432 10543210
                           ;; a     b      c      d
                           (bs! 0 (bsel nib 0 5 0 1 5 4))
                           (bs! 1 (bsel nib 1 3 0 2 5 2))
                           (bs! 2 (bsel nib 2 1 0 3 5 0))
                           si))))))
            (lp next-si
                (+ bi 3)))
          (let ((res (make-bytevector (- bi fill))))
            (bytevector-copy! bvec 0 res 0 (- bi fill))
            res)))))


;; (use-modules (tuile pr))
;; (define str "fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR12fooBAR1")
;; (define encoded (base64-encode (string->utf8 str)))
;; (pr encoded)
;; (define decoded (base64-decode encoded))
;; (pr (utf8->string decoded))
