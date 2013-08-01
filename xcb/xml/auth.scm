 ;; This file is part of Guile XCB.

 ;;    Guile XCB is free software: you can redistribute it and/or modify
 ;;    it under the terms of the GNU General Public License as published by
 ;;    the Free Software Foundation, either version 3 of the License, or
 ;;    (at your option) any later version.

 ;;    Guile XCB is distributed in the hope that it will be useful,
 ;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;;    GNU General Public License for more details.

 ;;    You should have received a copy of the GNU General Public License
 ;;    along with Guile XCB.  If not, see <http://www.gnu.org/licenses/>.

(define-module (xcb xml auth)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml core)
  #:use-module (xcb xml connection)
  #:export (xcb-connect!))

(define* (xcb-connect!
          #:optional (display-name (getenv "DISPLAY"))
          (hostname (gethostname)))
  (define (xcb-get-auths)
    (define auth-file (open-file (getenv "XAUTHORITY") "rb"))

    (define port
      (make-custom-binary-input-port
       "xcb-auth-input"
       (lambda (bv start count)
         (do ((n 0 (1+ n))
              (ch (read-char auth-file) (read-char auth-file)))
             ((or (>= n count) (eof-object? ch)) n)
           (bytevector-u8-set!
            bv (+ start n) (char->integer ch))))
       #f #f (lambda () (close-port port))))

    (define* (read-block)
      (define size
        (bytevector-u16-ref (get-bytevector-n port 2) 0 (endianness big)))

      (define value (get-bytevector-n port size))
      value)

    (define (auth-short bv)
      (+ (* (bytevector-u8-ref bv 0) 256)
         (bytevector-u8-ref bv 1)))

    (define (read-auth)
      (define entry-type
       (case (get-u8 port)
         ((0) 'ip)
         ((1) 'hostname)
         (else 'eof)))

      (if (not (eq? entry-type 'eof))
          (begin
            (get-u8 port)
            (let* ((address (read-block))
                   (display (utf8->string (read-block)))
                   (protocol (utf8->string (read-block)))
                   (data (read-block)))
              `((display . ,display)
                (entry-type . ,entry-type)
                (address . ,(if (eq? entry-type 'ip) address (utf8->string address)))
                (protocol . ,protocol)
                (data . ,data))))
          #f))

    (let next-auth ((auths '()))
      (define auth (read-auth))
      (if auth (next-auth (cons auth auths)) auths)))

  (define (parse-display-name disp)
    (define m (string-match ":([0-9]+).?([0-9]*)" disp))
    `((display . ,(match:substring m 1))
      (screen . ,(match:substring m 2))))

  (define (display-match? xauth-display display-string)
    (string=
     (assq-ref (parse-display-name display-string) 'display)
     xauth-display))

  (define (xcb-match-auth auths display-name hostname)
    (find (lambda (auth)
            (and
             (if (eq? 'hostname (assq-ref auth 'entry-type))
                 (string= (assq-ref auth 'address) hostname)
                 (equal? (assq-ref auth 'address) hostname))
             (display-match? (assq-ref auth 'display) display-name)))
          auths))

  (define protocol-major-version 11)
  (define protocol-minor-version 0)
  (define xbase "/tmp/.X11-unix/X")

  (define (handle-additional-authentication
           xcb-conn auth-method auth-data response)
    (format #t "X server requires additional authentication. Reason: ~a"
            (xcb->string (xref response 'reason)))
    (error "xml-xcb: Additional authentication not supported at this time"))

  (define (xcb-setup-unpack sock)
    (define (read! bv start count)
      (let* ((in-bv (make-bytevector count))
             (bytes-read (recv! sock in-bv)))
        (bytevector-copy! in-bv 0 bv start bytes-read)
        bytes-read))
    (define port (make-custom-binary-input-port "xcb-input" read! #f #f #f))
    (xcb-struct-unpack
     (case (lookahead-u8 port)
       ((0) SetupFailed)
       ((1) Setup)
       ((2) SetupAuthenticate))
     port))

  (define byte-order
    (case (native-endianness)
      ((little) (char->integer #\l))
      ((big) (char->integer #\B))
      (else (error "xml-xcb: Unrecognized byte order." (native-endianness)))))

  (define sock (socket AF_UNIX SOCK_STREAM 0))

  (connect sock AF_UNIX
           (string-append
            xbase
            (assq-ref (parse-display-name display-name) 'display)))

  (let ((auth (xcb-match-auth (xcb-get-auths) display-name hostname)))
    (define xcb-conn
      (receive (buffer get-buffer-bv)
          (open-bytevector-output-port)
        (make-xcb-connection
         buffer get-buffer-bv sock (make-hash-table) display-name)))

    (define my-SetupRequest
      (make-SetupRequest
       byte-order
       protocol-major-version
       protocol-minor-version
       (string-length (assq-ref auth 'protocol))
       (bytevector-length (assq-ref auth 'data))
       (string->xcb (assq-ref auth 'protocol))
       (bv->xcb-string (assq-ref auth 'data))))

    (xcb-enable-xproto! xcb-conn)

    (xcb-struct-pack
     SetupRequest my-SetupRequest (xcb-connection-buffer-port xcb-conn))
    (xcb-connection-flush! xcb-conn)

    (let ((reply (xcb-setup-unpack (xcb-connection-socket xcb-conn))))
      (cond
       ((Setup? reply)
        (set-xcb-connection-setup! xcb-conn reply)
        ;; (enable-big-requests xcb-conn)
        ;; (enable-xc-misc xcb-conn)
        ;; (enable-generic-events xcb-conn)
        (let ((max-length (xref reply 'maximum_request_length)))
          (set-original-maximum-request-length! xcb-conn max-length)
          (set-maximum-request-length! xcb-conn max-length))
        xcb-conn)
       ((SetupFailed? reply)
        (display (xcb->string (xref reply 'reason)))
        #f)
       ((SetupAuthenticate? reply)
        (display "Further authentication required, but not supported."))))))
