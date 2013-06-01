(define-module (xcb xml auth)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml core)
  #:use-module (xcb xml connection)
  #:export (xcb-connect!))

(define-public (xcb-disconnect! xcb-conn)
  (set-xcb-connection-setup! xcb-conn #f)
  (close-port (xcb-connection-input-port xcb-conn)))

(define* (xcb-connect! #:optional (display-name "0") (hostname (gethostname)))
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

    (define (read-block)
      (define size (get-u8 port))
      (define value (get-bytevector-n port size))
      (get-u8 port)
      value)
    
    (define (read-auth)
      (define entry-type 
        (case (get-u8 port)
          ((0) 'ip)
          ((1) 'hostname)
          (else 'eof)))

      (if (not (eq? entry-type 'eof))
          (begin
            (get-bytevector-n port 2)
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

  (define (xcb-match-auth auths display-name hostname)
    (find (lambda (auth)
            (and
             (if (eq? 'hostname (assq-ref auth 'entry-type))
                 (string= (assq-ref auth 'address) hostname)
                 (equal? (assq-ref auth 'address) hostname))
             (string= (assq-ref auth 'display) display-name)))
          auths))

  (define protocol-major-version 11)
  (define protocol-minor-version 0)
  (define xbase "/tmp/.X11-unix/X")

  (define (wrap-socket sock)
    (define (write! bv start count)
      (let ((out-bv (make-bytevector count)))
        (bytevector-copy! bv start out-bv 0 count)
        (send sock out-bv)))
    (define (read! bv start count)
      (let* ((in-bv (make-bytevector count)) 
             (bytes-read (recv! sock in-bv)))
        (bytevector-copy! in-bv 0 bv start bytes-read)
        bytes-read))
    (define (close) (close-port sock))

    (values
     (make-custom-binary-input-port "xcb-input" read! #f #f close)
     (make-custom-binary-output-port "xcb-output" write! #f #f close)))

  (define (handle-additional-authentication
           xcb-conn auth-method auth-data response)
    (format #t "X server requires additional authentication. Reason: ~a"
            (xcb->string (SetupAuthenticate-get-reason response)))
    (error "xml-xcb: Additional authentication not supported at this time"))

  (define (xcb-setup-unpack port)
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

  (connect sock AF_UNIX (string-append xbase display-name))

  (let ((auth (xcb-match-auth (xcb-get-auths) display-name hostname)))
    (define xcb-conn
      (receive (in out) 
          (wrap-socket sock)
        (receive (buffer get-buffer-bv)
            (open-bytevector-output-port)
          (make-xcb-connection
           in out buffer get-buffer-bv sock (make-hash-table)))))

    (define my-SetupRequest
      (make-SetupRequest
       byte-order
       protocol-major-version
       protocol-minor-version
       (string-length (assq-ref auth 'protocol))
       (bytevector-length (assq-ref auth 'data))
       (string->xcb (assq-ref auth 'protocol))
       (bv->xcb-string (assq-ref auth 'data))))


    (xcb-connection-register-errors xcb-conn xcb-errors 0)
    (xcb-connection-register-events xcb-conn xcb-events 0)

    (xcb-struct-pack 
     SetupRequest my-SetupRequest (xcb-connection-buffer-port xcb-conn))
    (xcb-connection-flush! xcb-conn)

    (let ((reply (xcb-setup-unpack (xcb-connection-input-port xcb-conn))))
      (cond 
       ((Setup? reply)
        (set-xcb-connection-setup! xcb-conn reply)
        (enable-big-requests xcb-conn)

        (enable-xc-misc xcb-conn)
        (set-maximum-request-length! 
         xcb-conn
         (Setup-get-maximum_request_length reply))
        xcb-conn)
       ((SetupFailed? reply) 
        (display (xcb->string (SetupFailed-get-reason reply)))
        #f)
       ((SetupAuthenticate? reply)
        (display "Further authentication required, but not supported."))))))
