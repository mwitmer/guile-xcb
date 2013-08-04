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

(define-module (xcb xml connection)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml type)
  #:export (xcb-connection-last-xid
            xcb-connection?
            xcb-connection-buffer-port
            xcb-connection-socket
            xcb-connection-has-extension?
            xcb-connection-use-extension!
            xcb-connection-display
            next-request-number
            set-xcb-connection-last-xid!
	    make-xcb-connection
            poll-xcb-connection
            set-xcb-connection-setup!
            xcb-connection-setup
            get-maximum-request-length
            set-maximum-request-length!
            set-original-maximum-request-length!
            xcb-connection-data set-xcb-connection-data!
            all-events all-errors))

(define generic-event-number 35)
(define max-uint16 (- (expt 2 16) 1))

(define-record-type xcb-connection
  (inner-make-xcb-connection
   buffer-port
   get-bv
   socket
   request-structs
   next-request-number
   last-xid
   events
   errors
   display
   extensions
   mutex)
  xcb-connection?
  (buffer-port xcb-connection-buffer-port set-xcb-connection-buffer-port!)
  (get-bv xcb-connection-get-bv set-xcb-connection-get-bv!)
  (socket xcb-connection-socket)
  (request-structs request-structs)
  (next-request-number next-request-number set-next-request-number!)
  (setup xcb-connection-setup set-xcb-connection-setup!)
  (last-xid xcb-connection-last-xid set-xcb-connection-last-xid!)
  (original-maximum-request-length
   original-maximum-request-length set-original-maximum-request-length!)
  (maximum-request-length maximum-request-length set-maximum-request-length!)
  (events all-events)
  (errors all-errors)
  (extensions xcb-connection-extensions)
  (display xcb-connection-display)
  (data xcb-connection-data set-xcb-connection-data!)
  (mutex xcb-connection-mutex))

(set-record-type-printer!
 xcb-connection
 (lambda (xcb-conn port)
   (if (xcb-connected? xcb-conn)
    (display "#<xcb-connection (connected)>")
    (display "#<xcb-connection (not connected)>"))))

(define-public
  (make-xcb-connection buffer-port get-bv socket request-structs display)
  (inner-make-xcb-connection
   buffer-port get-bv
   socket
   request-structs
   1 0
   (make-hash-table)
   (make-hash-table)
   display
   (make-hash-table)
   (make-mutex)))

(define-public (xcb-disconnect! xcb-conn)
  (set-xcb-connection-setup! xcb-conn #f)
  (close-port (xcb-connection-socket xcb-conn)))

(define-public (xcb-connected? xcb-conn)
  (if (xcb-connection-setup xcb-conn) #t #f))

(define (xcb-connection-has-extension? xcb-conn extension)
  (hashq-ref (xcb-connection-extensions xcb-conn) extension))

(define (xcb-connection-use-extension! xcb-conn extension)
  (hashq-set! (xcb-connection-extensions xcb-conn) extension #t))

(define-public (xcb-connection-register-events xcb-conn event-hash major-opcode)
  (define xcb-conn-events (all-events xcb-conn))
  (define add-event!
    (lambda (h) (hashv-set! xcb-conn-events (+ (car h) major-opcode) (cdr h))))
  (hash-for-each-handle add-event! event-hash))

(define-public (xcb-connection-register-errors xcb-conn error-hash major-opcode)
  (define xcb-conn-errors (all-errors xcb-conn))
  (define add-error!
    (lambda (h) (hashv-set! xcb-conn-errors (+ (car h) major-opcode) (cdr h))))
  (hash-for-each-handle add-error! error-hash))

(define-public (number-for-event xcb-conn event-type)
  (call-with-prompt
   'number-for-event
   (lambda ()
     (hash-map->list
      (lambda (num ev)
        (if (eq? ev event-type) (abort-to-prompt 'number-for-event num)))
      (all-events xcb-conn))
     #f)
   (lambda (cont num) num)))

(define-public (xcb-connection-send xcb-conn major-opcode minor-opcode request)
  (define buffer (xcb-connection-buffer-port xcb-conn))
  (define max-length (maximum-request-length xcb-conn))
  (define length (ceiling-quotient (+ (bytevector-length request) 3) 4))
  (define use-bigreq?
    (and (xcb-connection-has-extension? xcb-conn 'bigreq)
         (> length (original-maximum-request-length xcb-conn))))
  (define has-content? (> (bytevector-length request) 0))
  (define reported-length (if use-bigreq? (+ length 1) length))
  (define mutex (xcb-connection-mutex xcb-conn))
  (define message-length-bv
    (uint-list->bytevector
     (list reported-length)
     (native-endianness) (if use-bigreq? 4 2)))
  (if (and max-length (> length max-length))
      (error "xml-xcb: Request length too long for X server: " length))
  (dynamic-wind
    (lambda () (while (not (try-mutex mutex))))
    (lambda ()
      (put-u8 buffer major-opcode)
      (if minor-opcode (put-u8 buffer minor-opcode)
          (put-u8 buffer (if has-content? (bytevector-u8-ref request 0) 0)))
      (if use-bigreq? (put-bytevector buffer #vu8(0 0)))
      (put-bytevector buffer message-length-bv)
      (when has-content?
        (if minor-opcode
            (put-bytevector buffer request 0)
            (if (> (bytevector-length request) 1)
                (put-bytevector buffer request 1)))
        (if (not minor-opcode)
            (let ((left-over (remainder (+ 3 (bytevector-length request)) 4)))
              (if (> left-over 0)
                  (put-bytevector buffer (make-bytevector left-over 0))))))
      (let ((request-number (next-request-number xcb-conn)))
        (xcb-connection-flush! xcb-conn)
        (set-next-request-number!
         xcb-conn (logand max-uint16 (+ request-number 1)))
        request-number))
    (lambda () (unlock-mutex mutex))))

(define-public (mock-connection server-bytes events errors)
  (receive (buffer-port get-buffer-bytevector)
      (open-bytevector-output-port)
    (let ((conn (make-xcb-connection
                 buffer-port
                 get-buffer-bytevector
                 (open-bytevector-input-port server-bytes)
                 (make-hash-table) #f)))
      (xcb-connection-register-events conn events 0)
      (xcb-connection-register-errors conn errors 0)
      (values conn (lambda () ((xcb-connection-get-bv conn)))))))

(define-public (xcb-connection-register-reply-struct
                xcb-conn sequence-number reply-struct)
  (hashv-set! (request-structs xcb-conn) sequence-number reply-struct))

(define (recv1! sock)
  (define bv (make-bytevector 1))
  (if (file-port? sock)
      (recv! sock bv)
      (bytevector-u8-set! bv 0 (get-u8 sock)))
  (bytevector-u8-ref bv 0))

(define (recv-n! sock n)
  (define bv (make-bytevector n))
  (if (file-port? sock)
      (recv! sock bv)
      (bytevector-copy! (get-bytevector-n sock n) 0 bv 0 n))
  bv)

(define* (poll-xcb-connection xcb-conn #:optional async?)
  (define (unpack-event event-number bv)
    (define event-struct (hashv-ref (all-events xcb-conn) event-number))
    (define event-data
      (if (not event-struct) (cons event-number bv)
          (xcb-struct-unpack-from-bytevector event-struct bv)))
    (vector event-struct event-data #f))

  (define (read-generic-event sock)
    (define extension-opcode (recv1! sock))
    (define sequence-number
      (bytevector-u16-native-ref (recv-n! sock 2) 0))
    (define length
      (bytevector-u32-native-ref (recv-n! sock 4) 0))
    (define event-number
      (bytevector-u16-native-ref (recv-n! sock 2) 0))
    (define rest (recv-n! sock (+ 22 (* 4 length))))
    (unpack-event event-number rest))

  (define (read-event sock event-number)
    (define event-struct (hashv-ref (all-events xcb-conn) event-number))
    (define bv (recv-n! sock 31))
    (unpack-event event-number bv))

  (define (read-reply sock)
    (define first-data-byte (recv1! sock))
    (define sequence-number
      (bytevector-u16-native-ref (recv-n! sock 2) 0))
    (define extra-length (bytevector-u32-native-ref (recv-n! sock 4) 0))
    (define reply-rest (recv-n! sock (+ (* extra-length 4) 24)))
    (define reply-struct
      (hashv-ref (request-structs xcb-conn) sequence-number))
    (define reply-for-struct
      (receive (port get-bytevector)
          (open-bytevector-output-port)
        (let ((length-bv (make-bytevector 4)))
          (bytevector-u32-native-set! length-bv 0 extra-length)
          (put-bytevector port length-bv))
        (put-u8 port first-data-byte)
        (put-bytevector port reply-rest)
        (get-bytevector)))
    (define reply-data
      (xcb-struct-unpack-from-bytevector reply-struct reply-for-struct))
    (vector reply-struct reply-data sequence-number))

  (define (read-error sock)
    (define error-number (recv1! sock))
    (define sequence-number
      (bytevector-u16-native-ref (recv-n! sock 2) 0))
    (define bv (recv-n! sock 28))
    (define error-struct (hashv-ref (all-errors xcb-conn) error-number))
    (define error-data
      (if (not error-struct)
          (cons error-number bv)
          (xcb-struct-unpack-from-bytevector error-struct bv)))
    (vector error-struct error-data sequence-number))


  (define mutex (xcb-connection-mutex xcb-conn))

  (define (file-ready? fd)
    (define do-select
      (lambda () (select (list fd) '() '() 0 50000)))
    (define on-error
      (lambda args
        (if (= (system-error-errno args) EINTR)
            '(() () ())
            (apply throw args))))
    (memq fd (car (catch 'system-error do-select on-error))))

  (define sock (xcb-connection-socket xcb-conn))

  (dynamic-wind
    (lambda () (while (not (try-mutex mutex))))
    (lambda ()
     (receive (data-type data)
         (if (or (not async?) (file-ready? (port->fdes sock)))
             (let ((next-byte (recv1! sock)))
               (if (eof-object? next-byte)
                   (values 'none #f)
                   (case next-byte
                     ((0) (values 'error (read-error sock)))
                     ((1) (values 'reply (read-reply sock)))
                     (else (values 'event
                                   (if (= next-byte generic-event-number)
                                       (read-generic-event sock)
                                       (read-event sock next-byte)))))))
             (values 'none #f))
       (values data-type data)))
    (lambda () (unlock-mutex mutex))))

(define-public (xcb-struct data) (vector-ref data 0))
(define-public (xcb-data data) (vector-ref data 1))
(define-public (xcb-sequence-number data) (vector-ref data 2))

(define-public (xcb-connection-flush! xcb-conn)
  (define bv ((xcb-connection-get-bv xcb-conn)))
  (define port (xcb-connection-socket xcb-conn))
  (receive (new-port get-bv)
      (open-bytevector-output-port)
    (set-xcb-connection-buffer-port! xcb-conn new-port)
    (set-xcb-connection-get-bv! xcb-conn get-bv)
    (if (file-port? port)
        (send port bv)
        (put-bytevector new-port bv))))
