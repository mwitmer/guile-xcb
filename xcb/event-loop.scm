(define-module (xcb event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml connection)
  #:export (event-loop))

(define-record-type event-loop-data
  (make-event-loop-data-inner
   event-default event-handlers reply-handlers error-handlers)
  event-loop-data?
  (event-default event-default set-event-default!)
  (event-handlers event-handlers)
  (reply-handlers reply-handlers)
  (error-handlers error-handlers))

(define-public unknown-event '(unknown-event))
(define (on-unknown-event event notify) (notify unknown-event))

(define (make-event-loop-data)
  (make-event-loop-data-inner 
   on-unknown-event (make-hash-table)
   (make-hash-table) (make-hash-table)))

(define-public (event-loop-prepare! xcb-conn)
  (set-xcb-connection-data! xcb-conn (make-event-loop-data)))

(define-public (listen-default! xcb-conn proc)
  (set-event-default! (xcb-connection-data xcb-conn) proc))

(define-public (unlisten-default! xcb-conn)
  (set-event-default! (xcb-connection-data xcb-conn) #f))

(define-public (listen! xcb-conn struct proc)
  (hashq-set! (event-handlers (xcb-connection-data xcb-conn)) struct proc))

(define-public (unlisten! xcb-conn struct)
  (hashq-set! (event-handlers (xcb-connection-data xcb-conn)) struct #f))

(define-public (reply-listen! xcb-conn sequence-number reply-proc error-proc)
  (hashv-set!
   (reply-handlers (xcb-connection-data xcb-conn)) sequence-number reply-proc)
  (hashv-set!
   (error-handlers (xcb-connection-data xcb-conn)) sequence-number error-proc))

(define (default-loop-proc) (abort '(forever)))

(define-public solicit abort)

(define* (event-loop xcb-conn #:optional (proc default-loop-proc))
  (define conts (make-hash-table))
  (define early (make-hash-table))
  (define done? (make-parameter #f))
  (define (poll) 
    (xcb-connection-flush! xcb-conn)
    (poll-xcb-connection xcb-conn))
  (define (notify tag val)
    (define cont (hashq-ref conts tag))
    (hashq-remove! conts tag)
    (if cont (cont val) (hashq-set! early tag val)))
  (define (dispatch data-type data)
    (define loop-data (xcb-connection-data xcb-conn))
    (define events (event-handlers loop-data))
    (define default (event-default loop-data))
    (define replies (reply-handlers loop-data))
    (define errors (error-handlers loop-data))
    (define dispatch-proc
      (case data-type 
        ((event) (or (hashq-ref events (xcb-struct data)) default))
        ((reply) (hashv-ref replies (xcb-sequence-number data)))
        ((error) (hashv-ref errors  (xcb-sequence-number data)))))
    (if (and dispatch-proc data) (dispatch-proc (xcb-data data) notify)))
  (define (new-prompt type data) (% (dispatch type data) loop))
  (define (loop cont key)
    (define (on-miss)
      (hashq-set! conts key cont)
      (while (and (not (done?)) (xcb-connected? xcb-conn))
        (call-with-values poll new-prompt)))
    (define early-val (hashq-ref early key))
    (define (use-early-val) (cont early-val) (hashq-remove! early key))
    (if early-val (use-early-val) (on-miss)))
  (% (begin (proc) (done? #t)) loop))

(define-public (xcb-later xcb-conn proc . args)
  (define notify-tag `(xcb-cookie ,proc))
  (define value (make-parameter #f))
  (reply-listen!
   xcb-conn
   (apply proc xcb-conn args)
   (lambda (reply notify) (notify notify-tag reply))
   (lambda (error notify) (notify notify-tag error)))
  notify-tag)

(define-public (xcb-now xcb-conn proc . args)
  (solicit (apply xcb-later xcb-conn proc args)))
