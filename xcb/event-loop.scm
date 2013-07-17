(define-module (xcb event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml connection)
  #:use-module (flow event-loop)
  #:export (with-replies
               with-connection loop-with-connection
               event-loop-prepare! xcb-event-loop)
  #:re-export ((abort . solicit) notify make-tag enqueue-async!))

(define-record-type event-loop-data
  (make-event-loop-data-inner
   event-default event-handlers reply-handlers
   error-handlers default-error-handler)
  event-loop-data?
  (event-default event-default set-event-default!)
  (event-handlers event-handlers)
  (reply-handlers reply-handlers)
  (error-handlers error-handlers)
  (default-error-handler default-error-handler set-default-error-handler!))

(define-public unknown-event (make-tag 'unknown-event))
(define-public current-xcb-connection (make-parameter #f))
(define-public (unsolicit tag) (abort tag #f))
(define (on-unknown-event event) (notify unknown-event event))

(define (basic-error-handler cont arg) (throw 'xcb-error arg))

(define (make-event-loop-data)
  (make-event-loop-data-inner
   on-unknown-event (make-hash-table)
   (make-hash-table) (make-hash-table)
   basic-error-handler))

(define (event-loop-prepared? xcb-conn)
  (not (not (xcb-connection-data xcb-conn))))

(define* (event-loop-prepare! xcb-conn #:optional error-handler)
  (define event-loop-data (make-event-loop-data))
  (if error-handler (set-default-error-handler! event-loop-data error-handler))
  (set-xcb-connection-data! xcb-conn event-loop-data))

(define-public listen-default!
  (case-lambda
    ((xcb-conn proc)
     (set-event-default! (xcb-connection-data xcb-conn) proc))
    ((proc)
     (set-event-default! (xcb-connection-data (current-xcb-connection)) proc))))

(define-public unlisten-default!
  (case-lambda
    ((xcb-conn)
     (set-event-default! (xcb-connection-data xcb-conn) #f))
    (()
     (set-event-default! (xcb-connection-data (current-xcb-connection)) #f))))

(define* (inner-listen! xcb-conn event-struct tag proc #:optional guard)
  (define event-dispatchers (event-handlers (xcb-connection-data xcb-conn)))
  (define previous-dispatcher (hashq-ref event-dispatchers event-struct))
  (define dispatcher (or previous-dispatcher (make-hash-table)))
  (hashq-set! dispatcher tag (if guard (cons guard proc) proc))
  (if (not previous-dispatcher)
   (hashq-set! event-dispatchers event-struct dispatcher)))

(define-public listen!
  (case-lambda
    ((a b c d)
     (define (listen-with-connection xcb-conn struct tag proc)
       (inner-listen! xcb-conn struct tag proc))
     (define (listen-without-connection struct tag proc guard)
       (inner-listen! (current-xcb-connection) struct tag proc guard))
     ((if (xcb-connection? a) listen-with-connection listen-without-connection)
      a b c d))
    ((xcb-conn struct tag proc guard) (inner-listen! xcb-conn struct tag proc guard))
    ((struct tag proc) (inner-listen! (current-xcb-connection) struct tag proc))))

(define unlisten-inner!
  (case-lambda
    ((xcb-conn event-struct)
     (define event-dispatchers (event-handlers (xcb-connection-data xcb-conn)))
     (hashq-remove! event-dispatchers event-struct))
    ((xcb-conn event-struct tag)
     (define event-dispatchers (event-handlers (xcb-connection-data xcb-conn)))
     (define dispatcher (hashq-ref event-dispatchers event-struct))
     (if dispatcher (hashq-remove! dispatcher tag)))))

(define-public unlisten!
  (case-lambda
    ((a b)
     (define (unlisten-with-connection xcb-conn struct)
       (unlisten-inner! xcb-conn struct))
     (define (unlisten-without-connection struct tag)
       (unlisten-inner! (current-xcb-connection) struct tag))
     ((if (xcb-connection? a)
          unlisten-with-connection unlisten-without-connection)
      a b))
    ((struct) (unlisten-inner! (current-xcb-connection) struct))
    ((xcb-conn struct tag) (unlisten-inner! xcb-conn struct tag))))

(define (dispatch-event dispatchers event-struct event)
  (define (dispatch dispatcher)
    (let look ((tests (hash-map->list (lambda (k v) v) dispatcher)))
      (if (not (null? tests))
          (begin
            (if (pair? (car tests))
                (if ((caar tests) event) ((cdar tests) event))
                ((car tests) event))
            (look (cdr tests))))))
  (define dispatcher (hashq-ref dispatchers event-struct))
  (and=> dispatcher dispatch))

(define-public reply-listen!
  (case-lambda
    ((xcb-conn sequence-number reply-proc error-proc)
     (hashv-set!
      (reply-handlers (xcb-connection-data xcb-conn))
      sequence-number reply-proc)
     (hashv-set!
      (error-handlers (xcb-connection-data xcb-conn))
      sequence-number error-proc))
    ((sequence-number reply-proc error-proc)
     (hashv-set!
      (reply-handlers (xcb-connection-data (current-xcb-connection)))
      sequence-number reply-proc)
     (hashv-set!
      (error-handlers (xcb-connection-data (current-xcb-connection)))
      sequence-number error-proc))))

(define (default-loop-proc) (abort (make-tag 'forever)))

(define* (xcb-event-loop xcb-conn #:optional (proc default-loop-proc))
  (define loop-data
    (begin
      (if (not (event-loop-prepared? xcb-conn)) (event-loop-prepare! xcb-conn))
      (xcb-connection-data xcb-conn)))
  (parameterize ((current-xcb-connection xcb-conn))
    (define (dispatch)
      (define (poll)
        (if (xcb-connected? xcb-conn)
            (poll-xcb-connection xcb-conn #t)
            (values #f #f)))
      (define (dispatch data-type data)
        (define events (event-handlers loop-data))
        (define default (event-default loop-data))
        (define replies (reply-handlers loop-data))
        (define errors (error-handlers loop-data))
        (define dispatch-proc
          (case data-type
            ((event)
             (if (hashq-get-handle events (xcb-struct data))
                 (lambda (r) (dispatch-event events (xcb-struct data) r))
                 default))
            ((reply) (hashv-ref replies (xcb-sequence-number data)))
            ((error) (hashv-ref errors (xcb-sequence-number data)))
            (else #f)))
        (if (and dispatch-proc data) (dispatch-proc (xcb-data data)) #f))
      (call-with-values poll dispatch))
    (define (finished?) (not (xcb-connected? xcb-conn)))
    (define (after)
      (if (xcb-connected? xcb-conn) (xcb-connection-flush! xcb-conn)))
    (define (on-error) (default-error-handler loop-data))
    (do-event-loop dispatch finished? proc #:after after #:on-error on-error)))

(define-public (delay-reply proc . args)
  (define notify-tag `(xcb-cookie ,proc))
  (define value (make-parameter #f))
  (reply-listen!
   (apply proc (current-xcb-connection) args)
   (lambda (reply) (notify notify-tag reply))
   (lambda (error) (notify (default-error-tag) error)))
  notify-tag)

(define-public (reply-for proc . args) (abort (apply delay-reply proc args)))

(define-syntax with-replies
  (syntax-rules ()
    ((_ ((reply proc arg ...) ...) stmt stmt* ...)
     ((lambda ()
        (define (inner-listen! update call-proc . args)
          (reply-listen!
           (apply call-proc (current-xcb-connection) args)
           (lambda (reply-struct) (update reply-struct))
           (lambda (error-struct) (notify (default-error-tag) error-struct))))
        (with-notifies inner-listen!
                       ((reply proc arg ...) ...) stmt stmt* ...))))))

(define-syntax with-connection
  (syntax-rules ()
    ((_ xcb-conn stmt ...)
     (xcb-event-loop xcb-conn (lambda () stmt ...)))))

(define-syntax loop-with-connection
  (syntax-rules ()
    ((_ xcb-conn stmt ...)
     (xcb-event-loop xcb-conn (lambda () stmt ... (abort (make-tag 'forever)))))))
