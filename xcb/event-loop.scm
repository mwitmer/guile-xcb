(define-module (xcb event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml connection)
  #:export (event-loop with-replies with-connection))

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
(define-public default-error-tag (make-parameter '(error)))
(define-public current-xcb-connection (make-parameter #f))

(define (make-event-loop-data)
  (make-event-loop-data-inner
   on-unknown-event (make-hash-table)
   (make-hash-table) (make-hash-table)))

(define (event-loop-prepared? xcb-conn)
  (not (not (xcb-connection-data xcb-conn))))

(define-public (event-loop-prepare! xcb-conn)
  (set-xcb-connection-data! xcb-conn (make-event-loop-data)))

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

(define-public listen! 
  (case-lambda
    ((xcb-conn struct proc)
     (hashq-set! (event-handlers (xcb-connection-data xcb-conn)) struct proc))
    ((struct proc)
     (hashq-set! (event-handlers (xcb-connection-data (current-xcb-connection)))
                 struct proc))))

(define-public unlisten! 
  (case-lambda
    ((xcb-conn struct)
     (hashq-set! (event-handlers (xcb-connection-data xcb-conn)) struct #f))
    ((struct)
     (hashq-set! (event-handlers (xcb-connection-data (current-xcb-connection)))
                 struct #f))))

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

(define (default-loop-proc) (abort '(forever)))

(define-public solicit abort)

(define* (event-loop xcb-conn #:optional (proc default-loop-proc))
  (define conts (make-hash-table))
  (define early (make-hash-table))
  (define done? (make-parameter #f))
  (define finish-tag '(finished))
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
      (let loop ()
        (define result (call-with-values poll new-prompt))
        (if (and (not (done?)) (xcb-connected? xcb-conn)) (loop) result)))
    (define early-val (hashq-ref early key))
    (define (use-early-val) (cont early-val) (hashq-remove! early key))
    (if early-val
        (use-early-val)
        (on-miss)))
  (if (not (event-loop-prepared? xcb-conn)) (event-loop-prepare! xcb-conn))
  (parameterize ((current-xcb-connection xcb-conn))
    (% ((lambda ()
          (define result (proc)) 
          (done? #t)
          result)) loop)))

(define-public (delay-reply proc . args)
  (define notify-tag `(xcb-cookie ,proc))
  (define value (make-parameter #f))
  (reply-listen!
   (apply proc (current-xcb-connection) args)
   (lambda (reply notify) (notify notify-tag reply))
   (lambda (error notify) (notify (default-error-tag) error)))
  notify-tag)

(define-syntax with-replies
  (syntax-rules ()
    ((_ ((reply proc arg ...) ...)
        stmt stmt* ...)
     (let* ((not-ready '(not-ready))
            (replies (list (cons 'reply (make-parameter not-ready)) ...))
            (notify-tag '(xcb-cookie (reply <- proc) ...))
            (update-and-notify
             (lambda (tag val notify)
               (define eval-param (lambda (param) (apply param '())))
               (define reply-param (assq-ref replies tag))
               (reply-param val)
               (let ((results (map-in-order 
                               eval-param
                               (map-in-order cdr replies))))
                 (if (not (memq not-ready results))
                     (notify notify-tag 
                             (apply (lambda (reply ...) stmt stmt* ...) 
                                    results)))))))
       (reply-listen! (proc (current-xcb-connection) arg ...)
        (lambda (reply-struct notify) 
          (update-and-notify 'reply reply-struct notify))
        (lambda (error-struct notify) 
          (notify (default-error-tag) error-struct)))
       ...
       notify-tag))))

(define-syntax with-connection
  (syntax-rules ()
    ((_ xcb-conn stmt ...)
     (event-loop xcb-conn (lambda () stmt ...)))))
