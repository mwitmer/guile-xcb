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

;;; Commentary:

;; These procedures provide an X-specific implementation of (flow
;; event-loop), with helper macros and procedures intended to make it
;; easy to receive replies to X requests, events, and errors both
;; synchronously and asynchronously.

(define-module (xcb event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml)
  #:use-module ((xcb xml xproto) #:select (query-extension))
  #:use-module (flow event-loop)
  #:export (with-replies create-listener
               with-connection loop-with-connection
               screen-info
               create-tagged-listener
               event-loop-prepare!)
  #:re-export (solicit notify make-tag post-to-event-loop notify-map unsolicit))

(define-record-type event-loop-data
  (make-event-loop-data-inner
   event-default event-handlers reply-handlers
   default-error-handler)
  event-loop-data?
  (event-default event-default set-event-default!)
  (event-handlers event-handlers)
  (reply-handlers reply-handlers)
  (default-error-handler default-error-handler set-default-error-handler!))

(define (on-unknown-event event) #f)

(define (basic-error-handler cont . args) (throw 'xcb-error args))

(define (make-event-loop-data)
  (make-event-loop-data-inner
   on-unknown-event (make-hash-table) (make-hash-table)
   basic-error-handler))

(define-public (event-loop-prepared? xcb-conn)
  "-- Scheme Procedure: event-loop-prepared? xcb-conn
     Returns `#t' if XCB-CONN is prepared for use in an event loop."
  (not (not (xcb-connection-data xcb-conn))))

(define* (event-loop-prepare! xcb-conn #:optional error-handler)
  "-- Scheme Procedure: event-loop-prepare! xcb-conn error-handler
     Prepares XCB-CONN for use in an event loop. ERROR-HANDLER is a
     procedure that will be called when the X server sends an error.
     It takes two arguments: RESUME, a procedure that, when called,
     will return control back to the event loop, and DATA, a vector
     returned from `poll-xcb-connection'.

     It is only necessary to call this procedure before calling
     `xcb-event-loop' if ERROR-HANDLER is provided, or if reply/event
     handler procedures are going to be attached to the connection
     before the event loop starts. Otherwise, XCB-CONN will
     automatically be prepared with the default error handler at that
     point."
  (define event-loop-data (make-event-loop-data))
  (if error-handler (set-default-error-handler! event-loop-data error-handler))
  (set-xcb-connection-data! xcb-conn event-loop-data))

(define-public listen-default!
  (case-lambda
    "-- Scheme Procedure: listen-default! [xcb-conn] proc
     Installs PROC as the default procedure to call when an event is
     not otherwise handled by XCB-CONN.

     PROC will be called with the event as its argument; if the event
     is not recognized by the connection, perhaps because it pertains
     to an extension that has not been enabled, a bytevector
     containing the raw binary data from the server will be passed
     instead."
    ((xcb-conn proc)
     (set-event-default! (xcb-connection-data xcb-conn) proc))
    ((proc)
     (set-event-default! (xcb-connection-data (current-xcb-connection)) proc))))

(define-public unlisten-default!
  (case-lambda
    "-- Scheme Procedure: unlisten-default! [xcb-conn]
     Removes the default event handler from XCB-CONN."
    ((xcb-conn)
     (set-event-default!
      (xcb-connection-data xcb-conn)
      on-unknown-event))
    (()
     (set-event-default!
      (xcb-connection-data (current-xcb-connection))
      on-unknown-event))))

(define* (inner-listen! xcb-conn event-struct tag proc #:optional guard)
  (define event-dispatchers (event-handlers (xcb-connection-data xcb-conn)))
  (define previous-dispatcher (hashq-ref event-dispatchers event-struct))
  (define dispatcher (or previous-dispatcher (make-hash-table)))
  (hashq-set! dispatcher tag (if guard (cons guard proc) proc))
  (if (not previous-dispatcher)
   (hashq-set! event-dispatchers event-struct dispatcher)))

(define-public listen!
  (case-lambda
    "-- Scheme Procedure: listen! [xcb-conn] event-type tag proc [guard]
     Add procedure PROC to the list of procedures that may be called
     when an event of type EVENT-TYPE is received by XCB-CONN. PROC
     takes one argument, EVENT, which is an XCB structure
     corresponding to the event from the server.

     TAG is any value that can uniquely identify the procedure,
     making it easier to later remove the procedure from the list. Any
     procedure already associated with TAG for the given EVENT-TYPE
     will be overridden by the new one.

     GUARD, if present, should be a predicate that takes the same
     argument as PROC and returns `#t' if PROC should be called, or
     `#f' if the event does not match some set of criteria and PROC
     should be skipped."
    ((a b c d)
     (define (listen-with-connection xcb-conn struct tag proc)
       (inner-listen! xcb-conn struct tag proc))
     (define (listen-without-connection struct tag proc guard)
       (inner-listen! (current-xcb-connection) struct tag proc guard))
     ((if (xcb-connection? a) listen-with-connection listen-without-connection)
      a b c d))
    ((xcb-conn struct tag proc guard)
     (inner-listen! xcb-conn struct tag proc guard))
    ((struct tag proc)
     (inner-listen! (current-xcb-connection) struct tag proc))))

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
    "-- Scheme Procedure: unlisten! [xcb-conn] event-type tag
     Removes the event handler procedure associated with value TAG
     from the list of event handlers on event type EVENT-TYPE for xcb
     connection XCB-CONN."
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
    "-- Scheme Procedure: reply-listen! [xcb-conn] sequence-number
          reply-proc
     When a reply with SEQUENCE-NUMBER is received by XCB-CONN, calls
     REPLY-PROC with the reply as an argument. The handler is removed
     after it has been called."
    ((xcb-conn sequence-number reply-proc)
     (hashv-set!
      (reply-handlers (xcb-connection-data xcb-conn))
      sequence-number reply-proc))
    ((sequence-number reply-proc)
     (hashv-set!
      (reply-handlers (xcb-connection-data (current-xcb-connection)))
      sequence-number reply-proc))))

(define-public (xcb-event-loop xcb-conn proc)
  "-- Scheme Procedure: xcb-event-loop xcb-conn proc
     Execute thunk PROC in a dynamic scope where
     `current-xcb-connection' is bound to XCB-CONN, and an event loop
     for handling calls to the `solicit'/`notify' procedures is in
     place."
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
        (define dispatch-proc
          (case data-type
            ((event)
             (if (hashq-get-handle events (xcb-struct data))
                 (lambda (r) (dispatch-event events (xcb-struct data) r))
                 default))
            ((reply)
             (and=> (hashv-remove! replies (xcb-sequence-number data)) cdr))
            ((error) (event-loop-error data) #f)
            (else #f)))
        (if (and dispatch-proc data) (dispatch-proc (xcb-data data)) #f))
      (call-with-values poll dispatch))
    (define (finished?) (not (xcb-connected? xcb-conn)))
    (define (on-error . args)
      (if (default-error-handler loop-data)
          (apply (default-error-handler loop-data) args)
          (error args)))
    (do-event-loop dispatch proc on-error)))

(define-public (delay-reply proc . args)
  "-- Scheme Procedure: delay-reply proc . args
     Call XCB request procedure PROC on the current xcb connection
     with arguments ARGS. Returns a tag that, when used in a call to
     `solicit', will yield the corresponding reply for the request. Do
     not use this procedure for a request that does not receive a
     reply from the server, or else the procedure that called it will
     never resume."
  (define notify-tag (make-tag `(xcb-cookie ,proc)))
  (define value (make-parameter #f))
  (reply-listen!
   (apply proc args)
   (lambda (reply) (notify notify-tag reply)))
  notify-tag)

(define-public (reply-for proc . args) 
  "-- Scheme Procedure: reply-for proc . args
     Calls `delay-reply' on the given arguments and then immediately
     solicits and returns the reply."
  (solicit (apply delay-reply proc args)))

(define-syntax-rule (with-connection xcb-conn stmt ...)
  "-- Scheme Syntax: with-connection xcb-conn stmt ...
     Creates a thunk with body STMT ... and runs an event loop until
     the thunk is completed."
  (xcb-event-loop xcb-conn (lambda () stmt ...)))

(define-syntax-rule (loop-with-connection xcb-conn stmt ...)
  "-- Scheme Syntax: loop-with-connection xcb-conn stmt ...
     Creates a thunk with body STMT ... and runs an event loop until
     the thunk is completed and the xcb connection is also
     disconnected."
  (xcb-event-loop 
   xcb-conn
   (lambda () 
     stmt ... (while (xcb-connected? (current-xcb-connection)) 
                (event-loop-tick)))))

(define (verify-fields event . plist)
  (let verify ((plist plist))
    (if (null? plist) #t
        (let ((val1 (xref-string event (keyword->symbol (car plist))))
              (val2 (cadr plist)))
          (if (xcb= val1 val2) (verify (cddr plist)) #f)))))

(define-syntax make-listener  
  (syntax-rules (=>)
    ((_ event-struct tag extraguard ... (guard ...) => proc)
     (listen! event-struct tag
              proc
              (lambda (name) (verify-fields name extraguard ... guard ...))))
    ((_ event-struct tag extraguard ... () => proc)
     (listen! event-struct tag proc))
    ((_ event-struct tag name (guard guard* ...) expr expr* ...)
     (listen! event-struct tag
              (lambda (name) expr expr* ...)
              (lambda (name) (verify-fields name guard guard* ...))))
    ((_ event-struct tag name () expr expr* ...)
     (listen! event-struct tag (lambda (name) expr expr* ...)))))

(define-syntax create-listener  
  (syntax-rules ()
    "-- Scheme Syntax: create-listener (stop! reset! reset-expr ...)
          ((event-struct event #:field value ...) body body* ...) ...
 -- Scheme Syntax: create-listener (stop! reset! reset-expr ...)
          ((event-struct #:field value ...) => proc) ...
     Adds a set of event listeners to the current xcb connection. Each
     EVENT-STRUCT is an XCB struct type for an event and EVENT is
     bound to the event within the accompanying expressions BODY
     BODY* ....

     Alternatively, EVENT may be ommitted and the BODY expressions
     replaced with `=> PROC', with PROC begin a one-argument
     procedure that the event will be passed to. A single
     `create-listener' expression may have a mix of the two forms.

     #:FIELD is a keyword for a field of the event struct (eg.
     #:window, #:event, #:count, etc.), and VALUE is the required
     value for that field. There can be any number of #:FIELD/VALUE
     pairs. The body expressions will only be evaluted if the fields
     of the event match the required values.

     STOP! is the name for a procedure that can be called anywhere in
     a body expression that removes the listeners from the connection.

     RESET! is the name for a procedure that executes the expressions
     RESET-EXPR ... and then reinstates the listeners. It is called
     once to start the listeners when the macro is first evaluated.

     The macro returns STOP! and RESET! as two values."
    ((_ (stop! reset! reset-expr ...)
        ((event-struct name guard ...) body body* ...) ...)
     (let ((tag (make-tag `listener)))
      (create-tagged-listener tag (stop! reset! reset-expr ...)
        ((event-struct name guard ...) body body* ...) ...)))
    ((_ (stop!) ((event-struct name guard ...) body body* ...) ...)
     (create-listener
         (stop! reset!) ((event-struct name guard ...) body body* ...) ...))
    ((_ () ((event-struct name guard ...) body body* ...) ...)
     (create-listener
         (stop! reset!) ((event-struct name guard ...) body body* ...) ...))))

(define-syntax create-tagged-listener  
  (syntax-rules ()
    "-- Scheme Syntax: create-tagged-listener tag ...
     Identitical to `create-listener' besides requiring an additional
     argument TAG, which is used as the tag argument in
     `listen!'/`unlisten!'.

     Use `create-tagged-listener' if newly created listeners are meant
     to replace pre-existing ones with the same tag."
    ((_ tag (stop! reset! reset-expr ...)
        ((event-struct name guard ...) body body* ...) ...)
     (letrec* ((stop! (lambda () (unlisten! event-struct tag) ...))
               (reset!
                (lambda ()
                  reset-expr ...
                  (make-listener
                   event-struct tag name (guard ...) body body* ...) ...)))
       (reset!) (values stop! reset!)))
    ((_ tag (stop!) ((event-struct name guard ...) body body* ...) ...)
     (create-tagged-listener tag
         (stop! reset!) ((event-struct name guard ...) body body* ...) ...))
    ((_ tag () ((event-struct name guard ...) body body* ...) ...)
     (create-tagged-listener tag
         (stop! reset!) ((event-struct name guard ...) body body* ...) ...))))

(define-syntax-rule (with-replies ((reply proc arg ...) ...) stmt stmt* ...)
  (let* ((pre-notify-tag (notify-map (list (delay-reply proc arg ...) ...)))
         (notify-tag (make-tag '(with-replies (reply <- proc) ...)))
         (on-complete (lambda (reply ...) stmt stmt* ...))
         (when-done
          (lambda (replies)
            (notify notify-tag (apply on-complete replies)))))
    (solicit pre-notify-tag when-done)
    notify-tag))

(define-syntax-rule (with-replies ((reply proc arg ...) ...) stmt stmt* ...)
  "-- Scheme Syntax: with-replies ((reply proc arg ...) ...) stmt stmt*
          ...
     Calls each XCB request procedure PROC with its associated ARG
     list as arguments. When all of the replies are received, they
     are each bound to the corresponding symbol REPLY and used as
     arguments to call a procedure with body STMT STMT* ... .

     Returns a tag that can be used in a call to `solicit' to wait for
     the procedure to execute and then return its result."
  (let* ((pre-notify-tag (notify-map (list (delay-reply proc arg ...) ...)))
         (notify-tag (make-tag '(with-replies (reply <- proc) ...)))
         (on-complete (lambda (reply ...) stmt stmt* ...))
         (when-done
          (lambda (replies)
            (notify notify-tag (apply on-complete replies)))))
    (solicit pre-notify-tag when-done)
    notify-tag))

(define-public (delay-enable-extension extension-key)
  "-- Scheme Procedure: delay-enable-extension extension-key
     Sends a `query-extension' request to the server and, if the
     extension is present, enables that extension for the current xcb
     connection when the reply arrives. Returns a tag for use with
     `solicit' to defer to the event loop until the reply arrives."
  (define extension-info (get-extension-info extension-key))
  (with-replies ((query query-extension (car extension-info)))
    ((cdr extension-info) query)))
