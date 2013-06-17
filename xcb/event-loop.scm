(define-module (xcb event-loop)
  #:use-module ((xcb xml connection) 
                #:select (xcb-listen-default! 
                          xcb-unlisten-default!
                          poll-xcb-connection
                          xcb-listen!
                          xcb-unlisten!))
  #:use-module (ice-9 q)
  #:use-module (ice-9 receive)
  #:re-export (xcb-listen-default!
               xcb-unlisten-default!
               xcb-listen!
               xcb-unlisten!)
  #:export (xcb-await
            xcb-now
            xcb-later
            xcb-event-loop))

(define* (xcb-event-loop xcb-conn sentinel #:optional (defer? #f))
  "Repeatedly poll the X server connected to XCB-CONN for reply,
event, and error thunks until the value of parameter SENTINEL changes
in any way (even to #f).

If DEFER? is #t, reply thunks are evaluted immediately, while event
and error thunks are queued and evaluted after the loop
exits. Otherwise, thunks are evaluated in the order they are
received."
  (define not-ready '(not-ready))
  (define event-q (make-q))
  (define (drain-q! q) (if (not (q-empty? q)) (begin ((deq! q)) (drain-q! q))))
  (parameterize ((sentinel not-ready))
    (while (eq? (sentinel) not-ready)
      (receive (type val) (poll-xcb-connection xcb-conn)
        (case type
          ((event) (if defer? (enq! event-q val) (val)))
          ((error) (val))
          ((reply) (val)))))
    (drain-q! event-q)
    (sentinel)))

(define-syntax xcb-await
  (syntax-rules ()
    ((_ ((reply (proc xcb-conn arg ...))) expr ...)
     (let ((result (make-parameter #f)))
       (add-hook! (proc xcb-conn arg ...) 
                  (lambda (r) (result ((lambda (reply) expr ...) r))))
       (delay (xcb-event-loop xcb-conn result #t))))
    ((_ ((reply (proc xcb-conn arg ...))

         (reply* (proc* xcb-conn* arg* ...)) ...)
        expr ...)
     (let ((inner-result (make-parameter #f))
           (inner-result-wait
            (delay (force (xcb-event-loop xcb-conn inner-result #t)))))
      (add-hook! 
       (proc xcb-conn arg ...)
       (lambda (reply) 
         (inner-result
          (xcb-await ((reply* (proc* xcb-conn* arg* ...)) ...)
            expr ...))))
      (delay (force inner-result-wait))))))

(define-syntax xcb-now
  (syntax-rules ()
    ((_ proc xcb-conn arg ...)
     (force (xcb-later proc xcb-conn arg ...)))))

(define-syntax xcb-later
  (syntax-rules ()
    ((_ proc xcb-conn arg ...)
     (xcb-await ((reply (proc xcb-conn arg ...))) reply))))
