(define-module (xcb xml)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml doc)
  #:use-module (xcb xml auth)
  #:use-module (xcb xml core)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml type)
  #:use-module ((xcb xml records) #:select (make-typed-value))
  #:re-export (xcb-connect! 
               xcb-disconnect!
               xcb-connected?
               xcb-listen!
               xcb-connection-display
               xcb-unlisten!
               xcb-event-unlisten-default!
               xcb-error-unlisten-default!
               xcb-event-listen-default!
               xcb-error-listen-default!
               xcb-await
               xcb-now
               xcb-later
               xcb-connection-setup
               make-new-xid
               enable-extension
               poll-xcb-connection
               xcb-enum-get
               xcb-enum-or
               xcb-event-loop
               document-full
               document-brief))

(define-public (xcb-none xcb-type) (make-typed-value 0 xcb-type))
(define-public xcb-current-time 0)



