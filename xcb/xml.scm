(define-module (xcb xml)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml common)
  #:use-module (xcb xml auth)
  #:use-module (xcb xml core)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml type)
  #:re-export (xcb-connect! 
               xcb-disconnect!
               xcb-connected?
               xcb-listen!
               xcb-unlisten!
               xcb-await
               xcb-now
               xcb-connection-setup
               typed-value-value
               typed-value-type
               make-new-xid
               enable-extension
               poll-xcb-connection
               xcb-enum-get
               xcb-enum-or
               document-full
               document-brief))

(define-public xcb-no-symbol 0)
(define-public (xcb-none xcb-type) (make-typed-value 0 xcb-type))
(define-public xcb-current-time 0)



