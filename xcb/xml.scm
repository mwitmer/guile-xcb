(define-module (xcb xml)
  #:use-module ((xcb xml connection)
                #:select (xcb-connected?
                          xcb-connection-setup
                          xcb-connection-has-extension?
                          xcb-connection-display
                          poll-xcb-connection))
  #:use-module ((xcb xml doc) #:select (document-full document-brief))
  #:use-module ((xcb xml auth) #:select (xcb-connect! xcb-disconnect!))
  #:use-module ((xcb xml core)
                #:select (make-new-xid
                          xcb->string
                          xcb-bytes->string
                          string->xcb
                          string->xcb2b
                          make-xid))
  #:use-module ((xcb xml union) #:select (xunion-ref))
  #:use-module ((xcb xml struct) #:select (xref xset!))
  #:use-module ((xcb xml enum) #:select (xenum-or xenum-ref xenum-key-ref
                                                  xenum-keys xenum-values))
  #:use-module (xcb xml type)
  #:use-module ((xcb xml records) #:select (make-typed-value typed-value-value))
  #:re-export (make-xid
               poll-xcb-connection
               string->xcb
               string->xcb2b
               xcb->string
               xid=
               xenum-ref
               xenum-key-ref
               xenum-or
               xenum-keys
               xenum-values
               xcb-bytes->string
               document-full
               document-brief
               xcb-connect!
               xcb-disconnect!
               xcb-connected?
               xcb-connection-display
               xcb-connection-has-extension?
               (typed-value-value . xid->integer)
               xunion-ref
               xcb-connection-setup
               xref
               xset!
               make-new-xid))

(define-public (xcb-none xcb-type) (make-typed-value 0 xcb-type))
(define-public xcb-current-time 0)
