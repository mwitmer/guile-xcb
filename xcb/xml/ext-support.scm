(define-module (xcb xml ext-support)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml type)
  #:use-module ((xcb xml records) #:select (make-typed-value)) 
  #:export (enable-extension))

(define* (enable-extension xcb-conn name set-opcode! 
                           events errors #:optional proc)
  (define (enable opcode reply)
    (set-opcode! opcode)
    (xcb-connection-register-events xcb-conn events opcode)
    (xcb-connection-register-errors xcb-conn errors opcode)
    (if proc (proc reply) #t))
  (xcb-await 
      ((reply (QueryExtension 
               xcb-conn (string-length name) 
               (list->vector 
                (map (lambda (ch) (make-typed-value ch char))
                     (string->list name))))))
    (if (QueryExtension-reply-get-present reply)
        (enable (QueryExtension-reply-get-major_opcode reply) reply) #f)))
