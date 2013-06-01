(define-module (xcb xml sample randr)
  #:use-module (xcb xml) 
  #:use-module (xcb xml core) 
  #:use-module (xcb xml type) 
  #:use-module (xcb xml xproto)
  #:use-module ((rnrs base) #:select (vector-for-each))
  #:use-module ((xcb xml ext randr) #:renamer (symbol-prefix-proc 'randr:)))

(define xcb-conn (xcb-connect!))

(force (enable-extension 
        xcb-conn "RANDR" 
        randr:set-extension-opcode! 
        randr:xcb-events 
        randr:xcb-errors))

(define setup (xcb-connection-setup xcb-conn))
(define screen (Setup-get-roots setup 0))
(define root-window (SCREEN-get-root screen))
(define screen-resources (xcb-now (randr:GetScreenResourcesCurrent xcb-conn root-window)))

(vector-for-each 
 (lambda (output)
   (define output-info (xcb-now (randr:GetOutputInfo xcb-conn output xcb-current-time)))
   (format #t "~a\n" (xcb-bytes->string (randr:GetOutputInfo-reply-get-name output-info))))
 (randr:GetScreenResourcesCurrent-reply-get-outputs screen-resources))

(xcb-disconnect! xcb-conn)
