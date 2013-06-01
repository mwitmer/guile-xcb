(define-module (xcb xml sample win)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml xproto))

(define-public (xcb-sample)
  (define xcb-conn (xcb-connect!))
  (define setup (xcb-connection-setup xcb-conn))
  (define root (typed-value-value (vector-ref (Setup-get-roots setup) 0)))
  (define root-window (SCREEN-get-root root))
  (define my-window (make-new-xid xcb-conn WINDOW))
  (define my-gc (make-new-xid xcb-conn GCONTEXT))

  (CreateWindow 
   xcb-conn 24 my-window root-window 0 0 200 200 0 'CopyFromParent 0 CW
   `((BackPixel . ,(SCREEN-get-white_pixel root))
     (EventMask . ,(xcb-enum-or EventMask 'KeyRelease 'KeyPress))))

  (CreateGC xcb-conn my-gc my-window GC 
            `((Foreground . ,(SCREEN-get-black_pixel root))))

  (MapWindow xcb-conn my-window)

  (let ((terminated? (make-parameter #f)))
    (xcb-listen! xcb-conn KeyPress
      (lambda (key-press)
        (format #t "KeyPress: ~a\n" (KeyPress-get-detail key-press))))

    (xcb-listen! xcb-conn KeyRelease
      (lambda (key-release)
        (define keycode (KeyRelease-get-detail key-release))
        (format #t "KeyRelease: ~a\n" keycode)
        (if (= keycode 9) (terminated? #t))))

    (let loop ()
      (poll-xcb-connection xcb-conn)
      (if (not (terminated?)) (loop))))

  (xcb-disconnect! xcb-conn))
