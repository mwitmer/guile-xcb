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

(define-module (xcb xml sample win)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml)
  #:use-module (xcb xml connection)
  #:use-module (xcb xml xproto))

(define-public (xcb-sample)
  (define xcb-conn (xcb-connect!))
  (define setup (xcb-connection-setup xcb-conn))
  (define root  (Setup-get-roots setup 0))
  (define root-window (SCREEN-get-root root))
  (define my-window (make-new-xid xcb-conn WINDOW))
  (define my-gc (make-new-xid xcb-conn GCONTEXT))
  (define terminated? (make-parameter #f))

  (CreateWindow 
   xcb-conn 24 my-window root-window 0 0 200 200 0 'CopyFromParent 0 CW
   `((BackPixel . ,(SCREEN-get-white_pixel root))
     (EventMask . ,(xcb-enum-or EventMask 'KeyRelease 'KeyPress))))

  (CreateGC xcb-conn my-gc my-window GC 
            `((Foreground . ,(SCREEN-get-black_pixel root))))

  (MapWindow xcb-conn my-window)

  (xcb-listen! xcb-conn KeyPress
    (lambda (key-press)
      (format #t "KeyPress: ~a\n" (KeyPress-get-detail key-press))))

  (xcb-listen! xcb-conn KeyRelease
    (lambda (key-release)
      (define keycode (KeyRelease-get-detail key-release))
      (format #t "KeyRelease: ~a\n" keycode)
      (if (= keycode 9) (terminated? #t))))

  (xcb-event-loop xcb-conn terminated? #f)

  (xcb-disconnect! xcb-conn))
