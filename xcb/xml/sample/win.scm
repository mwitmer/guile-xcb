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

;;; Simple window creation and event handling sample: create a 200x200
;;; window with white background and display the keycodes of
;;; key-presses and key-releases inside it. Press <ESC> to quit.

(define-module (xcb xml sample win)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto))

(define-public (xcb-sample)
  (loop-with-connection (xcb-connect!)
    (define xcb-conn (current-xcb-connection))
    (define root (xref (xcb-connection-setup xcb-conn) 'roots 0))
    (define root-window (xref root 'root))
    (define my-window (make-new-xid xcb-conn WINDOW))
    (listen! KeyPress-event
             (lambda (key-press notify)
               (define keycode (xref key-press 'detail))
               (format #t "KeyPress: ~a\n" keycode)
               (format #t "KeyRelease: ~a\n" (solicit 'release))
               (if (= keycode 9) (xcb-disconnect! xcb-conn))))
    (listen! KeyRelease-event
             (lambda (key-release notify)
               (notify 'release (xref key-release 'detail))))
    (listen-default!
     (lambda (unknown-event notify)
       (format #t "Unknown-event: ~a\n" unknown-event)))
    (CreateWindow
     xcb-conn 24 my-window root-window 0 0 200 200 0 'CopyFromParent 0 CW
     `((BackPixel . ,(xref root 'white_pixel))
       (EventMask
        . ,(xenum-or EventMask 'KeyRelease 'KeyPress))))
    (MapWindow xcb-conn my-window)))
