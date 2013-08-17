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

;;; tinywm implementation for Guile XCB
;;;
;;; Press Alt-F1 to send the current window to the back
;;; Hold Alt-Left Mouse Button to drag the window around
;;; Hold Alt-Right Mouse Button to resize the window
;;; Ctrl-Alt-q to Quit
;;;
;;; To start a program, use the function `wm-shell-command' in the
;;; REPL that appears at startup with the name of the command as the
;;; argument.
;;;
;;; Add a line like this to your .xinitrc to use this window manager:
;;; exec guile -s /PATH/TO/tinywm.scm

(define-module (xcb xml sample tinywm)
  #:use-module (system repl server)
  #:use-module (xcb xml xproto)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml))

(define xcb-conn (xcb-connect!))

(define (wm-shell-command command)
  (define display-string
    (format #f "DISPLAY=~a" (xcb-connection-display xcb-conn)))
  (if (= (primitive-fork) 0)
      (let ((env (cons display-string (environ))))
        (execle "/bin/sh" env "/bin/sh" "-c" command))))

(event-loop-prepare!
 xcb-conn
 (lambda (cont err)
   (format (current-error-port) "Error: ~a\n" err) (cont)))

(loop-with-connection xcb-conn
  (define setup (xcb-connection-setup xcb-conn))
  (define screen (xref setup 'roots 0))
  (define root (xref screen 'root))
  (define action (make-parameter 'none))
  (define win (make-parameter #f))

  (define (on-motion-notify motion-notify)
    (with-replies ((pointer query-pointer root) (geom get-geometry (win)))
      (define (new-coord p g s) (if (> (+ p g) s) (- s g) p))
      (if (eq? (action) 'move)
          (configure-window
           (win)
           #:x (new-coord (xref pointer 'root-x)
                          (xref geom 'width)
                          (xref screen 'width-in-pixels))
           #:y (new-coord (xref pointer 'root-y)
                          (xref geom 'height)
                          (xref screen 'height-in-pixels)))
          (configure-window
           (win)
           #:width (- (xref pointer 'root-x) (xref geom 'x))
           #:height (- (xref pointer 'root-y) (xref geom 'y))))))

  (define (on-button-release button-release)
    (ungrab-pointer xcb-current-time))

  (define (on-window-click window button-press)
    (configure-window window #:stack-mode 'above)
    (with-replies ((geom get-geometry window))
      (cond
       ((= (xref button-press 'detail) 1)
        (action 'move)
        (warp-pointer (xcb-none xwindow) window 0 0 0 0 1 1))
       (else
        (action 'resize)
        (warp-pointer (xcb-none xwindow) window 0 0 0 0
                        (xref geom 'width) (xref geom 'height))))
      (grab-pointer
       #f root '(button-release button-motion pointer-motion-hint)
       'async 'async root (xcb-none xcursor) xcb-current-time)))

  (define (on-button-press button-press)
    (win (xref button-press 'child))
    (if (not (= (xid->integer (win)) 0)) (on-window-click (win) button-press)))

  (define (on-key-press key-press)
    (cond
     ((and (win) (= (xref key-press 'detail) 67))
      (configure-window (win) #:stack-mode 'below))
     ((= (xref key-press 'detail) 24) (xcb-disconnect! xcb-conn))))

  (listen! motion-notify-event 'mn on-motion-notify)
  (listen! button-release-event 'br on-button-release)
  (listen! button-press-event 'bp on-button-press)
  (listen! key-press-event 'kp on-key-press)

  (grab-key #t root '(#{1}#) 67 'async 'async)
  (grab-key #t root '(control #{1}#) 24 'async 'async)
  (grab-button #f root '(button-press button-release) 'async 'async root
               (xcb-none xcursor) '#{1}# '(#{1}#))
  (grab-button #f root '(button-press button-release) 'async 'async root
                 (xcb-none xcursor) '#{3}# '(#{1}#))
  (spawn-server)
  (wm-shell-command "xterm -e 'telnet localhost 37146'"))
