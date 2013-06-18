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
(define setup (xcb-connection-setup xcb-conn))
(define screen (xref setup 'roots 0))
(define root (xref screen 'root))

(define terminated? (make-parameter #f))
(define action (make-parameter 'none))
(define win (make-parameter #f))

(define (wm-shell-command command)
  (define display-string 
    (format #f "DISPLAY=~a" (xcb-connection-display xcb-conn)))
  (if (= (primitive-fork) 0)
      (let ((env (cons display-string (environ))))
        (execle "/bin/sh" env "/bin/sh" "-c" command))))

(GrabKey xcb-conn #t root '(#{1}#) 67 'Async 'Async)
(GrabKey xcb-conn #t root '(Control #{1}#) 24 'Async 'Async)
(GrabButton
 xcb-conn #f root '(ButtonPress ButtonRelease) 'Async 'Async
 root (xcb-none CURSOR) '#{1}# '(#{1}#))
(GrabButton
 xcb-conn #f root '(ButtonPress ButtonRelease) 'Async 'Async
 root (xcb-none CURSOR) '#{3}# '(#{1}#))

(define (on-motion-notify motion-notify)
  (xcb-await ((pointer (QueryPointer xcb-conn root))
              (geom (GetGeometry xcb-conn (win))))
    (define (new-coord p g s) (if (> (+ p g) s) (- s g) p))
    (if (eq? (action) 'move)
        (ConfigureWindow 
         xcb-conn (win) ConfigWindow
         `((X . ,(new-coord (xref pointer 'root_x) 
                            (xref geom 'width)
                            (xref screen 'width_in_pixels)))
           (Y . ,(new-coord (xref pointer 'root_y) 
                            (xref geom 'height)
                            (xref screen 'height_in_pixels)))))
        (ConfigureWindow 
         xcb-conn (win) ConfigWindow 
         `((Width . ,(- (xref pointer 'root_x) 
                        (xref geom 'x)))
           (Height . ,(- (xref pointer 'root_y) 
                         (xref geom 'y))))))))

(define (on-button-release button-release) 
  (UngrabPointer xcb-conn xcb-current-time))

(define (on-window-click window button-press)
  (ConfigureWindow 
   xcb-conn window ConfigWindow 
   `((StackMode . ,(xenum-ref StackMode 'Above))))
  (xcb-await ((geom (GetGeometry xcb-conn window)))
    (cond
     ((= (xref button-press 'detail) 1)
      (action 'move)
      (WarpPointer xcb-conn (xcb-none WINDOW) window 0 0 0 0 1 1))
     (else
      (action 'resize)
      (WarpPointer 
       xcb-conn (xcb-none WINDOW) window 0 0 0 0 
       (xref geom 'width)
       (xref geom 'height))))
    (GrabPointer
     xcb-conn #f root '(ButtonRelease ButtonMotion PointerMotionHint)
     'Async 'Async root (xcb-none CURSOR) xcb-current-time)))

(define (on-button-press button-press)
  (win (xref button-press 'child))
  (if (not (= (xid->integer (win)) 0)) 
      (on-window-click (win) button-press)))

(define (on-key-press key-press)
  (cond
   ((and (win) (= (xref key-press 'detail) 67)) 
    (ConfigureWindow 
     xcb-conn (win) ConfigWindow 
     `((StackMode . ,(xenum-ref StackMode 'Below)))))
   ((= (xref key-press 'detail) 24) (terminated? #t))))

(xcb-listen! xcb-conn MotionNotify-event on-motion-notify)
(xcb-listen! xcb-conn ButtonRelease-event on-button-release)
(xcb-listen! xcb-conn ButtonPress-event on-button-press)
(xcb-listen! xcb-conn KeyPress-event on-key-press)

(wm-shell-command "xterm -e 'telnet localhost 37146'")

(spawn-server)
(xcb-event-loop xcb-conn terminated?)
(xcb-disconnect! xcb-conn)
