#!/home/mark/build/guile-2.0/bin/guile
!#
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

(define-module (xcb xml sample tinywm)
  #:use-module (system repl server)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml))

(define xcb-conn (xcb-connect!))
(define setup (xcb-connection-setup xcb-conn))
(define screen  (Setup-get-roots setup 0))
(define root (SCREEN-get-root screen))

(define terminated? (make-parameter #f))
(define action (make-parameter 'none))
(define win (make-parameter #f))

(define (wm-shell-command command)
  (define display-string 
    (format #f "DISPLAY=" (xcb-connection-display xcb-conn)))
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
         `((X . ,(new-coord (QueryPointer-reply-get-root_x pointer) 
                            (GetGeometry-reply-get-width geom)
                            (SCREEN-get-width_in_pixels screen)))
           (Y . ,(new-coord (QueryPointer-reply-get-root_y pointer) 
                            (GetGeometry-reply-get-height geom)
                            (SCREEN-get-height_in_pixels screen)))))
        (ConfigureWindow 
         xcb-conn (win) ConfigWindow 
         `((Width . ,(- (QueryPointer-reply-get-root_x pointer) 
                        (GetGeometry-reply-get-x geom)))
           (Height . ,(- (QueryPointer-reply-get-root_y pointer) 
                         (GetGeometry-reply-get-y geom))))))))

(define (on-button-release button-release) 
  (UngrabPointer xcb-conn xcb-current-time))

(define (on-button-press button-press)
  (win (ButtonPress-get-child button-press))
  (ConfigureWindow 
   xcb-conn (win) ConfigWindow 
   `((StackMode . ,(xcb-enum-get StackMode 'Above))))
  (xcb-await ((geom (GetGeometry xcb-conn (win))))
    (cond
     ((= (ButtonPress-get-detail button-press) 1)
      (action 'move)
      (WarpPointer xcb-conn (xcb-none WINDOW) (win) 0 0 0 0 1 1))
     (else
      (action 'resize)
      (WarpPointer 
       xcb-conn (xcb-none WINDOW) (win) 0 0 0 0 
       (GetGeometry-reply-get-width geom)
       (GetGeometry-reply-get-height geom))))
    (GrabPointer
     xcb-conn #f root '(ButtonRelease ButtonMotion PointerMotionHint)
     'Async 'Async root (xcb-none CURSOR) xcb-current-time)))

(define (on-key-press key-press)
  (cond
   ((and (win) (= (KeyPress-get-detail key-press) 67)) 
    (ConfigureWindow 
     xcb-conn (win) ConfigWindow 
     `((StackMode . ,(xcb-enum-get StackMode 'Below)))))
   ((= (KeyPress-get-detail key-press) 24)
    (terminated? #t))))

(xcb-listen! xcb-conn MotionNotify on-motion-notify #t)
(xcb-listen! xcb-conn ButtonRelease on-button-release #t)
(xcb-listen! xcb-conn ButtonPress on-button-press #t)
(xcb-listen! xcb-conn KeyPress on-key-press #t)
(xcb-listen! xcb-conn Drawable-error on-drawable-error #t)
(xcb-listen! xcb-conn Window-error on-window-error #t)

(spawn-server)
(wm-shell-command "xterm -e 'telnet localhost 37146'")
(xcb-event-loop xcb-conn terminated?)
(xcb-disconnect! xcb-conn)
