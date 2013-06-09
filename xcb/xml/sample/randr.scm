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

(define-module (xcb xml sample randr)
  #:use-module (xcb xml) 
  #:use-module (xcb xml core) 
  #:use-module (xcb xml type) 
  #:use-module (xcb xml xproto)
  #:use-module ((rnrs base) #:select (vector-for-each))
  #:use-module (xcb xml ext randr))

(define xcb-conn (xcb-connect!))

(force (xcb-enable-randr! xcb-conn))

(define setup (xcb-connection-setup xcb-conn))
(define screen (Setup-get-roots setup 0))
(define root-window (SCREEN-get-root screen))
(define screen-resources 
  (xcb-now (GetScreenResourcesCurrent xcb-conn root-window)))

(vector-for-each 
 (lambda (output)
   (define output-info 
     (xcb-now (GetOutputInfo xcb-conn output xcb-current-time)))
   (format 
    #t "~a\n" 
    (xcb-bytes->string (GetOutputInfo-reply-get-name output-info))))
 (GetScreenResourcesCurrent-reply-get-outputs screen-resources))

(xcb-disconnect! xcb-conn)
