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
