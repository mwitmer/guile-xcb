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
  #:use-module (xcb event-loop)
  #:use-module ((xcb xml struct) #:select (xref))
  #:use-module ((xcb xml records) #:select (make-typed-value)) 
  #:export (enable-extension))

(define (string->xcb str)
  (list->vector 
   (map (lambda (ch) (make-typed-value ch char))
        (string->list str))))

(define* (enable-extension 
          xcb-conn name header set-opcode! events errors #:optional proc)
  (define (enable opcode first-event first-error reply)
    (set-opcode! opcode)
    (xcb-connection-register-events xcb-conn events first-event)
    (xcb-connection-register-errors xcb-conn errors first-error)
    (let ((result (if proc (proc reply) #t)))
      (xcb-connection-use-extension! xcb-conn header)
      result))
  (xcb-await xcb-conn
    ((reply QueryExtension (string-length name) (string->xcb name)))
    (if (xref reply 'present)
        (enable (xref reply 'major_opcode)
                (xref reply 'first_event)
                (xref reply 'first_error) reply)
       (error "Could not find extension version on server" name))))
