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
  #:use-module (xcb xml type)
  #:use-module ((xcb xml core) #:select (xref))
  #:use-module ((xcb xml records) #:select (make-typed-value)))

(define-public (enable-extension xcb-conn name set-opcode! events errors reply)
  (define (enable)
    (set-opcode! (xref reply 'major-opcode))
    (xcb-connection-register-events xcb-conn events (xref reply 'first-event))
    (xcb-connection-register-errors xcb-conn errors (xref reply 'first-error))
    (xcb-connection-use-extension! xcb-conn name) #t)
  (if (xref reply 'present) (enable) #f))
