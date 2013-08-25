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

;;; Commentary:

;; This module exposes a public interface of support procedures for
;; the Guile XCB implementation of the X protocol.

(define-module (xcb xml)
  #:use-module ((xcb xml connection)
                #:select (xcb-connected?
                          xcb-connection-setup
                          xcb-connection-has-extension?
                          xcb-connection-display
                          set-on-xid-range-exhausted!
                          xcb-data
                          xcb-struct
                          xcb-sequence-number
                          current-xcb-connection
                          xcb-disconnect!
                          poll-xcb-connection))
  #:use-module ((xcb xml doc) #:select (document-full document-brief))
  #:use-module ((xcb xml auth) #:select (xcb-connect!))
  #:use-module ((xcb xml struct) #:select (xcb-struct-fields xcb-struct-name))
  #:use-module ((xcb xml core)
                #:select (make-new-xid
                          xref
                          xcb=
                          xset!
                          xref-string
                          enable-big-requests!
                          update-xid-range!
                          xcb-event->vector
                          make-xid))
  #:use-module ((xcb xml union) #:select (xunion-ref))
  #:use-module ((xcb xml enum) #:select (xenum-or xenum-ref xenum-key-ref
                                                  xenum-keys xenum-values))
  #:use-module (xcb xml type)
  #:use-module ((xcb xml records) #:select (make-typed-value typed-value-value))
  #:re-export (make-xid
               poll-xcb-connection
               xcb-event->vector
               xid=
               xenum-ref
               xenum-key-ref
               xcb-data
               xcb-struct
               xcb-struct-name
               xcb-sequence-number
               xenum-or
               xenum-keys
               xenum-values
               enable-big-requests!
               document-full
               xcb-struct-fields
               set-on-xid-range-exhausted!
               document-brief
               xcb-connect!
               xcb-disconnect!
               xcb-connected?
               current-xcb-connection
               xcb-connection-display
               xcb-connection-has-extension?
               xid->integer
               xunion-ref
               xcb-connection-setup
               xref
               xref-string
               xset!
               xcb=
               make-new-xid))

(define-public (xcb-none xcb-type) (make-typed-value 0 xcb-type))
(define-public xcb-current-time 0)
