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

(define-module (xcb xml core)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml connection)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs base) #:select (vector-map))
  #:use-module ((xcb xml struct) #:select (xref xset!))
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml ext xc_misc)
  #:use-module (xcb xml ext ge)
  #:use-module (xcb xml ext bigreq)
  #:use-module ((xcb xml records) #:select (make-typed-value))
  #:export (enable-extension))

(define-public (bv->xcb-string bv)
  (let ((vec (make-vector (bytevector-length bv))))
    (for-each 
     (lambda (n)
       (vector-set! 
        vec n 
        (make-typed-value (integer->char (bytevector-u8-ref bv n)) char))) 
     (iota (bytevector-length bv)))
    vec))

(define-public (xcb-pad-bv bv)
  (let ((newbv (make-bytevector (* 2 (bytevector-length bv)) 0)))
    (for-each
     (lambda (n)
       (bytevector-u8-set! newbv n (bytevector-u8-ref bv n)))
     (iota (bytevector-length bv)))
    newbv))

(define-public (xcb-pad-string str)
  (string-pad-right str (* (string-length str) 2) (integer->char 0)))

(define-public (string->xcb str)
  (list->vector 
   (map (lambda (ch) (make-typed-value ch char))
        (string->list str))))

(define-public (xcb->string str)
  (apply string (vector->list str)))

(define-public (xcb-bytes->string str)
  (apply string (map integer->char (vector->list str))))

(define-public (string->xcb2b str)
  (let ((str-bv (string->utf16 str (native-endianness))))
    (list->vector
     (fold-right
      (lambda (el prev)  
	(cons (make-CHAR2B 
	       (bytevector-u8-ref str-bv (+ el 1))
	       (bytevector-u8-ref str-bv el)) prev)) 
      '() (iota (string-length str) 0 2)))))

(define-public (next-xid-value xcb-conn)
  (let* ((setup (xcb-connection-setup xcb-conn))
         (base (xref setup 'resource_id_base))
         (mask (xref setup 'resource_id_mask))
         (inc (logand mask (- mask)))
         (last-xid (xcb-connection-last-xid xcb-conn))
         (current-xid 
          ;; (if (> (+ last-xid inc) mask)
          ;;     (begin
          ;;       (enable-xc-misc xcb-conn)
          ;;       (if (xcb-connection-has-extension? xcb-conn 'xc_misc) 
          ;;           (update-xid-range! xcb-conn inc)
          ;;           (error "xml-xcb: Not more xids available!")))
              (+ last-xid inc))) ;    )
    (set-xcb-connection-last-xid! xcb-conn current-xid)
    (logior current-xid base)))

(define-public (make-new-xid xcb-conn xcb-type)
  (make-typed-value (next-xid-value xcb-conn) xcb-type))

(define-public (make-xid val xcb-type)
  (make-typed-value val xcb-type))

;; (define (update-xid-range! xcb-conn inc)
;;   (xcb-await ((range (GetXIDRange xcb-conn)))
;;     (let ((xid-count (xref range 'count))
;;           (xid-start (xref range 'start_id))
;;           (setup (xcb-connection-setup xcb-conn)))
;;       (if (and (= xid-start 0) (= xid-count 1))
;;           (error "xml-xcb: Not more xids available!"))
;;       (set-xcb-connection-last-xid! xcb-conn xid-start)
;;       (xset! setup 'resource_id_mask! (* (+ xid-start (- xid-count 1)) inc))
;;       xid-start)))

;; (define-public (enable-xc-misc xcb-conn) (xcb-enable-xc_misc! xcb-conn))

;; (define-public (enable-big-requests xcb-conn)
;;   (xcb-enable-bigreq! 
;;    xcb-conn
;;    (lambda (reply)
;;      (xcb-await ((enable (Enable xcb-conn)))
;;        (set-maximum-request-length! 
;;         xcb-conn (xref enable 'maximum_request_length))))))

(define-public (enable-generic-events xcb-conn) (xcb-enable-ge! xcb-conn))
