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
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml xproto)
  #:use-module ((xcb xml records)
                #:select (xcb-type-name xcb-type-list? make-typed-value typed-value?))
  #:use-module (xcb xml struct))

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

(define-public (xcb2b->string str)
  (define (val2b c2b)
    (integer->char
     (+ (xref c2b 'byte2) (* 255 (xref c2b 'byte1)))))
  (apply string (map val2b (vector->list str))))

(define-public (xcb-bytes->string str)
  (apply string (map integer->char (vector->list str))))

(define-public (string->xcb-bytes str)
  (list->vector
   (map (lambda (ch) (make-typed-value (char->integer ch) BYTE))
        (string->list str))))

(define-public (string->xcb2b str)
  (let ((str-bv (string->utf16 str (native-endianness))))
    (list->vector
     (fold-right
      (lambda (el prev)
	(cons (make-xchar2b
	       (bytevector-u8-ref str-bv (+ el 1))
	       (bytevector-u8-ref str-bv el)) prev))
      '() (iota (string-length str) 0 2)))))

(define-public (next-xid-value xcb-conn)
  (let* ((setup (xcb-connection-setup xcb-conn))
         (base (xref setup 'resource-id-base))
         (mask (xref setup 'resource-id-mask))
         (inc (logand mask (- mask)))
         (last-xid (xcb-connection-last-xid xcb-conn))
         (current-xid
          (if (> (+ last-xid inc) mask)
              (update-xid-range!
               xcb-conn
               ((on-xid-range-exhausted xcb-conn) xcb-conn))
              (+ last-xid inc))))
    (set-xcb-connection-last-xid! xcb-conn current-xid)
    (logior current-xid base)))

(define-public make-new-xid
  (case-lambda
    "-- Scheme Procedure: make-new-xid [xcb-conn] xcb-type
     Generate a new unique identifier for referring to a resource
     maintained by the X server connected to XCB-CONN (or
     `current-xcb-connection', if it is ommitted). X requests that
     instruct the server to create, for example, new Windows or
     Graphical Contexts require an XID for the new resource. XCB-TYPE
     is an xid type such as `xwindow' or `xgcontext'."
    ((xcb-conn xcb-type) (make-typed-value (next-xid-value xcb-conn) xcb-type))
    ((xcb-type)
     (make-typed-value (next-xid-value (current-xcb-connection)) xcb-type))))

(define-public (make-xid val xcb-type)
  "-- Scheme Procedure: make-xid n xid-type
     Create an XID with of type XID-TYPE with id number N."
  (make-typed-value val xcb-type))

(define (xcb-event->vector/c xcb-conn event)
  (define rtd (record-type-descriptor event))
  (define event-type ((record-accessor rtd 'xcb-struct-type) event))
  (define raw (xcb-struct-pack-to-bytevector event))
  (define ev
   (cons (number-for-event (current-xcb-connection) event-type)
         (bytevector->u8-list raw)))
  (list->vector (map integer->char ev)))

(define-public xcb-event->vector
  (case-lambda
    "-- Scheme Procedure: xcb-event->vector [xcb-conn] event
     Convert EVENT into a character vector for use in sending a
     `send-event' request on connection XCB-CONN (or
     `current-xcb-connection' if it is ommitted)."
    ((xcb-conn event) (xcb-event->vector/c xcb-conn event))
    ((event) (xcb-event->vector/c (current-xcb-connection) event))))

(define-public enable-big-requests!
  (case-lambda
    "-- Scheme Procedure: enable-big-requests! [xcb-conn] enable
     Sets the request size limit for XCB-CONN, or
     `current-xcb-connection' if it ommitted, to the value of
     ENABLE's `maximum-request-length' field. ENABLE should be a
     reply to the big request extension's `enable' request."
    ((xcb-conn enable-reply)
     (set-maximum-request-length!
      xcb-conn (xref enable-reply 'maximum-request-length)))
    ((enable-reply)
     (set-maximum-request-length!
      (current-xcb-connection) (xref enable-reply 'maximum-request-length)))))

(define-public (update-xid-range! xcb-conn range)
  (let* ((xid-count (xref range 'count))
         (xid-start (xref range 'start-id))
         (setup (xcb-connection-setup xcb-conn))
         (mask (xref setup 'resource-id-mask))
         (inc (mask (xref setup 'resource-id-mask))))
    (if (and (= xid-start 0) (= xid-count 1))
        (error "xml-xcb: Not more xids available!"))
    (set-xcb-connection-last-xid! xcb-conn xid-start)
    (xset! setup 'resource-id-mask! (* (+ xid-start (- xid-count 1)) inc))
    xid-start))

(define (xcb-convert-to-string val type)
  (if (xcb-type-list? type)
      (cond
       ((eq? (xcb-type-name xchar2b-type) (xcb-type-name type))
        (xcb2b->string val))
       ((eq? (xcb-type-name char) (xcb-type-name type))
        (xcb->string val))
       ((eq? (xcb-type-name BYTE) (xcb-type-name type))
        (xcb-bytes->string val))
       ((eq? (xcb-type-name void) (xcb-type-name type))
        (xcb-bytes->string val))
       (else #f))
      #f))

(define-public (xcb-convert-from-string val type)
  (cond
   ((eq? (xcb-type-name xchar2b-type) (xcb-type-name type))
    (string->xcb2b val))
   ((eq? (xcb-type-name char) (xcb-type-name type))
    (string->xcb val))
   ((eq? (xcb-type-name BYTE) (xcb-type-name type))
    (string->xcb-bytes val))
   (else (error (format #f "xml-xcb: Don't know how to convert \
string ~a to type ~a" val type)))))

(define (no-convert val type) #f)

(define-public xref
  (case-lambda
    "-- Scheme Procedure: xref rec field | rec field n
     Return the value of field FIELD in record REC. In the second form,
     return the Nth value of list field FIELD in record REC."
    ((rec field)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field no-convert)
      rec))
    ((rec field n)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field no-convert)
      rec n))))

(define-public xref-string
  (case-lambda
    "-- Scheme Procedure: xref-string rec field | rec field n
     Returns the same value as xref, unless the field's type is
     char[], xchar2b[], xbyte[], or void[], in which case it will
     return the value converted to a Scheme string. The second form
     of this procedure is identical to `xref'."
    ((rec field)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field xcb-convert-to-string)
      rec))
    ((rec field n)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field xcb-convert-to-string)
      rec n))))

(define-public xset!
  (case-lambda
    "-- Scheme Procedure: xset! rec field val | rec field n val
     Set the value of field FIELD in REC to VAL. In the second form,
     set the Nth element of list field FIELD in REC to VAL."
    ((rec field val)
     ((xcb-struct-modifier
       (xcb-struct-for-rec rec) field xcb-convert-from-string)
      rec val))
    ((rec field n val)
     ((xcb-struct-modifier
       (xcb-struct-for-rec rec) field xcb-convert-from-string)
      rec n val))))

(define-public (xcb= val1 val2)
  "-- Scheme Procedure: xcb= val1 val2
     Check to see if VAR1 and VAL2 would be represented by the same
     data when transferred to and from the X server.

     XIDs, numbers, symbols, strings, and booleans can be compared
     with this procedure."
  (cond
   ((typed-value? val1) (xid= val1 val2))
   ((number? val1) (= val1 val2))
   ((symbol? val1) (eq? val1 val2))
   ((string? val1) (string= val1 val2))
   ((boolean? val1) (eq? val1 val2))
   (else (error "xml-xcb: Unable to compare values" val1 val2))))
