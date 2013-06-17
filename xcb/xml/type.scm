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

(define-module (xcb xml type)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml records)
  #:export (list-type mock-new-xid typed-value-pack typed-value-unpack))

;; Some xcb structs are a little too optimistic about how many bytes
;; they expect to read from the server. Rather than keeping track of
;; total bytes read and immediately stopping when we reach the length
;; the server said it would sent, we'll just fill in the remaining
;; values with zeroes. It's that or a bunch of *undefined*'s; pick
;; your poison!

(define (safe-bytevector-uint-ref bv start endianness n)
  (if (eof-object? bv) 0
      (bytevector-uint-ref bv start endianness n)))

(define (safe-get-u8 bv) (if (eof-object? bv) 0 (get-u8 bv)))

(define-public (type-predicate predicate)
  (lambda (val)
    (predicate (typed-value-value val))))

(define (typed-value-pack value port) 
  ((xcb-type-pack (typed-value-type value))
   port
   (typed-value-value value)))

(define-public (typed-value-value-or-enum typed-val)
  (define value (typed-value-value typed-val))
  (define type (typed-value-type typed-val))
  (define (enum-lookup enum) (or (xenum-key-ref enum value) value))
  (define (mask-lookup mask)
    (define (mask-lookup key bit) (if (> (logand value bit) 0) key #f))
    (delq #f (hash-map->list mask-lookup (key-value-hash mask))))
  (cond
   ((xcb-type-enum type) => enum-lookup)
   ((xcb-type-mask type) => mask-lookup)
   (else value)))

(define (typed-value-unpack type port)
  (define result ((xcb-type-unpack type)
    port))
  result)

(define (unsigned-bit-length-predicate bits)
  (lambda (n)
   (and (integer? n) 
	(>= n 0)
	(<= (integer-length n) bits))))

(define (signed-bit-length-predicate bits)
  (lambda (n)
   (and (integer? n) 
	(<= (integer-length n) (- bits 1)))))

(define-public (pack-int-proc n)
  (lambda (port value)
   (let ((bv (make-bytevector n)))
     (bytevector-uint-set! bv 0 value (native-endianness) n)
     (for-each (lambda (n)
                 (put-u8 port (bytevector-u8-ref bv n)))
               (iota (bytevector-length bv))))))

(define-public (unpack-int-proc n)
  (lambda (port)
    (let ((bv (get-bytevector-n port n)))
      (safe-bytevector-uint-ref bv 0 (native-endianness) n))))

(define-public CARD8
  (make-xcb-type 
   'CARD8 
   (type-predicate (unsigned-bit-length-predicate 8))
   (pack-int-proc 1)
   (unpack-int-proc 1)
   #f))

(define-public BYTE
  (make-xcb-type 
   'BYTE
   (type-predicate (unsigned-bit-length-predicate 8))
   (pack-int-proc 1)
   (unpack-int-proc 1)
   #f))

(define-public CARD16
  (make-xcb-type 
   'CARD16
   (type-predicate (unsigned-bit-length-predicate 16))
   (pack-int-proc 2)
   (unpack-int-proc 2)
   #f))

(define-public INT8
  (make-xcb-type 
   'INT8
   (type-predicate (signed-bit-length-predicate 8))
   (pack-int-proc 1)
   (unpack-int-proc 1)
   #f))

(define-public INT16
  (make-xcb-type 
   'INT16
   (type-predicate (signed-bit-length-predicate 16))
   (pack-int-proc 2)
   (unpack-int-proc 2)
   #f))

(define-public INT32
  (make-xcb-type 
   'INT32
   (type-predicate (signed-bit-length-predicate 32))
   (pack-int-proc 4)
   (unpack-int-proc 4)
   #f))

(define-public CARD32
  (make-xcb-type 
   'CARD32
   (type-predicate (unsigned-bit-length-predicate 32))
   (pack-int-proc 4)
   (unpack-int-proc 4)
   #f))

(define-public char
  (make-xcb-type 
   'char
   (type-predicate char?)
   (lambda (port val)
     (put-u8 port (char->integer val)))
   (lambda (port)
     (integer->char (safe-get-u8 port)))
   #f))

(define-public void
  (make-xcb-type 
   'void
   (type-predicate (unsigned-bit-length-predicate 8))
   (pack-int-proc 1)
   (unpack-int-proc 1)
   #f))

(define-public BOOL
  (make-xcb-type 
   'BOOL
   (type-predicate boolean?)
   (lambda (port val)
     (put-u8 port (if (or (eqv? 0 val) (not val)) 0 1)))
   (lambda (port)
     (> (safe-get-u8 port) 0))
   #f))

(define-public (typecheck value)
  (if (not ((xcb-type-predicate (typed-value-type value)) value))
      (error (format #f "Value ~a does not satisfy its type predicate" 
		     (typed-value-value value))
	     (xcb-type-predicate (typed-value-type value))))
  value)

(define-public (self-type name pack unpack opaque?)
  (letrec ((type
	    (make-xcb-type 
	     name 
	     (lambda (value) (eq? (typed-value-type value) type)) 
	     pack
	     unpack
	     opaque?)))
    type))

(define next-mock-xid-value
  (let ((current-xid-value -1))
    (lambda* (#:optional reset?)
      (set! current-xid-value (if reset? 0 (1+ current-xid-value)))
      current-xid-value)))

(define* (mock-new-xid xcb-type #:optional reset?)
  (make-typed-value
   (next-mock-xid-value reset?)
   xcb-type))

(define (unsigned-bit-length-predicate bits)
  (lambda (n)
   (and (integer? n) 
	(>= n 0)
	(and  (<= (integer-length n) bits)))))

(define-public (clone-xcb-type name to-clone)
  (make-xcb-type
   name
   (xcb-type-predicate to-clone)
   (xcb-type-pack to-clone)
   (xcb-type-unpack to-clone)
   (xcb-type-opaque? to-clone)))

(define-public (self-union-type name types pack unpack opaque?)
  (make-xcb-type
   name
   (lambda (v)
     (any (lambda (p) (p v)) (map xcb-type-predicate types)))
   pack
   unpack
   opaque?))

(define (list-type type)
  ((record-constructor 
    xcb-type 
    '(name predicate pack unpack opaque? list? enum require-enum? mask))
   (xcb-type-name type) 
   (xcb-type-predicate type)
   (xcb-type-pack type)
   (xcb-type-unpack type)
   (xcb-type-opaque? type)
   #t
   (xcb-type-enum type)
   (xcb-type-require-enum? type)
   (xcb-type-mask type)))

(define (predicate-and p1 p2)
  (lambda (val)
    (and (p1 val) (p2 val))))

(define (xcb-enum-predicate enum)
  (type-predicate
   (lambda (val)
     (if (xenum-key-ref enum val) #t
	 #f))))

(define-public (xid= x1 x2)
  (and
   (eq? (typed-value-type x1)
        (typed-value-type x2))
   (= (typed-value-value x1)
      (typed-value-value x2))))

(define-public (enum-type type enum require-enum?)
  ((record-constructor xcb-type '(name predicate pack unpack opaque? list? mask enum require-enum?))
   (symbol-append (xcb-type-name type) (if require-enum? '-with-enum '-with-altenum))
   (if require-enum?
       (predicate-and (xcb-type-predicate type) (xcb-enum-predicate enum))
       (xcb-type-predicate type))
   (xcb-type-pack type)
   (xcb-type-unpack type)
   (xcb-type-opaque? type)
   (xcb-type-list? type)
   (xcb-type-mask type)
   enum
   require-enum?))

(define-public (mask-type type enum)
  ((record-constructor xcb-type '(name predicate pack unpack opaque? list? mask enum))
   (symbol-append (xcb-type-name type) '-with-mask)
   (xcb-type-predicate type)
   (xcb-type-pack type)
   (xcb-type-unpack type)
   (xcb-type-opaque? type)
   (xcb-type-list? type)
   enum
   #f))
