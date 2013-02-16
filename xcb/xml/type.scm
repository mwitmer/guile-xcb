(define-module (xcb xml type)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-9 gnu)
  #:export (self-type
	    CARD8
	    CARD16
	    CARD32
	    typecheck
	    list-type
	    type-predicate
	    typed-value-value
	    typed-value-type
	    make-xcb-type
	    typed-value?
	    typed-value-unpack
	    typed-value-pack
	    xcb-type-list?
	    xcb-type-opaque?
	    xcb-type-pack
	    xcb-type-size
	    xcb-type-unpack
	    xcb-type-predicate
	    make-typed-value))

(define-record-type xcb-type
  (make-xcb-type name predicate size pack unpack opaque?)
  xcb-type?
  (name xcb-type-name)
  (predicate xcb-type-predicate)
  (size xcb-type-size)
  (pack xcb-type-pack)
  (unpack xcb-type-unpack)
  (opaque? xcb-type-opaque?)
  (list? xcb-type-list?))

(define (typed-value-pack value port) 
  ((xcb-type-pack (typed-value-type value))
   port
   (typed-value-value value)))

(define (typed-value-unpack type port)
  ((xcb-type-unpack type)
   port))

(define (unsigned-bit-length-predicate bits)
  (lambda (n)
   (and (integer? n) 
	(>= n 0)
	(and  (<= (integer-length n) bits)))))

(define-public (pack-uint-proc n)
  (lambda (port value)
   (let ((bv (make-bytevector n)))
     (bytevector-uint-set! bv 0 value (native-endianness) n)
     (put-bytevector port bv))))

(define-public (unpack-uint-proc n)
  (lambda (port)
   (let ((bv (get-bytevector-n port n)))
     (bytevector-uint-ref bv 0 (native-endianness) n))))

(define-public CARD8
  (make-xcb-type 
   'CARD8 
   (type-predicate (unsigned-bit-length-predicate 8))
   1
   (pack-uint-proc 1)
   (unpack-uint-proc 1)
   #f))

(define-public CARD16
  (make-xcb-type 
   'CARD16
   (type-predicate (unsigned-bit-length-predicate 16))
   2
   (pack-uint-proc 2)
   (unpack-uint-proc 2)
   #f))

(define-public CARD32
  (make-xcb-type 
   'CARD32
   (type-predicate (unsigned-bit-length-predicate 32))
   4
   (pack-uint-proc 4)
   (unpack-uint-proc 4)
   #f))

(define-public BOOL
  (make-xcb-type 
   'BOOL
   (lambda (v) (or (eq? #t v) (eq? #f v)))
   1
   (lambda (port val)
     (put-u8 port (if val 1 0)))
   (lambda (port)
     (> (get-u8 port) 0))
   #f))

(define-record-type typed-value
  (make-typed-value value type)
  typed-value?
  (value typed-value-value)
  (type typed-value-type))

(set-record-type-printer! 
 typed-value 
 (lambda (value port)
   (format 
    port "<~a (~a)>"     
    (typed-value-value value)
    (xcb-type-name (typed-value-type value)))))

(define (typecheck value)
  (if (not ((xcb-type-predicate (typed-value-type value)) value))
      (error (format #f "Value ~a does not satisfy its type predicate" 
		     (typed-value-value value))
	     (xcb-type-predicate (typed-value-type value))))
  value)

(define (self-type name pack unpack opaque?)
  (letrec ((type
	    (make-xcb-type 
	     name 
	     (lambda (value) (eq? (typed-value-type value) type)) 
	     4
	     pack
	     unpack
	     opaque?)))
    type))

(define (unsigned-bit-length-predicate bits)
  (lambda (n)
   (and (integer? n) 
	(>= n 0)
	(and  (<= (integer-length n) bits)))))

(define-public (clone-xcb-type name to-clone)
  (make-xcb-type
   name
   (xcb-type-predicate to-clone)
   (xcb-type-size to-clone)
   (xcb-type-pack to-clone)
   (xcb-type-unpack to-clone)
   (xcb-type-opaque? to-clone)))

(define-public (self-union-type name types pack unpack opaque?)
  (make-xcb-type
   name
   (lambda (v)
     (any (lambda (p) (p v)) (map xcb-type-predicate types)))
   (max (map xcb-type-size types))
   pack
   unpack
   opaque?))

(define (list-type type)
  ((record-constructor 
    xcb-type 
    '(name predicate pack unpack opaque? list?))
   (xcb-type-name type) 
   (xcb-type-predicate type)
   (xcb-type-pack type)
   (xcb-type-unpack type)
   (xcb-type-opaque? type)
   #t))

(define-public (type-predicate predicate)
  (lambda (val)
    (predicate (typed-value-value val))))
