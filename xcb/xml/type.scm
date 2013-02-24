(define-module (xcb xml type)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (xcb xml enum)
  #:export (self-type
	    CARD8
	    CARD16
	    CARD32
	    INT16
	    typecheck
	    list-type
	    typed-value-value
	    typed-value-type
	    make-xcb-type
	    typed-value?
	    typed-value-unpack
	    typed-value-pack
	    xcb-type?
	    xcb-type-name
	    xcb-type-list?
	    xcb-type-opaque?
	    xcb-type-mask
	    xcb-type-pack
	    xcb-type-size
	    xcb-type-enum
	    xcb-type-require-enum?
	    xcb-type-unpack
	    xcb-type-predicate
	    make-typed-value))

(define-public (type-predicate predicate)
  (lambda (val)
    (predicate (typed-value-value val))))

(define-record-type xcb-type
  (make-xcb-type name predicate pack unpack opaque?)
  xcb-type?
  (name xcb-type-name)
  (predicate xcb-type-predicate)
  (pack xcb-type-pack)
  (unpack xcb-type-unpack)
  (opaque? xcb-type-opaque?)
  (list? xcb-type-list?)
  (mask xcb-type-mask)
  (enum xcb-type-enum)
  (require-enum? xcb-type-require-enum?))

(define (typed-value-pack value port) 
  ((xcb-type-pack (typed-value-type value))
   port
   (typed-value-value value)))

(define (typed-value-unpack type port)
  (receive (size value) 
      ((xcb-type-unpack type)
       port)
    (values size value)))

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
     (put-bytevector port bv))))

(define-public (unpack-int-proc n)
  (lambda (port)
   (values 
    n
    (let ((bv (get-bytevector-n port n)))
      (bytevector-uint-ref bv 0 (native-endianness) n)))))

(define-public CARD8
  (make-xcb-type 
   'CARD8 
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

(define-public INT16
  (make-xcb-type 
   'INT16
   (type-predicate (signed-bit-length-predicate 16))
   (pack-int-proc 2)
   (unpack-int-proc 2)
   #f))

(define-public CARD32
  (make-xcb-type 
   'CARD32
   (type-predicate (unsigned-bit-length-predicate 32))
   (pack-int-proc 4)
   (unpack-int-proc 4)
   #f))

(define-public BOOL
  (make-xcb-type 
   'BOOL
   (lambda (v) (or (eq? #t v) (eq? #f v)))
   (lambda (port val)
     (put-u8 port (if val 1 0)))
   (lambda (port)
     (values 1 (> (get-u8 port) 0)))
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
     (if (xcb-enum-key-get enum val) #t
	 #f))))

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
