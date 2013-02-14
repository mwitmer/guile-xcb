(define-module (xcb xml struct)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 receive)
  #:export (define-xcb-struct))

(define-record-type xcb-struct
  (make-xcb-struct underlying-record-type types field-order)
  xcb-struct?
  (underlying-record-type underlying-record-type)
  (types types)
  (field-order field-order))

(define-record-type typed-value
  (make-typed-value value predicate)
  typed-value?
  (value typed-value-value)
  (predicate typed-value-predicate))

(set-record-type-printer! 
 typed-value 
 (lambda (value port)
   (format 
    port "<~a (~a)>"     
    (typed-value-value value)
    (let ((name (procedure-name (typed-value-predicate value))))
     (if name name (typed-value-predicate value))))))

(define-syntax type-self-predicate
  (syntax-rules ()
    ((_ name)
     (letrec ((name (lambda (val)
		      (eq? (typed-value-predicate val) name))))
       (set-object-property! name 'opqaue? #t)
       name))))

(define-public (type-predicate predicate)
  (lambda (val)
    (predicate (typed-value-value val))))

(define-syntax define-xcb-struct
  (syntax-rules ()
    ((define-xcb-struct type
	(constructor constructor-tag ...)
	predicate
	(field-tag type-predicate . more) ...)
     (begin
      (define type
	(make-xcb-struct-for-record-type 'type `((field-tag . ,type-predicate) ...)))
      (define constructor
	(xcb-struct-constructor type '(constructor-tag ...)))
      (define predicate
	(xcb-struct-predicate type))
      (define-xcb-struct-field type field-tag type-predicate . more)
      ...))))

(define-syntax define-xcb-struct-field
  (syntax-rules (*pad* *list*)
    ((define-xcb-struct-field type field-tag type-predicate accessor)
     (define accessor (xcb-struct-accessor type 'field-tag)))
    ((define-xcb-struct-field type field-tag type-predicate accessor modifier)
     (begin
       (begin
	 (define accessor (xcb-struct-accessor type 'field-tag))
	 (define modifier (xcb-struct-modifier type 'field-tag)))))
    ((define-xcb-struct-field type field-tag type-predicate *list* accessor modifier)
     (begin
       (begin
	 (define accessor (xcb-struct-vector-accessor type 'field-tag))
	 (define modifier (xcb-struct-vector-modifier type 'field-tag)))))
    ((define-xcb-struct-field type *pad* size) *unspecified*)))

(define (make-xcb-struct-for-record-type type field-specifiers)
  (let ((types (make-hash-table)))
    (for-each (lambda (field-specifier)
	   (if (not (eq? (car field-specifier) '*pad*))
	       (hashq-set! types (car field-specifier) (cdr field-specifier))))
	 field-specifiers)
    (make-xcb-struct 
     (make-record-type 'type (delete '*pad* (map car field-specifiers)))
     types
     field-specifiers)))

(define (typecheck value)
  (if (not ((typed-value-predicate value)  value))
      (error (format #f "Value ~a does not satisfy its type predicate" (typed-value-value value)) 
	     (typed-value-predicate value)))
  value)

(define (xcb-struct-constructor type fields)  
  (receive (list-fields non-list-fields)
      (partition pair? fields)
   (lambda args    
     (let ((new-record 
	    (apply 
	     (record-constructor 
	      (underlying-record-type type) 
	      non-list-fields) 
	     (map
	      (lambda (arg field)
		(if (typed-value? arg)
		    (typecheck arg)
		    (typecheck (make-typed-value arg (hashq-ref (types type) field)))))
	      args non-list-fields))))
       new-record))))

(define (xcb-struct-predicate type)
  (record-predicate (underlying-record-type type)))

(define (xcb-struct-accessor type field-tag)
  (lambda (rec)
    (let ((value ((record-accessor (underlying-record-type type) field-tag) rec)))
      (if (object-property (typed-value-predicate value) 'opaque?)
	  value
	  (typed-value-value value)))))

(define (xcb-struct-vector-accessor type field-tag)
  (lambda (rec n)
    (vector-ref ((xcb-struct-accessor type field-tag)) n)))

(define (xcb-struct-vector-modifier type field-tag)
  (lambda (rec n arg)
    (let ((val (if (typed-value? arg)
		   (typecheck arg)
		   (typecheck (make-typed-value arg (hashq-ref (types type) field-tag))))))
      (vector-set! ((xcb-struct-accessor type field-tag)) n val))))

(define (xcb-struct-modifier type field-tag)
  (lambda (rec arg)
    (let ((val (if (typed-value? arg)
		   (typecheck arg)
		   (typecheck (make-typed-value arg (hashq-ref (types type) field-tag))))))
      ((record-modifier (underlying-record-type type) field-tag) rec val))))
