(define-module (xcb xml struct)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml type)
  #:export (define-xcb-struct))

(define-record-type xcb-struct
  (make-xcb-struct underlying-record-type types list-length-expressions field-order)
  xcb-struct?
  (underlying-record-type underlying-record-type)
  (types types)
  (list-length-expressions list-length-expressions)
  (field-order field-order))

(define-syntax define-xcb-struct
  (syntax-rules ()
    ((define-xcb-struct type
	(constructor constructor-tag ...)
	predicate
	(field-tag type-predicate accessor-or-list-tag . more) ...)
     (begin
      (define type
	(make-xcb-struct-for-record-type 
	 'type 
	 (list
	  (xcb-struct-field-specifier 
	   field-tag type-predicate accessor-or-list-tag . more) ...)))
      (define constructor
	(xcb-struct-constructor type '(constructor-tag ...)))
      (define predicate
	(xcb-struct-predicate type))
      (define-xcb-struct-field type field-tag type-predicate accessor-or-list-tag . more)
      ...))))

(define-syntax xcb-struct-field-specifier
  (syntax-rules ()
    ((_ field-tag type-predicate accessor)
     (list 'field-tag type-predicate #f))
    ((_ field-tag type-predicate accessor modifier)
     (list 'field-tag type-predicate #f))
    ((_ field-tag type-predicate list-tag list-length-expression accessor modifier)
     (list 'field-tag type-predicate '*list* list-length-expression))))

(define-syntax define-xcb-struct-field
  (syntax-rules (*pad* *list*)
    ((define-xcb-struct-field type field-tag type-predicate accessor)
     (define accessor (xcb-struct-accessor type 'field-tag)))
    ((define-xcb-struct-field type field-tag type-predicate accessor modifier)
     (begin
       (begin
	 (define accessor (xcb-struct-accessor type 'field-tag))
	 (define modifier (xcb-struct-modifier type 'field-tag)))))
    ((define-xcb-struct-field type field-tag type-predicate *list* list-length-expression 
       accessor modifier)
     (begin
       (begin
	 (define accessor (xcb-struct-vector-accessor type 'field-tag))
	 (define modifier (xcb-struct-vector-modifier type 'field-tag)))))
    ((define-xcb-struct-field type *pad* size) *unspecified*)))

(define (make-xcb-struct-for-record-type type field-specifiers)
  (let ((types (make-hash-table))
	(list-length-expressions (make-hash-table)))    
    (for-each 
     (lambda (field-specifier)
       (let ((predicate (cadr field-specifier)))
	 (if (eq? (caddr field-specifier) '*list*)
	     (begin
	       (set! (predicate-list? predicate) #t)
	       (hashq-set! list-length-expressions 
			   (car field-specifier) 
			   (cadddr field-specifier))))
	 (case (car field-specifier) 
	   ((*pad*) #f)
	   (else (hashq-set! types (car field-specifier) predicate)))))
     field-specifiers)
    (make-xcb-struct 
     (make-record-type 'type (delete '*pad* (map car field-specifiers)))
     types
     list-length-expressions
     field-specifiers)))

(define (xcb-struct-constructor type fields)  
  (let ((list-fields
	 (delete 
	  #f 
	  (hash-map->list 
	   (lambda (key value)
	     (if (predicate-list? value) key #f))
	   (types type)))))
    (lambda args    
      (let ((new-record 
	     (apply 
	      (record-constructor (underlying-record-type type) fields) 
	      (map
	       (lambda (arg field)
		 (if (typed-value? arg)
		     (typecheck arg)
		     (typecheck (make-typed-value arg (hashq-ref (types type) field)))))
	       args fields))))
	(for-each (lambda (field)
		    ((record-modifier (underlying-record-type type) field) new-record
		     (make-vector 
		      ((hashq-ref (list-length-expressions type) field) new-record))))
		  list-fields)
	new-record))))

(define (xcb-struct-predicate type)
  (record-predicate (underlying-record-type type)))

(define (xcb-struct-accessor type field-tag)
  (lambda (rec)
    (let ((value ((record-accessor (underlying-record-type type) field-tag) rec)))
      (cond 
       ((vector? value) value)
       ((predicate-opaque? (typed-value-predicate value)) value)
       (else (typed-value-value value))))))

(define (xcb-struct-vector-accessor type field-tag)
  (lambda (rec n)
    (let ((value
	   (vector-ref ((xcb-struct-accessor type field-tag) rec) n)))
      (if (predicate-opaque? (typed-value-predicate value))
	  value
	  (typed-value-value value)))))

(define (xcb-struct-vector-modifier type field-tag)
  (lambda (rec n arg)
    (let ((val 
	   (if (typed-value? arg)
	       (typecheck arg)
	       (typecheck (make-typed-value arg (hashq-ref (types type) field-tag))))))
      (vector-set! ((xcb-struct-accessor type field-tag) rec) n val))))

(define (xcb-struct-modifier type field-tag)
  (lambda (rec arg)
    (let ((val (if (typed-value? arg)
		   (typecheck arg)
		   (typecheck (make-typed-value arg (hashq-ref (types type) field-tag))))))
      ((record-modifier (underlying-record-type type) field-tag) rec val))))
