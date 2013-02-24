(define-module (xcb xml struct)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module ((rnrs base) #:select (vector-for-each))
  #:export (define-xcb-struct
	     underlying-record-type
	     xcb-struct?
	     types
	     xcb-struct-pack
	     resolve-type
	     xcb-struct-unpack))

(load-extension "libguile_xcb_expression" "init_xcb_expressions")

(define (resolve-type type mask enum require-enum?)
  (let ((base-type
	 (cond
	  ((xcb-struct? type)
	   (xcb-type-for-struct type))
	  ((xcb-type? type) 
	   type)
	  (else
	   (error "xml-xcb: Cannnot determine type name for " type)))))
    (cond
     (enum (enum-type base-type enum require-enum?))
     (mask (mask-type base-type mask))
     (else base-type))))

(define (xcb-type-for-struct xcb-struct)
  (make-xcb-type
   (symbol-append (record-type-name (underlying-record-type xcb-struct)) '-type)
   (type-predicate (record-predicate (underlying-record-type xcb-struct)))
   (lambda (port rec) (xcb-struct-pack xcb-struct rec port))
   (lambda (port) (xcb-struct-unpack xcb-struct 0 port))
   #f))

(define-record-type xcb-struct-type
  (make-xcb-struct 
   underlying-record-type 
   switch
   types
   field-value-expressions
   list-length-expressions
   field-order)
  xcb-struct?
  (underlying-record-type underlying-record-type)
  (switch switch)
  (types types)
  (field-value-expressions field-value-expressions)
  (list-length-expressions list-length-expressions)
  (field-order field-order))

(define-public (xcb-struct-name xcb-struct)
  (record-type-name (underlying-record-type xcb-struct)))

(define-public (xcb-struct-predicate xcb-struct)
  (record-predicate (underlying-record-type xcb-struct)))

(define-syntax define-xcb-struct
  (syntax-rules ()
    ((define-xcb-struct type
       (constructor constructor-tag ...)
       predicate
       type-name
       switch-expression
       (field-tag xcb-type . more) ...)
     (begin
       (define-public type
       	 (make-xcb-struct-for-record-type 
       	  'type
       	  switch-expression
       	  (list
       	   (xcb-struct-field-specifier 
       	    field-tag xcb-type . more) ...)))
       (define-public constructor
       	 (xcb-struct-constructor type '(constructor-tag ...)))
       (define-public predicate
       	 (xcb-struct-predicate type))
       (define-public type-name
       	 (xcb-type-for-struct type))
       (define-xcb-struct-field type field-tag xcb-type . more)
       ...))))

(define-syntax xcb-struct-field-specifier
  (syntax-rules (*list* *pad* *expr*)
    ((_ *pad* pad-size)
     (list '*pad* pad-size))
    ((_ field-tag xcb-type accessor)
     (list 'field-tag xcb-type #f))
    ((_ field-tag xcb-type accessor modifier)
     (list 'field-tag xcb-type #f))
    ((_ field-tag xcb-type *expr* field-value-expression)
     (list 'field-tag xcb-type '*expr* field-value-expression))
    ((_ field-tag xcb-type *list* list-length-expression accessor modifier)
     (list 'field-tag xcb-type '*list* list-length-expression))))

(define-syntax define-xcb-struct-field
  (syntax-rules (*list* *pad* *expr*)
    ((define-xcb-struct-field type field-tag xcb-type accessor)
     (define-public accessor (xcb-struct-accessor type  'field-tag)))
    ((define-xcb-struct-field type field-tag xcb-type accessor modifier)
     (begin
       (begin
	 (define-public accessor (xcb-struct-accessor type 'field-tag))
	 (define-public modifier (xcb-struct-modifier type 'field-tag)))))
    ((define-xcb-struct-field type field-tag xcb-type *list* list-length-expression 
       accessor modifier)
     (begin
       (define-public accessor 
	 (if list-length-expression 
	     (xcb-struct-vector-accessor type 'field-tag)
	     (xcb-struct-accessor type 'field-tag)))
       (define-public modifier 
	 (if list-length-expression
	     (xcb-struct-vector-modifier type 'field-tag)
	     (xcb-struct-modifier type 'field-tag)))))
    ((define-xcb-struct-field type *pad* size) *unspecified*)
    ((define-xcb-struct-field type field-tag xcb-type *expr*) *unspecified*)))

(define (make-xcb-struct-for-record-type xcb-struct-name switch field-specifiers)
  (let* ((types (make-hash-table))
	 (list-length-expressions (make-hash-table))
	 (field-value-expressions (make-hash-table))
	 (field-order 
	  (fold
	   (lambda (field-specifier prev)
	     (if (eq? (car field-specifier) '*pad*)
		 (cons `(*pad* ,(cadr field-specifier)) prev)
		 (let ((xcb-type 
			(if (eq? (caddr field-specifier) '*list*)
			    (begin
			      (hashq-set!
			       list-length-expressions 
			       (car field-specifier) 
			       (cadddr field-specifier))
			      (list-type (cadr field-specifier)))
			    (cadr field-specifier))))
		   (if (eq? (caddr field-specifier) '*expr*)
		       (hash-set! field-value-expressions 
				  (car field-specifier) 
				  (cadddr field-specifier)))
		   (hashq-set! types (car field-specifier) xcb-type)
		   (cons (car field-specifier) prev))))
	   '()
	   field-specifiers)))
    (for-each (lambda (field)
		(hashq-set! types (car field) (cdr field)))
	      (xcb-switch-fields switch))
    (make-xcb-struct 
     (make-record-type 
      xcb-struct-name 
      (append (delete '*pad* (map car field-specifiers)) 
	      (map car (xcb-switch-fields switch))))
     switch
     types
     field-value-expressions
     list-length-expressions
     field-specifiers)))

(define (xcb-struct-field-ref-proc xcb-struct rec)
  "Returns a procedure that will return the value for FIELD in the given REC. If FIELD is not present in REC, it will check to see if FIELD is actually in the form LIST_len, and if so, return the length of list LIST in REC"
  (lambda (field)
    (if (find (lambda (check-field) (eq? check-field field)) 
	      (record-type-fields (underlying-record-type xcb-struct)))
	(typed-value-value ((record-accessor (underlying-record-type xcb-struct) field) rec))
	(if (string= (string-take-right (symbol->string field) 4) "_len")
	    (let ((field (string->symbol (string-drop-right (symbol->string field) 4))))
	      (vector-length ((record-accessor (underlying-record-type xcb-struct) field) rec)))
	    (error (format #f "xml-xcb: No field or list for \"~a\" in record" rec) rec)))))

(define (xcb-struct-constructor xcb-struct fields)  
  (let ((list-fields
	 (delete #f 
		 (hash-map->list 
		  (lambda (key value)
		    (if (xcb-type-list? value) key #f))
		  (types xcb-struct)))))
    (lambda args    
      (let ((rec 
	     (apply 
	      (record-constructor 
	       (underlying-record-type xcb-struct) 
	       (append
		fields
		(if (switch xcb-struct) 
		    (map car (xcb-switch-fields (switch xcb-struct))) '()))) 
	      (append
	       (map
		(lambda (arg field)
		  (let* ((xcb-type (hashq-ref (types xcb-struct) field))
			 (checked-val (if (xcb-type-enum xcb-type)
					  (or (xcb-enum-get (xcb-type-enum xcb-type) arg)
					      (if (xcb-type-require-enum? xcb-type)
						  (error "xcb-xml: No enum value with name " arg)
						  arg))
					  arg)))
		   (if (typed-value? checked-val)
		       (typecheck checked-val)
		       (typecheck (make-typed-value 
				   checked-val 
				   (hashq-ref (types xcb-struct) field))))))
		args fields)
	       (if (switch xcb-struct) (map car (xcb-switch-fields (switch xcb-struct))) '())))))
	(for-each (lambda (field)
		    ((record-modifier (underlying-record-type xcb-struct) field) rec
		     (make-vector 
		      ((hashq-ref (list-length-expressions xcb-struct) field)
		       (xcb-struct-field-ref-proc xcb-struct rec))
		      (if (xcb-type-mask 
			   (hashq-ref (types xcb-struct) field))
			  (make-typed-value 0 (hashq-ref (types xcb-struct) field))))))
		  list-fields)
	rec))))

(define (xcb-struct-predicate xcb-struct)
  (record-predicate (underlying-record-type xcb-struct)))

(define (xcb-struct-accessor xcb-struct field-tag)
  (lambda (rec)
    (let ((value ((record-accessor (underlying-record-type xcb-struct) field-tag) rec)))
      (cond 
       ((vector? value) value)
       ((xcb-type-opaque? (typed-value-type value)) value)
       (else (typed-value-value value))))))

(define (xcb-struct-vector-accessor xcb-struct field-tag)
  (lambda (rec n)
    (let ((value
	   (vector-ref ((xcb-struct-accessor xcb-struct field-tag) rec) n)))
      (if (xcb-type-opaque? (typed-value-type value))
	  value
	  (typed-value-value value)))))

(define (xcb-struct-mask-value xcb-struct field-tag original arg t-or-f)
  (with-bit-set
   original
   (apply 
    logior
    (map 
     (lambda (arg) 
       (xcb-enum-get 
	(xcb-type-mask 
	 (hashq-ref (types xcb-struct) field-tag)) arg))
     (if (list? arg) arg (list arg))))
   t-or-f))

(define (xcb-struct-vector-mask-modify xcb-struct field-tag rec n arg t-or-f)
  (vector-set! 
   ((xcb-struct-accessor xcb-struct field-tag) rec) n
   (make-typed-value
    (xcb-struct-mask-value 
     xcb-struct
     field-tag
     ((xcb-struct-vector-accessor xcb-struct field-tag) rec n)
     arg
     t-or-f)
    (hashq-ref (types xcb-struct) field-tag))))

(define (xcb-maybe-get-from-enum xcb-type arg)
  "Return either ARG or, if XCB-TYPE has an enum, a value from
XCB-TYPE's enum with name ARG. If no enum value is found for ARG,
return ARG if the enum is not required by the type, or throw an error
if it is."
  (if (xcb-type-enum xcb-type)
      (or (xcb-enum-get (xcb-type-enum xcb-type) arg)
	  (if (xcb-type-require-enum? xcb-type)
	      (error "xcb-xml: No enum value with name " arg)
	      arg))
      arg))

(define (xcb-struct-vector-nonmask-modify xcb-struct field-tag rec n arg)
  (vector-set! 
   ((xcb-struct-accessor xcb-struct field-tag) rec) n 
   (if (typed-value? arg)
       (typecheck arg)
       (let* ((xcb-type (hashq-ref (types xcb-struct) field-tag)))
	 (typecheck (make-typed-value 
		     (xcb-maybe-get-from-enum xcb-type arg) 
		     xcb-type))))))

(define (xcb-struct-vector-modifier xcb-struct field-tag)
  (lambda* (rec n arg #:optional (t-or-f *unspecified*))
    (if (and (not (eq? t-or-f *unspecified*))
	     (xcb-type-mask (hashq-ref (types xcb-struct) field-tag)))
	(xcb-struct-vector-mask-modify xcb-struct field-tag rec n arg t-or-f)
	(xcb-struct-vector-nonmask-modify xcb-struct field-tag rec n arg))))

(define (with-bit-set n bit val)
  (if val (logior n bit) (logand n (lognot bit))))

(define (xcb-struct-mask-modify xcb-struct field-tag rec arg t-or-f)
  ((record-modifier (underlying-record-type xcb-struct) field-tag) rec
   (typecheck 
    (make-typed-value
     (xcb-struct-mask-value 
      xcb-struct field-tag 
      ((xcb-struct-accessor xcb-struct field-tag) rec) 
      arg t-or-f)
     (hashq-ref (types xcb-struct) field-tag)))))

(define (xcb-struct-nonmask-modify xcb-struct field-tag rec arg)
  ((record-modifier (underlying-record-type xcb-struct) field-tag) rec
   (if (typed-value? arg)
       (typecheck arg)
       (let* ((xcb-type (hashq-ref (types xcb-struct) field-tag)))
	 (typecheck (make-typed-value 
		     (xcb-maybe-get-from-enum xcb-type arg) 
		     xcb-type))))))

(define (xcb-struct-modifier xcb-struct field-tag)
  "Returns a lambda that modifies the value of field FIELD-TAG in an instance
of XCB-STRUCT"
  (lambda* (rec arg #:optional (t-or-f *unspecified*))
    (if (and (not (eq? t-or-f *unspecified*)) 
	     (xcb-type-mask (hashq-ref (types xcb-struct) field-tag)))
	(xcb-struct-mask-modify xcb-struct field-tag rec arg t-or-f)
	(xcb-struct-nonmask-modify xcb-struct field-tag rec arg))))

(define (check-xcb-list-length xcb-struct rec field-name value)
  (and-let* ((list-length-expression 
	      (hashq-ref (list-length-expressions xcb-struct) field-name)))
   (let ((expected-length 
	  (list-length-expression
	   (xcb-struct-field-ref-proc xcb-struct rec))))
     (if (not (= expected-length (vector-length value)))
	 (error (format #f 
			"xml-xcb: Wrong length list in struct. Length: ~a Expected:"
			expected-length)
		(vector-length value))))))

(define (xcb-struct-pack xcb-struct rec port)
  (let ((write-pad-bytes
	 (lambda (n)
	   (for-each (lambda (n) (put-u8 port 0)) (iota n)))))
    (for-each 
     (lambda (field)
       (let ((field-name (car field))
	     (field-type (cadr field)))
	 (if (eq? field-name '*pad*) (write-pad-bytes field-type)
	     (let ((value ((record-accessor 
			    (underlying-record-type xcb-struct) 
			    field-name) rec)))
	       (if (vector? value)
		   (begin 
		     (check-xcb-list-length xcb-struct rec field-name value)
		     (vector-for-each
		      (lambda (value) 
			(typed-value-pack value port)) value))
		   (or (and-let* 
			   ((field-value-expression
			     (hashq-ref (field-value-expressions xcb-struct)
					field-name)))
			 (typed-value-pack
			  (make-typed-value
			   (field-value-expression 
			    (xcb-struct-field-ref-proc xcb-struct rec))
			   field-type)
			  port))
		       (typed-value-pack value port)))))))
     (field-order xcb-struct))
    (if (switch xcb-struct)
	(xcb-switch-pack (switch xcb-struct) xcb-struct rec port))))

(define (xcb-struct-unpack xcb-struct total-size port)
  (let* ((bytes-left (or total-size 0))
	 (skip-pad-bytes
	  (lambda (n)
	    (for-each (lambda (n) (get-u8 port)) (iota n))
	    (set! bytes-left (- bytes-left n))))
	 (rec ((record-constructor (underlying-record-type xcb-struct) '()))))
    (for-each
     (lambda (field)
       (let* ((field-name (car field))
	      (field-type (cadr field)))
	 (if (eq? field-name '*pad*) (skip-pad-bytes field-type)
	     (case (caddr field) 
	       ((*list*) 
		(let* ((list-length-expression 
			(hashq-ref (list-length-expressions xcb-struct) field-name))
		       (size (if list-length-expression
				 (list-length-expression
				  (xcb-struct-field-ref-proc xcb-struct rec))
				 #f)))		  
		  ((record-modifier 
		    (underlying-record-type xcb-struct) field-name) rec 
		    (list->vector 
		     (let read-next-list-item ((pre-vec-list '()) (size size))
		       (receive (unpack-size unpack-value)
			   (typed-value-unpack field-type port)
			 (set! bytes-left (- bytes-left unpack-size))
			 (if (or (and size (> size 1)) (and (not size) (> bytes-left )))
			     (list (read-next-list-item 
				    (append pre-vec-list 
					    (list (make-typed-value
						   unpack-value
						   field-type)))
				    (if size (- size 1) #f)))
			     (list (make-typed-value unpack-value field-type)))))))))
	       ((*expr*) 
		((record-modifier (underlying-record-type xcb-struct) field-name) 
		 rec
		 (make-typed-value 
		  ((hashq-ref (field-value-expressions xcb-struct) field-name) 
		   (xcb-struct-field-ref-proc xcb-struct rec))
		  field-type)))
	       (else
		(receive (unpack-size unpack-value)
		    (typed-value-unpack field-type port)
		  ((record-modifier (underlying-record-type xcb-struct) field-name)
		   rec
		   (make-typed-value 
		    unpack-value
		    field-type))
		  (set! bytes-left (- bytes-left unpack-size))))))))
     (field-order xcb-struct))
    (if (switch xcb-struct)
	(xcb-switch-unpack (switch xcb-struct) xcb-struct rec port))
    (values (or total-size (- bytes-left)) rec)))

(define-record-type xcb-switch
  (make-xcb-switch name expression case-expressions default)
  xcb-switch?
  (name xcb-switch-name)
  (expression xcb-switch-expression)
  (case-expressions xcb-switch-case-expressions)
  (default xcb-switch-default))

(define-public (make-empty-xcb-switch)
  (make-xcb-switch #f (lambda (n) #f) '() #f))

(define-record-type xcb-case-expression
  (make-xcb-case-expression name expression fields switches)
  xcb-case-expression?
  (name xcb-case-expression-name)
  (expression xcb-case-expression-expression)
  (fields xcb-case-expression-fields)
  (switches xcb-case-expression-switches))

(define (xcb-switch-unpack switch xcb-struct rec port)
  (let ((bitmask ((xcb-switch-expression switch) 
		  (xcb-struct-field-ref-proc xcb-struct rec))))
    (if (every not (map xcb-case-expression-unpack (xcb-switch-case-expressions switch)))
	(typed-value-unpack
	 (cdr xcb-switch-default)
	 port))))

(define (xcb-case-expression-unpack case-expression xcb-struct rec bitmask port)
  (if (= (xcb-and
	  ((xcb-case-expression-expression case-expression)
	   (xcb-struct-field-ref-proc xcb-struct rec))
	  bitmask) 0)
      #f
      (begin
       (for-each (lambda (field)
		   (let ((field-name (car field))
			 (field-type (cdr field)))
		     ((record-modifier (underlying-record-type xcb-struct) field-name) 
		      rec 
		      (make-typed-value
		       (typed-value-unpack field-type port)
		       field-type))))
		 (xcb-case-expression-fields case-expression))
       (for-each (lambda (switch) (xcb-switch-unpack switch xcb-struct rec port)) 
		 (xcb-case-expression-switches case-expression))
       #t)))

(define (xcb-switch-pack switch xcb-struct rec port)
  (let ((bitmask ((xcb-switch-expression switch) (xcb-struct-field-ref-proc xcb-struct rec))))
    (if (every not (map xcb-case-expression-pack (xcb-switch-case-expressions switch)))
	(typed-value-pack
	 (make-typed-value
	  (((record-accessor (underlying-record-type xcb-struct) 
			     (car (xcb-switch-default switch)))) rec)
	  (cdr xcb-switch-default))
	 port))))

(define (xcb-case-expression-pack case-expression xcb-struct rec bitmask port)
  (if (= (xcb-and
	  ((xcb-case-expression-expression case-expression)
	   (xcb-struct-field-ref-proc xcb-struct rec))
	  bitmask) 0)
      #f
      (begin
	(for-each (lambda (field)
		    (let ((field-name (car field))
			  (field-type (cdr field)))
		      (typed-value-pack 
		       ((record-accessor (underlying-record-type xcb-struct) field-name) 
			rec)
		       port)))
		  (xcb-case-expression-fields case-expression))
	(for-each (lambda (switch) (xcb-switch-pack switch xcb-struct rec port)) 
		  (xcb-case-expression-switches case-expression))
	#t)))

(define (xcb-case-expression-nested-fields case-expression)
  (apply append 
	 (xcb-case-expression-fields case-expression) 
	 (map xcb-switch-fields (xcb-case-expression-switches case-expression))))

(define (xcb-switch-fields switch)
  (if (not switch)
      '()
      (let ((subfields
	     (apply 
	      append 
	      (map xcb-case-expression-nested-fields (xcb-switch-case-expressions switch)))))
	(if (xcb-switch-default switch)
	    (cons (xcb-switch-default switch) subfields)
	    subfields))))

(define-public (xcb-struct-unpack-from-bytevector xcb-struct size bv)
  (let ((port (open-bytevector-input-port bv)))
    (xcb-struct-unpack xcb-struct size port)))

(define-public (xcb-struct-pack-to-bytevector xcb-struct rec)
  (call-with-values
      (lambda ()
	(open-bytevector-output-port))
    (lambda (port get-bytevector)
      (xcb-struct-pack xcb-struct rec port)
      (get-bytevector))))
