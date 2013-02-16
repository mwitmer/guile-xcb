(define-module (xcb xml struct)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 binary-ports)
  #:use-module (xcb xml type)
  #:use-module ((rnrs base) #:select (vector-for-each))
  #:export (define-xcb-struct))

(load-extension "libguile_xcb_expression" "init_xcb_expressions")

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

(define-syntax define-xcb-struct
  (syntax-rules ()
    ((define-xcb-struct type
	(constructor constructor-tag ...)
	predicate
	switch-expression
	(field-tag xcb-type . more) ...)
     (begin
      (define type
	(make-xcb-struct-for-record-type 
	 'type
	 switch-expression
	 (list
	  (xcb-struct-field-specifier 
	   field-tag xcb-type . more) ...)))
      (define constructor
	(xcb-struct-constructor type '(constructor-tag ...)))
      (define predicate
	(xcb-struct-predicate type))
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
     (define accessor (xcb-struct-accessor type 'field-tag)))
    ((define-xcb-struct-field type field-tag xcb-type accessor modifier)
     (begin
       (begin
	 (define accessor (xcb-struct-accessor type 'field-tag))
	 (define modifier (xcb-struct-modifier type 'field-tag)))))
    ((define-xcb-struct-field type field-tag xcb-type *list* list-length-expression 
       accessor modifier)
     (begin
       (define accessor 
	 (if (list-length-expression) 
	     (xcb-struct-vector-accessor type 'field-tag)
	     (xcb-struct-accessor type 'field-tag)))
       (define modifier 
	 (if (list-length-expression) 
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
    (if (find (lambda (check-field) (eq? check-field field)) record-type-fields)
	(typed-value-value ((record-accessor (underlying-record-type xcb-struct) field) rec))
	(if (string= (string-take-right (symbol->string field) 4) "_len")
	    (let ((field (string->symbol (string-drop-right (symbol->string field) 4))))
	      (vector-length ((record-accessor (underlying-record-type xcb-struct) field) rec)))
	    (error (format #f "xml-xcb: No field or list for \"~a\" in record" rec) rec)))))

(define (xcb-struct-constructor xcb-struct fields)  
  (let ((list-fields
	 (delete 
	  #f 
	  (hash-map->list 
	   (lambda (key value)
	     (if (xcb-type-list? value) key #f))
	   (types xcb-struct)))))
    (lambda args    
      (let ((rec 
	     (apply 
	      (record-constructor (underlying-record-type xcb-struct) 
				  (append
				   fields
				   (if (switch xcb-struct) 
				       (map car (xcb-switch-fields (switch xcb-struct))) '()))) 
	      (append
	       (map
		(lambda (arg field)
		  (if (typed-value? arg)
		      (typecheck arg)
		      (typecheck (make-typed-value arg (hashq-ref (types xcb-struct) field)))))
		args fields)
	       (if (switch xcb-struct) (map car (xcb-switch-fields (switch xcb-struct))) '())))))
	(for-each (lambda (field)
		    ((record-modifier (underlying-record-type xcb-struct) field) rec
		     (make-vector 
		      ((hashq-ref (list-length-expressions xcb-struct) field)
		       (xcb-struct-field-ref-proc xcb-struct rec)))))
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

(define (xcb-struct-vector-modifier xcb-struct field-tag)
  (lambda (rec n arg)
    (let ((val 
	   (if (typed-value? arg)
	       (typecheck arg)
	       (typecheck (make-typed-value arg (hashq-ref (types xcb-struct) field-tag))))))
      (vector-set! ((xcb-struct-accessor xcb-struct field-tag) rec) n val))))

(define (xcb-struct-modifier xcb-struct field-tag)
  (lambda (rec arg)
    (let ((val (if (typed-value? arg)
		   (typecheck arg)
		   (typecheck (make-typed-value arg (hashq-ref (types xcb-struct) field-tag))))))
      ((record-modifier (underlying-record-type xcb-struct) field-tag) rec val))))

(define (xcb-struct-pack xcb-struct rec port)
  (let ((write-pad-bytes
	 (lambda (n)
	   (for-each (lambda (n) (put-u8 port 0)) (iota n)))))
    (for-each 
     (lambda (field)
       (let ((field-name (car field))
	     (field-type (cadr field)))
	 (if (eq? field-name '*pad*) (write-pad-bytes field-type)
	     (let ((value ((record-accessor (underlying-record-type xcb-struct) field-name) rec)))
	       (if (vector? value)
		   (vector-for-each 
		    (lambda (value) 
		      (typed-value-pack value port)) value)
		   (or (and-let* ((field-value-expression
				   (hashq-ref (field-value-expressions xcb-struct) field-name)))
			 (typed-value-pack
			  (make-typed-value
			   (field-value-expression (xcb-struct-field-ref-proc xcb-struct rec))
			   field-type)
			  port))
		       (typed-value-pack value port)))))))
     (field-order xcb-struct))
    (if (switch xcb-struct)
	(xcb-switch-pack (switch xcb-struct) xcb-struct rec port))))

(define (xcb-struct-unpack xcb-struct total-size port)
  (let* ((bytes-read 0)
	 (skip-pad-bytes
	  (lambda (n)
	    (for-each (lambda (n) (get-u8 port)) (iota n))
	    (set! bytes-read (+ bytes-read n))))
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
				 (quotient (- total-size bytes-read) (xcb-type-size field-type))))
		       (vec (make-vector size)))
		  ((record-modifier (underlying-record-type xcb-struct) field-name) rec vec)
		  (for-each 
		   (lambda (n)
		     (vector-set! 
		      vec n 
		      (make-typed-value
		       (typed-value-unpack field-type port)
		       field-type))) 
		   (iota size))
		  (set! bytes-read (+ bytes-read (* size (xcb-type-size field-type))))))
	       ;; Events and replies from the X server don't contain
	       ;; exprfields, but just in case you want to unpack a
	       ;; request or struct you yourself packed before...
	       ((*expr*) 
		((record-modifier (underlying-record-type xcb-struct) field-name) 
		 rec
		 (make-typed-value 
		  ((hashq-ref (field-value-expressions xcb-struct) field-name) 
		   (xcb-struct-field-ref-proc xcb-struct rec))
		  field-type)))
	       (else
		((record-modifier (underlying-record-type xcb-struct) field-name) 
		 rec
		 (make-typed-value 
		  (typed-value-unpack field-type port)
		  field-type)
		 (set! bytes-read (+ bytes-read (xcb-type-size field-type)))))))))
     (field-order xcb-struct)
     (if (switch xcb-struct)
	 (xcb-switch-unpack (switch xcb-struct) xcb-struct rec port)))
    rec))

(define-record-type xcb-switch
  (make-xcb-switch name expression case-expressions default)
  xcb-switch?
  (name xcb-switch-name)
  (expression xcb-switch-expression)
  (case-expressions xcb-switch-case-expressions)
  (default xcb-switch-default))

(define-record-type xcb-case-expression
  (make-xcb-case-expression name expression fields switches)
  xcb-case-expression?
  (name xcb-case-expression-name)
  (expression xcb-case-expression-expression)
  (fields xcb-case-expression-fields)
  (switches xcb-case-expression-switches))

(define (xcb-switch-unpack switch xcb-struct rec port)
  (let ((bitmask ((xcb-switch-expression switch) (xcb-struct-field-ref-proc xcb-struct rec))))
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
	  (((record-accessor (underlying-record-type xcb-struct) (car xcb-switch-default))) rec)
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
  (let ((subfields
	 (apply 
	  append 
	  (map xcb-case-expression-nested-fields (xcb-switch-case-expressions switch)))))
    (if (xcb-switch-default switch)
	(cons (xcb-switch-default switch) subfields)
	subfields)))
