(define-module (language xml-xcb compile-scheme)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (sxml match))

(load-extension "libguile_xcb_expression" "init_xcb_expressions")

(define-public (parse-file file)
  (with-input-from-file file
    (lambda ()  
      (let ((exp (xml->sxml #:trim-whitespace? #t)))
	(compile-body (if (eq? (caadr exp) '*PI*)
			  (caddr exp)
			  (cadr exp))
		      #f)))))

(define-public (parse-string string)
  (let ((exp (xml->sxml string #:trim-whitespace? #t)))
    (compile-body (if (eq? (caadr exp) '*PI*)
		      (caddr exp)
		      (cadr exp))
		  #f)))

(define-public (compile-scheme exp env opts)
  (values
   (compile-body 
    (if (eq? (caadr exp) '*PI*)
	(caddr exp)
	(cadr exp))
    env)
   env
   env))

(define (false-or pred?) (lambda (exp) (or (not exp) (pred? exp))))

(define (xml-boolean? value)
  (and
   (string? value)
   (or (equal? (string-downcase value) "true") 
       (equal? (string-downcase value) "false"))))

(define hex-integer-regexp "^0x[0-9a-fA-F]+$")
(define xml-integer-regexp "^[-+]?[0-9]+$")

(define (xml-integer? value)
  (if (string-match xml-integer-regexp value) #t #f))

(define (dec-or-hex-integer? value)
  (or 
   (xml-integer? value)
   (and
    (string? value)
    (if (string-match hex-integer-regexp value) #t #f))))

(define element-syntax 
  (make-record-type 
   'element-syntax 
   '(tag syntax documentation) 
   (lambda (el port)
     (display 
      (format #f "<element-syntax [~a]>" 
	      (element-syntax-tag el)) 
      port))))

(define element-syntax? (record-predicate element-syntax))
(define element-syntax-tag (record-accessor element-syntax 'tag))
(define element-syntax-syntax (record-accessor element-syntax 'syntax))
(define element-syntax-documentation-syntax 
  (record-accessor element-syntax 'documentation))
(define make-element-syntax (record-constructor element-syntax))

(define combine-matchers 
  (lambda matchers
    (lambda (exp)
      (let perform-match ((matcher (car matchers)) (rest (cdr matchers)))
	(or (matcher exp #t) 
	    (if (null? rest)
		(xml-xcb-parse-error exp #f)
		(perform-match (car rest) (cdr rest))))))))

(define (xml-xcb-parse-error exp ignore-error?)
  (if ignore-error?
      #f
      (error "xml-xcb: invalid expression" 
	     (with-output-to-string (lambda () (sxml->xml exp))))))

(define (partition-elements elements . groups)
  (let ((result-list
	 (let partition-element ((inner-elements elements) (inner-groups groups))
	   (if (null? inner-groups)
	       (if (> (length inner-elements) 0)
		   (error (format #f "xml-xcb: Unmatched tags ~a element list ~a" 
				  inner-elements
				  elements))
		   '())
	       (receive (matches rest) 
		   (partition 
		    (lambda (element)
		      (eq? (element-syntax-tag element) (caar inner-groups))) 
		    inner-elements)
		 (if (and (cadar inner-groups)
			  (< (length matches) (cadar inner-groups)))
		     (error (format #f "Too few instances of ~a in element list ~a" 
				    (caar inner-groups) elements)))
		 (if (and (caddar inner-groups)
			  (> (length matches) (caddar inner-groups)))
		     (error (format #f "Too many instances of ~a in element list ~a" 
				    (caar inner-groups) elements)))
		 (cons matches (partition-element rest (cdr inner-groups))))))))
    (apply values result-list)))


(define (proc-for-op op)
  (case (string->symbol op)
    ((+) 'xcb-add)
    ((-) 'xcb-subtract)
    ((*) 'xcb-multiply)
    ((~) 'xcb-not)
    ((/) 'xcb-divide)
    ((&) 'xcb-and)
    ((<<) 'xcb-lsh)))

(define* (exprfield-match exp #:optional ignore-error?)
  (sxml-match exp
    ((exprfield (@ (name ,name)
		   (type ,type)
		   (enum (,enum #f))
		   (altenum (,altenum #f))
		   (mask (,mask #f)))
		,[expression-match -> expression])
     (guard
      (string? name)
      (string? type)
      ((false-or string?) enum)
      ((false-or string?) altenum)
      ((false-or string?) mask))
     (make-element-syntax 'exprfield expression #f))    
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (reply-match exp request-name #:optional ignore-error?)
  (sxml-match exp
    ((reply ,[(combine-matchers 
	       valueparam-match 
	       fields-match
	       switch-match		  
	       doc-match) -> reply-fields] ...)
     (receive (pads fields lists valueparams switch doc)
	 (partition-elements reply-fields
			     '(field 0 #f)
			     '(pad 0 #f)
			     '(list 0 #f)
			     '(valueparam 0 #f)
			     '(switch 0 1)
			     '(doc 0 1))
       (if  (= (length (append fields pads lists valueparams)) 0)
	   (error "xml-xcb: Need at least one fields or valueparam element in reply."))
       (make-element-syntax
	'reply
	#f
	(if (= (length doc) 1)
	    (element-syntax-documentation-syntax (car doc))
	    #f))))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (valueparam-match exp #:optional ignore-error?)
  (sxml-match exp
    ((valueparam (@ (value-mask-type ,value-mask-type)
		    (value-mask-name ,value-mask-name)
		    (value-list-name ,value-list-name)))
     (guard
      (string? value-mask-type)
      (string? value-mask-name)
      (string? value-list-name))
     (make-element-syntax 'valueparam #f #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (field-match exp #:optional ignore-error?)
  (sxml-match exp
    ((field (@ (name ,name)
	       (type ,type)
	       (enum (,enum #f))
	       (altenum (,altenum #f))
	       (mask (,mask #f))))
     (guard
      (string? name)
      (string? type)
      ((false-or string?) enum)
      ((false-or string?) altenum)
      ((false-or string?) mask))
     (make-element-syntax 'field #f #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (pad-match exp #:optional ignore-error?)
  (sxml-match exp
    ((pad (@ (bytes ,bytes)))
     (guard
      (xml-integer? bytes))
     (make-element-syntax 'pad #f #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (list-match exp #:optional ignore-error?)
  (sxml-match exp
    ((non-scheme-list (@ (name ,name)
			 (type ,type)
			 (enum (,enum #f))
			 (altenum (,altenum #f))
			 (mask (,mask #f)))
		      ,[expression-match -> expression] ...)
     (guard
      (string? name)
      (string? type)
      ((false-or string?) enum)
      ((false-or string?) altenum)
      ((false-or string?) mask))
     (make-element-syntax 'list #f #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (fields-match exp #:optional ignore-errors?)
  (or (pad-match exp #t)
      (field-match exp #t)
      (if (eq? (car exp) 'list) 
	  (list-match (cons 'non-scheme-list (cdr exp))) #f)
      (xml-xcb-parse-error exp ignore-errors?)))

(define* (switch-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((switch (@ (name ,name))
	     ,[(combine-matchers
		bitcase-match
		expression-match
		fields-match) -> switch-fields] ...)
     (guard
      (string? name))
     (receive (bitcases expressions pads lists fields)
	 (partition-elements switch-fields
			     '(bitcase 1 #f)
			     '(expression 1 1)
			     '(pads 0 1)
			     '(lists 0 1)
			     '(fields 0 1))
       (if (> (length (append pads lists fields)) 1)
	   (error "xml-xcb: Too many fields in switch expression"))
      (make-element-syntax 'switch #f #f)))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define* (item-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((item (@ (name (,name #f)))
	   ,[(combine-matchers
	      doc-match
	      expression-match) -> expressions-or-docs] ...)
     (guard
      ((false-or string?) name))
     (receive (expression doc)
	 (partition-elements expressions-or-docs 
			     '(expression 0 1)
			     '(doc 0 1))
       (make-element-syntax
	'item
	(cons (string->symbol name) 
	      (if (null? expression)
		  #f
		  (element-syntax-syntax (car expression))))
	`(,name ,doc))))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define* (bitcase-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((bitcase (@ (name (,name #f)))
	      ,[expression-match -> expression]
	      ,[(combine-matchers
		 fields-match
		 switch-match) -> fields] ...)
     (guard
      ((false-or string?) name))
     (make-element-syntax 'bitcase #f #f))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define (macro-match exp)
  (sxml-match exp
    ((request (@ (name ,name)
		 (opcode ,opcode)
		 (combine-adjacent (,combine-adjacent #f)))
	      ,[(combine-matchers 
		 exprfield-match
		 valueparam-match
		 fields-match
		 switch-match
		 (lambda* (el #:optional ignore-error?) 
		   (reply-match el name ignore-error?))
		 doc-match) -> request-fields] ...)
     (guard
      (string? name)
      (xml-integer? opcode)
      ((false-or xml-boolean?) combine-adjacent))
     (receive (exprfields valueparams fields pads lists switches replies doc)
	 (partition-elements request-fields
			     '(exprfield 0 #f)
			     '(field 0 #f)
			     '(pad 0 #f)
			     '(list 0 #f)
			     '(valueparam 0 #f)
			     '(switch 0 1)
			     '(reply 0 1)
			     '(doc 0 1))

       (make-element-syntax 
	'request #f 
	(if (= (length doc) 1)
	    `(begin
	       (register-documentation xcbdoc
		'request ,name
		,(element-syntax-documentation-syntax (car doc)))
	       ,(if (and
		     (= (length replies) 1)
		     (element-syntax-documentation-syntax (car replies)))
		    `(register-documentation xcbdoc
		      'reply ,name
		      ,(element-syntax-documentation-syntax (car replies)))
		    #f))
	    #f))))
    ((event (@ (name ,name)
	       (number ,number)
	       (no-sequence-number (,no-sequence-number? #f)))
	    ,[(combine-matchers
	       doc-match
	       fields-match) -> fields] ...)
     (guard
      (string? name)
      (xml-integer? number)
      ((false-or xml-boolean?) no-sequence-number?))
     (receive (doc pad field list)
	 (partition-elements fields
			     '(doc 0 1)
			     '(pad 0 #f)
			     '(field 0 #f)
			     '(list 0 #f))
       (make-element-syntax
	'event
	#f
	(if (= (length doc) 1)
	    `(register-documentation xcbdoc 
	      'event ,name 
	      ,(element-syntax-documentation-syntax (car doc)))
	    #f))))
    ((eventcopy (@ (name ,name)
		   (number ,number)
		   (ref ,ref)))
     (guard
      (string? name)
      (xml-integer? number)
      (string? ref))
     (make-element-syntax 'eventcopy #f #f))
    ((errorcopy (@ (name ,name)
		   (number ,number)
		   (ref ,ref)))
     (guard
      (string? name)
      (xml-integer? number)
      (string? ref))
     (make-element-syntax 'errorcopy #f #f))
    ((error (@ (name ,name)
	       (number ,number))
	    ,[fields-match -> fields] ...)
     (guard
      (string? name)
      (xml-integer? number))
     (receive (fields pads lists)
	 (partition-elements fields
			     '(field 0 #f)
			     '(pad 0 #f)
			     '(list 0 #f))
       (make-element-syntax 'error #f #f)))
    ((import ,import) (guard (string? import)) 
     (make-element-syntax 'import `(xcb xml ext ,(string->symbol import)) #f))
    ((xidtype (@ (name ,name))) (guard (string? name)) 
     (make-element-syntax 
      'xidtype `(define-public 
		  ,(string->symbol name) 
		  (self-type (quote ,(string->symbol name)) 
			     (pack-uint-proc 4) (unpack-uint-proc 4) #t)) #f))
    ((xidunion (@ (name ,name))
	       (type ,types) ...)
     (guard
      (string? name)
      (>= (length types) 1)
      (every string? types))
     (make-element-syntax 
      'xidunion 
      `(define-public 
	 ,(string->symbol name) 
	 (self-union-type 
	  (quote ,(string->symbol name))
	  (list ,@(map (lambda (type) (string->symbol type)) types))
	  (pack-uint-proc 4) (unpack-uint-proc 4) #t)) #f))
    ((enum (@ (name ,name))
	   ,[(combine-matchers
	      item-match
	      doc-match) -> elements] ...)
     (guard
      (string? name))
     (receive (items doc)
	 (partition-elements elements 
			     '(item 1 #f)
			     '(doc 0 1))
       (make-element-syntax 
	'enum
	`(begin
	   (define-public 
	     ,(string->symbol name)
	     (make-hash-table))
	   ,@(let item-creator ((items items) (next-value 0))
	       (if (not (null? items))
		   (let* ((item-syntax (element-syntax-syntax (car items)))
			  (provided-value (cdr item-syntax)))
		     (cons
		      `(hashq-set! ,(string->symbol name)
				   (quote ,(car item-syntax))
				   ,(if provided-value `(,provided-value #f) next-value))
		      (if provided-value
			  (item-creator (cdr items) next-value)
			  (item-creator (cdr items) (1+ next-value)))))
		   '())))
	(if (= (length doc) 1)
	    `(register-documentation xcbdoc
	      'enum ,name 
	      ,(element-syntax-documentation-syntax (car doc)))
	    #f))))
    ((typedef (@ (oldname ,oldname)
		 (newname ,newname)))
     (guard (string? oldname)
	    (string? newname))
     (make-element-syntax 
      'typedef
      `(define-public ,(string->symbol newname) 
	 (clone-xcb-type 
	  (quote ,(string->symbol newname)) 
	  ,(string->symbol oldname)))
      #f))
    ((struct (@ (name ,name))
	     ,[(combine-matchers 
		fields-match 
		switch-match) -> fields] ...)
     (guard
      (string? name))
     (receive (fields pads lists switches)
	 (partition-elements fields
			     '(field 0 #f)
			     '(list 0 #f)
			     '(pad 0 #f)
			     '(switch 0 1))
       (if (= (length (append fields lists pads)) 0)
	   (error "xml-xcb: struct must contain at least one fields element"))
       (make-element-syntax 'struct #f #f)))
    ((union (@ (name ,name))
	    ,[(combine-matchers
	       switch-match
	       fields-match) -> fields] ...)
     (guard
      (string? name))
     (receive (fields pads lists switches)
	 (partition-elements fields
			     '(field 0 #f)
			     '(list 0 #f)
			     '(pad 0 #f)
			     '(switch 0 1))
       (if (= (length (append fields lists pads)) 0)
	   (error "xml-xcb: struct must contain at least one fields element"))
       (make-element-syntax 'struct #f #f)))
    (,otherwise (xml-xcb-parse-error exp #f))))

(define* (expression-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((op (@ (op ,op))
	 ,[expression-match -> expression1] 
	 ,[expression-match -> expression2])
     (guard
      (find (lambda (el) (equal? el op)) '("+" "-" "/" "*" "&" "<<")))
     (make-element-syntax
      'expression
      `(lambda (field-ref) (,(proc-for-op op) (,expression1 field-ref) (,expression2 field-ref)))
      #f))
    ((unop (@ (op ,op))
	   ,[expression-match -> expression])
     (guard (equal? op "~"))
     (make-element-syntax
      'expression
      `(lambda (field-ref) (,(proc-for-op op) (,expression field-ref)))
      #f))
    ((fieldref ,fieldref) (guard (string? fieldref))
     (make-element-syntax 'expression `(lambda (field-ref) (field-ref ,fieldref)) #f))
    ((enumref (@ (ref ,ref)) ,enumref) (guard (string? enumref) (string? ref)) 
     (make-element-syntax 'expression `(lambda (field-ref) (enum-ref ,ref ,enumref)) #f))
    ((sumof (@ (ref ,ref))) (guard (string? ref)) 
     (make-element-syntax
      'expression
      `(lambda (field-ref) 
	 (fold
	  xcb-add
	  0
	  (map 
	   (lambda (listel) (listel))
	   (xcb-list-elements ,(string->symbol ref)))))
      #f))
    ((popcount ,[expression-match -> expression])
     (make-element-syntax 
      'expression 
      `(lambda (field-ref) 
	 (xcb-popcount (,expression field-ref))) #f))
    ((value ,value) (guard (dec-or-hex-integer? value)) 
     (make-element-syntax 'expression `(lambda (field-ref) (parse-dec-or-hex-integer ,value)) #f))
    ((bit ,bit) (guard (xml-integer? bit)) 
     (make-element-syntax 'expression `(lambda (field-ref) (xcb-lsh 1 (string->number ,bit))) #f))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define (doc-fields-match exp)
  (sxml-match exp
    ((field (@ (name ,name)) ,cdata) 
     (guard 
      (string? cdata)
      (string? name)) 
     (make-element-syntax 'field #f `((name . ,name) (description . ,cdata))))
    ((field (@ (name ,name))) 
     (guard 
      (string? name)) 
     (make-element-syntax 'field #f `((name . ,name) (description . #f))))
    ((error (@ (type ,type)) ,cdata) 
     (guard 
      (string? cdata)
      (string? type)) 
     (make-element-syntax 'error #f `((type . ,type) (description . ,cdata))))
    ((error (@ (type ,type))) (guard (string? type))
     (make-element-syntax 'error #f `((type . ,type) (description . #f))))
    ((brief ,brief) (guard (string? brief)) 
     (make-element-syntax 'brief #f brief))
    ((description ,description) (guard (string? description))
     (make-element-syntax 'description #f description))
    ((example ,example) (guard (string? example)) 
     (make-element-syntax 'example #f example))
    ((see (@ (name ,name)
	     (type ,type)))
     (guard (string? name)
	    (string? type))
     (make-element-syntax 'see #f `((name . ,name) (type . ,(string->symbol type)))))
    (,otherwise (xml-xcb-parse-error exp #f))))

(define* (doc-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((doc ,[doc-fields-match -> doc-fields] ...)
     (receive (fields errors see brief description example)
	 (partition-elements doc-fields 
			     '(field 0 #f) 
			     '(error 0 #f) 
			     '(see 0 #f) 
			     '(brief 0 1)
			     '(description 0 1)
			     '(example 0 1))
       (make-element-syntax 
	'doc 
	#f
	(let ((extract-documentation-syntax 
	       (lambda* (var sym #:optional flatten?)
		 (let ((syn (map (lambda (el) 
				   (element-syntax-documentation-syntax el)) 
				 var)))
		   (if (> (length var) 0)
		       (list
			(if flatten?
			    `(,sym . ,(car syn))
			    `(,sym . ,syn)))
		       '())))))
	  `(,'quote (,@(extract-documentation-syntax fields 'fields)
		     ,@(extract-documentation-syntax errors 'errors)
		     ,@(extract-documentation-syntax see 'see)
		     ,@(extract-documentation-syntax brief 'brief #t)
		     ,@(extract-documentation-syntax description 'description #t)
		     ,@(extract-documentation-syntax example 'example #t)))))))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))


(define (compile-body exp env)
  (sxml-match exp
    ((xcb (@ (header ,header)
	     (extension-xname (,extension-xname #f))
	     (extension-name (,extension-name #f))
	     (extension-multiword (,extension-multiword? #f))
	     (major-version (,major-version #f))
	     (minor-version (,minor-version #f)))
	  ,[macro-match -> macros] ...)
     (guard
      (string? header)
      ((false-or string?) extension-xname)
      ((false-or string?) extension-name)
      ((false-or xml-boolean?) extension-multiword?)
      ((false-or xml-integer?) major-version)
      ((false-or xml-integer?) minor-version))
     (receive (enums 
	       events
	       requests
	       errors
	       eventcopies
	       errorcopies
	       structs
	       unions
	       xidtypes
	       xidunions
	       typedefs
	       imports)
	 (partition-elements (filter element-syntax? macros) 
			     '(enum 0 #f)
			     '(event 0 #f)
			     '(request 0 #f)
			     '(error 0 #f)
			     '(eventcopy 0 #f)
			     '(errorcopy 0 #f)
			     '(struct 0 #f)
			     '(union 0 #f)
			     '(xidtype 0 #f)
			     '(xidunion 0 #f)
			     '(typedef 0 #f)
			     '(import 0 #f))
       `(begin
	  (define-module (xcb xml ext ,(string->symbol header))
	    #:use-module (xcb xml type)
	    #:use-module (xcb xml struct)
	    #:use-module (xcb xml common)	    
	    ,@(if (not (string= header "xproto")) '(#:use-module (xcb xml xproto)) '())
	    ,@(map (lambda (import) `(#:use-module ,(element-syntax-syntax import))) imports))
	  (load-extension "libguile_xcb_expression" "init_xcb_expressions")
	  (define xcbdoc (make-hash-table))
	  ,@(map element-syntax-syntax (append xidtypes xidunions typedefs))
	  ,@(delete 
	     #f 
	     (map 
	      element-syntax-documentation-syntax 
	      (append requests events enums)))
	  	  (register-xcb-documentation (quote ,(string->symbol header)) xcbdoc))))
    (,otherwise 
     (error "xml-xcb: invalid expression" 
	    (with-output-to-string (lambda () (sxml->xml otherwise)))))))
