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

(define-module (language xml-xcb compile-scheme)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml type)
  #:use-module (language xml-xcb support)
  #:use-module (sxml simple)
  #:use-module (sxml match))

(define-public (compile-scheme exp env opts)
  (let* ((syn (top-level-match exp env))
         (exp (element-syntax-syntax syn))
         (doc (element-syntax-documentation-syntax syn)))
    (values (if doc `(begin ,exp ,doc) exp) env env)))

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
  (define result-list
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
                 (let ((syntax-tag-names (caar inner-groups)))
                   (if (list? syntax-tag-names)
                       (find (lambda (el) (eq? el (element-syntax-tag element)))
                             syntax-tag-names)
                       (eq? (element-syntax-tag element) syntax-tag-names))))
               inner-elements)
            (if (and (cadar inner-groups)
                     (< (length matches) (cadar inner-groups)))
                (error (format #f "Too few instances of ~a in element list ~a"
                               (caar inner-groups) elements)))
            (if (and (caddar inner-groups)
                     (> (length matches) (caddar inner-groups)))
                (error (format #f "Too many instances of ~a in element list ~a"
                               (caar inner-groups) elements)))
            (cons matches (partition-element rest (cdr inner-groups)))))))
  (apply values result-list))


(define (proc-for-op op)
  (case (string->symbol op)
    ((~) 'lognot)
    ((/) 'quotient)
    ((&) 'logand)
    ((<<) 'ash)
    (else => identity)))

(define* (exprfield-match exp #:optional ignore-error?)
  (sxml-match exp
    ((exprfield (@ (name ,name)
		   (type ,type)
		   (enum (,enum #f))
		   (altenum (,altenum #f))
		   (mask (,mask #f)))
		,(expression-match -> expression))
     (guard
      (string? name)
      (string? type)
      ((false-or string?) enum)
      ((false-or string?) altenum)
      ((false-or string?) mask))
     (make-element-syntax
      'exprfield
      `(,(normalize-name name)
        ,(resolve-type-syntax type mask enum altenum)
	*expr*
	,(element-syntax-syntax expression)) #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (reply-match exp request-name #:optional ignore-error?)
  (sxml-match exp
    ((reply ,((combine-matchers
	       valueparam-match
	       fields-match
	       switch-match
	       doc-match) -> reply-fields) ...)
     (receive (fields valueparams switches doc)
	 (partition-elements reply-fields
			     '((field pad list exprfield) 0 #f)
			     '(valueparam 0 #f)
			     '(switch 0 1)
			     '(doc 0 1))
       (if  (= (length (append fields valueparams)) 0)
	   (error "xml-xcb: Need at least one fields or valueparam element in reply."))
       (make-element-syntax
	'reply
	(let* ((fields
                (cons
                 (make-element-syntax 'field '(length CARD32) #f)
                 fields))
               (switch-syntax (if (not (null? switches))
                                  (element-syntax-syntax (car switches)) #f))
               (reply-struct-name
		(symbol-append request-name '-reply))
	       (fields-syntax (map element-syntax-syntax fields))
	       (valueparam-syntax (if (not (null? valueparams))
				      (element-syntax-syntax (car valueparams))
				      #f)))
	  `(begin
	     ,(get-define-xcb-struct-syntax
	       reply-struct-name
	       (if (not (null? valueparams))
		   (merge-valueparam-with-fields
		    (element-syntax-syntax (car valueparams))
		    fields)
		   fields) 0 switch-syntax)))
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
     (make-element-syntax
      'valueparam
      `((value-mask-type . ,value-mask-type)
        (value-mask-name . ,(normalize-name value-mask-name))
        (value-list-name . ,(normalize-name value-list-name))) #f))
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
     (if (string-suffix? "_len" name)
         (make-element-syntax
          'exprfield
          `(,(normalize-name name)
            ,(resolve-type-syntax type mask enum altenum)
            *len*
            (lambda (field-ref)
              (vector-length
               (field-ref ',(normalize-name (string-drop-right name 4)))))) #f)
         (make-element-syntax
          'field
          `(,(normalize-name name)
            ,(resolve-type-syntax type mask enum altenum)) #f)))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define* (pad-match exp #:optional ignore-error?)
  (sxml-match exp
    ((pad (@ (bytes ,bytes)))
     (guard
      (xml-integer? bytes))
     (make-element-syntax 'pad `(*pad* ,(string->number bytes)) #f))
    (,otherwise (xml-xcb-parse-error exp ignore-error?))))

(define (resolve-type-syntax type mask enum altenum)
  `(resolve-type ,(make-type-name (remove-prefix type))
		,(if mask (normalize-name mask) #f)
		,(cond (enum (normalize-name enum))
		       (altenum (normalize-name altenum))
		       (else #f))
		,(if enum #t #f)))

(define* (list-match exp #:optional ignore-error?)
  (sxml-match exp
    ((non-scheme-list (@ (name ,name)
			 (type ,type)
			 (enum (,enum #f))
			 (altenum (,altenum #f))
			 (mask (,mask #f)))
		      ,(expression-match -> expression) ...)
     (guard
      (string? name)
      (string? type)
      ((false-or string?) enum)
      ((false-or string?) altenum)
      ((false-or string?) mask))
     (make-element-syntax
      'list
      `(,(normalize-name name)
	,(resolve-type-syntax type mask enum altenum)
	*list*
	,(if (= (length expression) 1)
	     (element-syntax-syntax (car expression))
	     #f)) #f))
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
	     ,((combine-matchers
		bitcase-match
		expression-match
		fields-match) -> switch-fields) ...)
     (guard
      (string? name))
     (receive (bitcases expressions fields)
	 (partition-elements switch-fields
			     '(bitcase 1 #f)
			     '(expression 1 1)
                             '((field pad list) 0 1))
       (make-element-syntax
        'switch
        `(make-xcb-switch
          (quote ,(normalize-name name))
          ,(element-syntax-syntax (car expressions))
          (list ,@(map element-syntax-syntax bitcases))
          ,(if (null? fields) #f
               (make-quoted-field (car fields)))) #f)))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define (make-quoted-field field)
  `(xcb-struct-field-specifier ,@(element-syntax-syntax field)))

(define* (bitcase-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((bitcase (@ (name (,name #f)))
	      ,(expression-match -> expression)
	      ,((combine-matchers
		 fields-match
		 switch-match) -> bitcase-fields) ...)
     (guard
      ((false-or string?) name))
     (receive (fields switches)
	 (partition-elements bitcase-fields
			     '((field pad list) 1 #f)
                             '(switch 0 1))
       (let ((fields-syntax (map make-quoted-field fields)))
         (make-element-syntax
          'bitcase
          `(make-xcb-case-expression
            (quote ,(if name (normalize-name name) #f))
            ,(element-syntax-syntax expression)
            ,(cons 'list fields-syntax)
            ,(if (null? switches) #f
                 (element-syntax-syntax (car switches))))
          #f))))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define* (item-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((item (@ (name (,name #f)))
	   ,((combine-matchers
	      doc-match
	      expression-match) -> expressions-or-docs) ...)
     (guard
      ((false-or string?) name))
     (receive (expression doc)
	 (partition-elements expressions-or-docs
			     '(expression 0 1)
			     '(doc 0 1))
       (make-element-syntax
	'item
	(cons (normalize-name name #f)
	      (if (null? expression)
		  #f
		  (element-syntax-syntax (car expression))))
	`(,(normalize-name name) ,doc))))
    (,otherwise (xml-xcb-parse-error exp ignore-errors?))))

(define (append-if-not-present lis el)
  (if (not (find (lambda (mem) (eq? mem el)) lis))
      (append lis (list el))
      lis))

(define (merge-valueparam-with-fields valueparam fields)
  (let ((value-mask-name (assoc-ref valueparam 'value-mask-name))
	(value-mask-type (assoc-ref valueparam 'value-mask-type))
	(value-list-name (assoc-ref valueparam 'value-list-name)))
    (let ((value-list-syntax
	   (make-element-syntax
	    'list
	    `(,value-list-name
	      CARD32
	      *list*
	      (lambda (field-ref)
		(logcount (field-ref (quote ,value-mask-name)))))
	    #f)))
      (if (not (find
		(lambda (field)
		  (eq? (car (element-syntax-syntax field)) value-mask-name))
		fields))
	  (append fields
		  (list
		   (make-element-syntax
		    'field
		    `(,value-mask-name
		      ,(resolve-type-syntax value-mask-type #f #f #f))
		    #f)
		   value-list-syntax))
	  (append fields (list value-list-syntax))))))

(define (get-define-xcb-struct-syntax name-sym fields minimum-length switch)
  `(define-xcb-struct
     ,name-sym
     (,(symbol-append 'make- name-sym)
      ,@(delete
	 '*pad*
	 (map
	  (lambda (field)
	    (car (element-syntax-syntax field)))
	  (filter
	   (lambda (field)
	     (and
	      (not (eq? (element-syntax-tag field) 'exprfield))))
	   fields))))
     ,(symbol-append name-sym '?)
     ,(symbol-append name-sym '-type)
     ,switch
     ,minimum-length
     ,@(map element-syntax-syntax fields)))

(define (get-immediate-field-names fields has-valueparam?)
  (define (expr? field) (and (> (length field) 2) 
                             (or (eq? (caddr field) '*expr*)
                                 (eq? (caddr field) '*len*))))
  (define actual-fields (filter (lambda (f) (not (eq? (car f) '*pad*))) fields))
  (define required-fields (filter (lambda (f) (not (expr? f))) actual-fields))
  (if has-valueparam?
      (delq 'value-mask (map car required-fields))
      (map car required-fields)))

(define (get-field-names-with-valueparam fields valueparam)
  (if valueparam (append fields 'valueparams) fields))

(define (get-field-names-with-switch fields switch)
  (if switch (append fields (list (cadadr switch))) fields))

(define* (expression-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((op (@ (op ,op))
	 ,(expression-match -> expression1)
	 ,(expression-match -> expression2))
     (guard
      (find (lambda (el) (equal? el op)) '("+" "-" "/" "*" "&" "<<")))
     (make-element-syntax
      'expression
      `(lambda (field-ref) (,(proc-for-op op)
			    (,(element-syntax-syntax expression1) field-ref)
			    (,(element-syntax-syntax expression2) field-ref)))
      #f))
    ((unop (@ (op ,op))
	   ,(expression-match -> expression))
     (guard (equal? op "~"))
     (make-element-syntax
      'expression
      `(lambda (field-ref) (,(proc-for-op op)
			    (,(element-syntax-syntax expression) field-ref)))
      #f))
    ((fieldref ,fieldref) (guard (string? fieldref))
     (make-element-syntax 'expression
                          `(lambda (field-ref) (field-ref
                                                ',(normalize-name fieldref))) #f))
    ((enumref (@ (ref ,ref)) ,enumref) (guard (string? enumref) (string? ref))
     (make-element-syntax 'expression
                          `(lambda (field-ref) (xenum-ref
                                                ,(normalize-name ref)
                                                (quote ,(normalize-name enumref)))) #f))
    ((sumof (@ (ref ,ref))) (guard (string? ref))
     (make-element-syntax
      'expression
      `(lambda (field-ref)
	 (fold
	  xcb-add
	  0
	  (map
	   (lambda (listel) (listel))
	   (xcb-list-elements ,(normalize-name ref)))))
      #f))
    ((popcount ,(expression-match -> expression))
     (make-element-syntax
      'expression
      `(lambda (field-ref)
	 (logcount (,(element-syntax-syntax expression) field-ref))) #f))
    ((value ,value) (guard (dec-or-hex-integer? value))
     (make-element-syntax 'expression `(lambda (field-ref) ,(parse-dec-or-hex-integer value)) #f))
    ((bit ,bit) (guard (xml-integer? bit))
     (make-element-syntax 'expression `(lambda (field-ref) (ash 1 (string->number ,bit))) #f))
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
     (make-element-syntax 'see #f `((name . ,name) (type . ,(normalize-name type)))))
    (,otherwise (xml-xcb-parse-error exp #f))))

(define* (doc-match exp #:optional ignore-errors?)
  (sxml-match exp
    ((doc ,(doc-fields-match -> doc-fields) ...)
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

(define (request-syntax name fields switches replies valueparams opcode)
  (define request-struct-name (symbol-append  name '-struct))
  (define fields-syntax (map element-syntax-syntax fields))
  (define switch-syntax (if (null? switches) #f
                            (element-syntax-syntax (car switches))))
  (define has-reply? (not (null? replies)))
  (define valueparam-syntax
    (if (not (null? valueparams)) (element-syntax-syntax (car valueparams)) #f))
  (define immediate-field-argument-names
    (get-immediate-field-names fields-syntax valueparam-syntax))
  (define field-argument-names
    (get-field-names-with-switch
     (get-field-names-with-valueparam
      immediate-field-argument-names valueparam-syntax)
     switch-syntax))
  (define struct-syntax
    (get-define-xcb-struct-syntax
     request-struct-name
     (if valueparam-syntax
         (merge-valueparam-with-fields
          (element-syntax-syntax (car valueparams))
          fields)
         fields) 0 switch-syntax))
  (define valueparam-result-syntax
    (if valueparam-syntax
        `(valueparam ,(assq-ref valueparam-enums name) valueparams)
        `(values #f #f)))
  (define parsed-opcode (parse-dec-or-hex-integer opcode))
  (define construct-struct-arguments
    `(list
      ,@(append
         (map (lambda (field) `(cons (quote ,field) ,field))
              (get-field-names-with-switch
               immediate-field-argument-names switch-syntax))
         (if valueparam-syntax
             (list
              `(cons (quote ,(assq-ref valueparam-syntax 'value-mask-name))
                     valuemask)
              `(cons (quote ,(assq-ref valueparam-syntax 'value-list-name))
                     valuelist))
             '()))))
  `(begin
     ,struct-syntax
     ,@(if has-reply? (list (element-syntax-syntax (car replies))) '())
     (define-public (,(symbol-append name '/c) xcb-conn ,@field-argument-names)
       (call-with-values (lambda () ,valueparam-result-syntax)
         (lambda (valuemask valuelist)
           (define sequence-number
             (xcb-connection-send
              xcb-conn
              (if in-extension? extension-opcode ,parsed-opcode)
              (if in-extension? ,parsed-opcode #f)
              (xcb-struct-pack-to-bytevector
               (construct-xcb-struct
                ,request-struct-name
                ,construct-struct-arguments
                (xcb-connection-string-converter xcb-conn)))))
           ,(if has-reply?
                `(xcb-connection-register-reply-struct
                  xcb-conn sequence-number
                  ,(symbol-append name '-reply)))
           sequence-number)))
     (define-public (,name ,@field-argument-names)
       ,(if valueparam-syntax
            `(apply ,(symbol-append name '/c) (current-xcb-connection)
                    ,@immediate-field-argument-names valueparams)
            `(,(symbol-append name '/c) (current-xcb-connection)
              ,@immediate-field-argument-names)))))

 (define (enable-extension-procname header-symbol)
  ((symbol-prefix-proc ((symbol-prefix-proc 'xcb-enable-) header-symbol)) '!))

(define (top-level-match exp env)
  (sxml-match exp
    ((request (@ (name ,name)
		 (opcode ,opcode)
		 (combine-adjacent (,combine-adjacent #f)))
	      ,((combine-matchers
		 exprfield-match
		 valueparam-match
		 fields-match
		 switch-match
		 (lambda* (el #:optional ignore-error?)
		   (reply-match el (normalize-name name) ignore-error?))
		 doc-match) -> request-fields) ...)
     (guard
      (string? name)
      (xml-integer? opcode)
      ((false-or xml-boolean?) combine-adjacent))
     (receive (fields valueparams switches replies doc)
	 (partition-elements request-fields
			     '((field list pad exprfield) 0 #f)
			     '(valueparam 0 #f)
			     '(switch 0 1)
			     '(reply 0 1)
			     '(doc 0 1))
       (let ((request-name (normalize-name name)))
         (make-element-syntax
          'request
          (request-syntax
           request-name fields switches replies valueparams opcode)
          (if (= (length doc) 1)
              `(begin
                 (register-documentation
                  xcbdoc
                  'request ,request-name
                  ,(element-syntax-documentation-syntax (car doc)))
                 ,(if (and
                       (= (length replies) 1)
                       (element-syntax-documentation-syntax (car replies)))
                      `(register-documentation
                        xcbdoc
                        'reply ,request-name
                        ,(element-syntax-documentation-syntax (car replies)))
                      #f))
              #f)))))
    ((event (@ (name ,name)
	       (number ,number)
	       (no-sequence-number (,no-sequence-number? #f)))
	    ,((combine-matchers
	       doc-match
	       fields-match) -> fields) ...)
     (guard
      (string? name)
      (xml-integer? number)
      ((false-or xml-boolean?) no-sequence-number?))
     (receive (doc fields)
	 (partition-elements fields
			     '(doc 0 1)
			     '((field pad list) 0 #f))

       (let ((event-struct-name (make-event-name name)))
         (make-element-syntax
          'event

          `(begin
             ,(get-define-xcb-struct-syntax
               event-struct-name
               (if no-sequence-number?
                   fields
                   `(,(car fields) .
                     (,(make-element-syntax
                        'field '(sequence-number CARD16) #f) .
                        ,(cdr fields)))) 31 #f)
             (hashv-set! xcb-events ,(parse-dec-or-hex-integer number) ,event-struct-name))
          (if (= (length doc) 1)
              `(register-documentation xcbdoc
                                       'event ,event-struct-name
                                       ,(element-syntax-documentation-syntax (car doc)))
              #f)))))
    ((eventcopy (@ (name ,name)
		   (number ,number)
		   (ref ,ref)))

     (guard
      (string? name)
      (xml-integer? number)
      (string? ref))
     (make-element-syntax
      'eventcopy
      (let ((event-struct-name (make-event-name name))
	    (old-event-struct-name (make-event-name ref)))
	`(begin
           (clone-xcb-struct
            ,old-event-struct-name
            ,event-struct-name
            ,(symbol-append 'make- event-struct-name)
            ,(symbol-append event-struct-name '?)
            ,(symbol-append event-struct-name '-type))
	   (hashv-set! xcb-events ,(parse-dec-or-hex-integer number) ,event-struct-name))) #f))
    ((errorcopy (@ (name ,name)
		   (number ,number)
		   (ref ,ref)))
     (guard
      (string? name)
      (xml-integer? number)
      (string? ref))
     (make-element-syntax
      'errorcopy
      (let ((error-struct-name (make-error-name name))
	    (old-error-struct-name (make-error-name ref)))
	`(begin
	   (clone-xcb-struct
            ,old-error-struct-name
            ,error-struct-name
            ,(symbol-append 'make- error-struct-name)
            ,(symbol-append error-struct-name '?)
            ,(symbol-append error-struct-name '-type))
	   (hashv-set! xcb-errors ,(parse-dec-or-hex-integer number) ,error-struct-name))) #f))
    ((error (@ (name ,name)
	       (number ,number))
	    ,(fields-match -> fields) ...)
     (guard
      (string? name)
      (xml-integer? number))
     (receive (field) (partition-elements fields '((field pad list) 0 #f))
       (make-element-syntax
	'error
	(let ((error-struct-name (make-error-name name)))
	  `(begin
	     ,(get-define-xcb-struct-syntax
	       error-struct-name
	       fields 31
	       #f)
	     (hashv-set! xcb-errors ,(parse-dec-or-hex-integer number) ,error-struct-name))) #f)))
    ((import ,import) (guard (string? import))
     (if (not (string= "xproto" import))
         (make-element-syntax 'import `(use-modules (xcb xml ext ,(string->symbol import))) #f)
         (make-element-syntax 'import #f #f)))
    ((xidtype (@ (name ,name))) (guard (string? name))
     (let ((xid-name (normalize-name name)))
       (make-element-syntax
        'xidtype `(define-public
                    ,xid-name
                    (self-type (quote ,xid-name)
                               (pack-int-proc 4) (unpack-int-proc 4) #t)) #f)))
    ((xidunion (@ (name ,name))
	       (type ,types) ...)
     (guard
      (string? name)
      (>= (length types) 1)
      (every string? types))
     (let ((xidunion-name (normalize-name name)))
       (make-element-syntax
        'xidunion
        `(define-public
           ,xidunion-name
           (self-union-type
            (quote ,xidunion-name)
            (list ,@(map (lambda (type) (normalize-name type)) types))
            (pack-int-proc 4) (unpack-int-proc 4) #t)) #f)))
    ((enum (@ (name ,name))
	   ,((combine-matchers
	      item-match
	      doc-match) -> elements) ...)
     (guard
      (string? name))
     (receive (items doc)
	 (partition-elements elements
			     '(item 1 #f)
			     '(doc 0 1))
       (let ((enum-name (normalize-name name)))
        (make-element-syntax
         'enum
         `(begin
            (define-public ,enum-name (make-xcb-enum (quote ,enum-name)))
            ,@(let item-creator ((items items) (next-value 0))
                (if (not (null? items))
                    (let* ((item-syntax (element-syntax-syntax (car items)))
                           (provided-value (cdr item-syntax)))
                      (cons
                       `(xenum-set! ,enum-name
                                    (quote ,(car item-syntax))
                                    ,(if provided-value `(,provided-value #f) next-value))
                       (if provided-value
                           (item-creator (cdr items)
                                         (1+ ((eval provided-value (current-module)) #f)))
                           (item-creator (cdr items) (1+ next-value)))))
                    '())))
         (if (= (length doc) 1)
             `(register-documentation
               xcbdoc
               'enum ,enum-name
               ,(element-syntax-documentation-syntax (car doc)))
             #f)))))
    ((typedef (@ (oldname ,oldname)
		 (newname ,newname)))
     (guard (string? oldname)
	    (string? newname))
     (let ((new (make-type-name newname))
           (old (make-type-name oldname)))
      (make-element-syntax
       'typedef
       `(define-public ,new (clone-xcb-type (quote ,new) ,old))
       #f)))
    ((struct (@ (name ,name))
	     ,((combine-matchers
		fields-match
		switch-match) -> fields) ...)
     (guard
      (string? name))
     (receive (fields switches)
	 (partition-elements fields
			     '((field list pad exprfield) 0 #f)
			     '(switch 0 1))
       (if (= (length fields) 0)
	   (error "xml-xcb: struct must contain at least one fields element"))
       (let ((struct-name (normalize-name name)))
	 (make-element-syntax
	  'struct
	  (get-define-xcb-struct-syntax
	   struct-name fields 0
	   (if (not (null? switches))
	       (element-syntax-syntax (car switches))
	       #f)) #f))))
    ((xcb (@ (header ,header)
               (extension-xname (,extension-xname #f))
               (extension-name (,extension-name #f))
               (extension-multiword (,extension-multiword? #f))
               (major-version (,major-version #f))
               (minor-version (,minor-version #f))))
     (guard
      (string? header)
      ((false-or string?) extension-xname)
      ((false-or string?) extension-name)
      ((false-or xml-boolean?) extension-multiword?)
      ((false-or xml-integer?) major-version)
      ((false-or xml-integer?) minor-version))
     (let ((header-symbol (string->symbol header)))
       (make-element-syntax
        'xcb
        (if (and (module-defined? env 'no-new-xcb-module?)
                 (module-ref env 'no-new-xcb-module?))
            #f
            (if (eq? header-symbol 'xproto)
                `(define-module (xcb xml ,header-symbol))
                `(define-module (xcb xml ext ,header-symbol))))
        #f)))
    ((xcb-2 (@ (header ,header)
               (extension-xname (,extension-xname #f))
               (extension-name (,extension-name #f))
               (extension-multiword (,extension-multiword? #f))
               (major-version (,major-version #f))
               (minor-version (,minor-version #f))))
     (guard
      (string? header)
      ((false-or string?) extension-xname)
      ((false-or string?) extension-name)
      ((false-or xml-boolean?) extension-multiword?)
      ((false-or xml-integer?) major-version)
      ((false-or xml-integer?) minor-version))
     (let ((header-symbol (string->symbol header)))
       (make-element-syntax
        'xcb
        `(begin
           (use-modules
            (srfi srfi-1)
            (rnrs bytevectors)
            (xcb xml type)
            (xcb xml struct)
            (xcb xml union)
            (xcb xml enum)
            (xcb xml connection)
            (xcb xml enum)
            (xcb xml doc)
            (language xml-xcb support)
            ,@(if (not (eq? header-symbol 'xproto))
                  '((xcb xml xproto)
                    (xcb xml ext-support)) '()))
           (define extension-opcode #f)
           (define xcb-events (make-hash-table))
           (define xcb-errors (make-hash-table))
           (define (set-extension-opcode! opcode) (set! extension-opcode opcode))
           (define in-extension? ,(if (eq? header-symbol 'xproto) #f #t))
           (define-public (,(symbol-append (enable-extension-procname header-symbol) '/c)
                           xcb-conn reply)
             ,(if (eq? header-symbol 'xproto)
                  '(begin
                     (xcb-connection-register-events xcb-conn xcb-events 0)
                     (xcb-connection-register-errors xcb-conn xcb-errors 0))
                  `(enable-extension xcb-conn (quote ,header-symbol)
                                     set-extension-opcode!
                                     xcb-events xcb-errors reply)))
           (define-public (,(enable-extension-procname header-symbol) reply)
             (,(symbol-append (enable-extension-procname header-symbol) '/c)
              (current-xcb-connection) reply))
           (add-extension-info! ',header-symbol ,extension-xname
                                ,(enable-extension-procname header-symbol)))
        `(begin
           (define xcbdoc (empty-xcb-doc))
           (register-xcb-documentation (quote ,header-symbol) xcbdoc)))))
    ((union (@ (name ,name))
	    ,((combine-matchers
	       switch-match
	       fields-match) -> fields) ...)
     (guard (string? name))
     (receive (fields switches)
         (partition-elements fields
                             '((field list pad) 0 #f)
                             '(switch 0 1))
       (if (= (length fields) 0)
           (error "xml-xcb: struct must contain at least one fields element"))
       (if (= (length switches) 1)
           (error "xml-xcb: switches not supported in unions at present"))
       (let ((union-name (normalize-name name)))
         (make-element-syntax
          'union
          `(begin
             (define-xcb-union
               ,union-name
               ,(symbol-append 'make- union-name)
               ,@(map element-syntax-syntax fields))
             (define ,(symbol-append union-name '-type)
               (xcb-type-for-union ,union-name)))
          #f))))
    (,otherwise (xml-xcb-parse-error exp #f))))
