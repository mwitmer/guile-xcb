(define-module (xcb xml common)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 regex)
  #:export (typed-value
	    typed-value-predicate
	    typed-value-value
	    typed-value?
	    make-typed-value))

(define hex-integer-regexp "^0x[0-9a-fA-F]+$")
(define xml-integer-regexp "^[-+]?[0-9]+$")

(define-record-type typed-value
  (make-typed-value value predicate)
  typed-value?
  (value typed-value-value)
  (predicate typed-value-predicate))

(set-record-type-printer! 
 typed-value 
 (lambda (value port)
   (format 
    port "<~a: ~a>" 
    (procedure-name (typed-value-predicate value))
    (typed-value-value value))))

(define-public (typed-value-check val)
  ((typed-value-predicate val) val))

(define-once documentation (make-hash-table))

(define (hash-force-nested-set! h keys value)
  (if (= (length keys) 1)
      (if (hash-ref h (car keys) #f) 
	  (error "xml-xcb: Overwriting pre-existing documentation for" keys)
	  (hash-set! h (car keys) value))
      (let ((subhash (or (hash-ref h (car keys) #f) (make-hash-table))))
	(hash-force-nested-set! subhash (cdr keys) value)
	(hash-set! h (car keys) subhash))))

(define (hash-nested-ref h keys)
  (if (> (length keys) 1)
      (let ((next-hash (hash-ref h (car keys) #f)))
	(if next-hash (hash-nested-ref next-hash (cdr keys)) #f))
      (hash-ref h (car keys) #f)))

(define-public (register-documentation dochash type path doc)
  (hash-force-nested-set! dochash
			  (if (list? path) 
			      (cons type path)
			      (list type path)) doc))

(define-public (register-xcb-documentation header doc)
  (hash-set! documentation header doc))

(define (display-documentation-multifield name lst)
  (format #t "\n~a:\n-------" name)
  (for-each (lambda (field)
	      (format #t "\n~a~a\n----------\n" 
		      (assq-ref field 'name)
		      (if (assq-ref field 'type)
			  (format #f " ~a" (assq-ref field 'type)) ""))
	      (if (assq-ref field 'description)
		  (format #t "~a" (assq-ref field 'description)))) lst)
  (newline))

(define-public (print-documentation documentation)
  (print-documentation-brief documentation)
  (let ((fields
	 (assq-ref documentation 'fields))
	(errors
	 (assq-ref documentation 'errors))
	(see
	 (assq-ref documentation 'see))
	(description
	 (assq-ref documentation 'description))
	(example
	 (assq-ref documentation 'example)))
    (if description
	(format #t "\nDescription: ~a\n\n" description))
    (if fields (display-documentation-multifield "Fields" fields))
    (if errors (display-documentation-multifield "Errors" errors))
    (if see (begin
	      (format #t "See Also:\n---------")
	      (map (lambda (see-field)
		     (format #t "\nType: ~a Name: ~a" 
			     (assq-ref see-field 'type)
			     (assq-ref see-field 'name))) see)))
    (if example
	(format #t "\nExample: ~a\n\n" example)))
  (newline))

(define-public (print-documentation-brief documentation)
    (and-let* ((brief (assq-ref documentation 'brief)))
	(format #t "\nBrief: ~a\n\n" brief)))

(define-public (document-brief package type . path)
  (document package type print-documentation-brief path))

(define-public (document-full package type . path)
  (document package type print-documentation path))

(define (document package type proc path)
  (let ((my-documentation
	 (hash-nested-ref
	  documentation
	  (cons package (cons type path)))))
    (if my-documentation
	(if (hash-table? my-documentation)
	    my-documentation
	    (proc my-documentation))
	(format #t "No documentation found for ~a in type ~a\n" path type))))

(define-public (parse-dec-or-hex-integer value)
  (if (string-match hex-integer-regexp value)
      (string->number (substring value 2) 16)
      (string->number value)))
