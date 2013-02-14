(define-module (xcb xml type)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (type-self-predicate
	    typecheck
	    list-predicate
	    type-predicate
	    predicate-opaque?
	    predicate-list?
	    typed-value-value
	    typed-value-predicate
	    make-typed-value))

(define predicate-opaque? (make-object-property))
(define predicate-list? (make-object-property))

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

(define (typecheck value)
  (if (not ((typed-value-predicate value) value))
      (error (format #f "Value ~a does not satisfy its type predicate" 
		     (typed-value-value value)) 
	     (typed-value-predicate value)))
  value)

(define-syntax type-self-predicate
  (syntax-rules ()
    ((_ name)
     (letrec ((name (lambda (val)
		      (eq? (typed-value-predicate val) name))))
       (set! (predicate-opaque? name) #t)
       name))))

(define-syntax list-predicate
  (syntax-rules ()
    ((_ predicate)
     (letrec ((pred (lambda (val)
		      (predicate val))))
       (set! (predicate-list? pred) #t)
       pred))))

(define-public (type-predicate predicate)
  (lambda (val)
    (predicate (typed-value-value val))))
