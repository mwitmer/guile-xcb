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

(define-module (xcb xml struct)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 binary-ports)
  #:use-module (xcb xml type)
  #:use-module (xcb xml union)
  #:use-module (xcb xml records)
  #:use-module (xcb xml enum)
  #:use-module ((rnrs bytevectors) #:select (bytevector-length))
  #:use-module ((rnrs base) #:select (vector-for-each vector-map))
  #:export (define-xcb-struct clone-xcb-struct 
             xcb-struct-field-specifier))

(define-public (resolve-type type mask enum require-enum?)
  (define base-type
   (cond
    ((xcb-struct? type) (xcb-type-for-struct type))
    ((xcb-union? type) (xcb-type-for-union type))
    ((xcb-type? type) type)
    (else (error "xml-xcb: Cannnot determine type name for " type))))
  (cond
   (enum (enum-type base-type enum require-enum?))
   (mask (mask-type base-type mask))
   (else base-type)))

(define (xcb-type-for-struct xcb-struct)
  (make-xcb-type
   (symbol-append (record-type-name (inner-type xcb-struct)) '-type)
   (type-predicate (record-predicate (inner-type xcb-struct)))
   (lambda (port rec) (xcb-struct-pack xcb-struct rec port))
   (lambda (port) (xcb-struct-unpack xcb-struct port))
   #f))

(define-public (xcb-struct-name xcb-struct)
  (record-type-name (inner-type xcb-struct)))

(define-public (xcb-struct-predicate xcb-struct)
  (record-predicate (inner-type xcb-struct)))

(define-syntax clone-xcb-struct
  (syntax-rules ()
    ((clone-xcb-struct
      xcb-struct new-xcb-struct constructor-name predicate-name xcb-struct-type)
     (begin
       (define-public new-xcb-struct
         (make-xcb-struct 
          (make-record-type 
           'new-xcb-struct
           (record-type-fields (inner-type xcb-struct))
           (lambda (rec port) (format port "#<~a>" new-xcb-struct)))
          (switch xcb-struct)
          (types xcb-struct)
          (field-value-expressions xcb-struct)
          (list-length-expressions xcb-struct)
          (get-constructor-args xcb-struct)
          (minimum-length xcb-struct)
          (field-order xcb-struct)))
       (define-public constructor-name
         (xcb-struct-constructor 
          new-xcb-struct (get-constructor-args xcb-struct)))
       (define-public predicate-name (xcb-struct-predicate new-xcb-struct))
       (define-public xcb-struct-type (xcb-type-for-struct new-xcb-struct))))))

(define-syntax define-xcb-struct
  (syntax-rules ()
    ((define-xcb-struct type
       (constructor constructor-tag ...)
       predicate
       type-name
       switch-expression
       minimum-length
       (field-tag xcb-type . more) ...)
     (begin
       (define-public type
       	 (make-xcb-struct-for-record-type 
       	  'type
       	  switch-expression
       	  (list
       	   (xcb-struct-field-specifier 
       	    field-tag xcb-type . more) ...)
	  '(constructor-tag ...)
          minimum-length))
       (define-public constructor 
         (xcb-struct-constructor type '(constructor-tag ...)))
       (define-public predicate (xcb-struct-predicate type))
       (define-public type-name (xcb-type-for-struct type))))))

(define-syntax xcb-struct-field-specifier
  (syntax-rules (*list* *pad* *expr*)
    ((_ *pad* pad-size)
     (list '*pad* pad-size))
    ((_ field-tag xcb-type *expr* field-value-expression)
     (list 'field-tag xcb-type '*expr* field-value-expression))
    ((_ field-tag xcb-type *list* list-length-expression)
     (list 'field-tag xcb-type '*list* list-length-expression))
    ((_ field-tag xcb-type)
     (list 'field-tag xcb-type #f))))

(define (make-xcb-struct-for-record-type 
	 xcb-struct-name switch all-field-specifiers 
         my-constructor-args minimum-length)
  (define types (make-hash-table))
  (define field-specifiers
    (filter (lambda (fs) (not (eq? (car fs) '*pad* ))) all-field-specifiers))
  (define list-length-expressions (make-hash-table))
  (define field-value-expressions (make-hash-table))
  (define (field-visible? field)
    (or (< (length field) 3) (not (eq? (caddr field) '*expr*))))
  (define (process-type field-specifier)
    (define xcb-type 
      (if (eq? (caddr field-specifier) '*list*)
          (list-type (cadr field-specifier))
          (cadr field-specifier)))
    (if (xcb-type-list? xcb-type)
        (hashq-set! 
         list-length-expressions (car field-specifier)
         (cadddr field-specifier)))
    (if (eq? (caddr field-specifier) '*expr*)
        (hashq-set! field-value-expressions 
                    (car field-specifier)
                    (cadddr field-specifier)))
    (hashq-set! types (car field-specifier) xcb-type))
  (define switch-name (if switch (xcb-switch-name switch) #f))
  (define basic-fields 
    (cons
     'xcb-struct-type
     (map car (filter field-visible? field-specifiers))))

  (for-each process-type field-specifiers)

  (make-xcb-struct
   (make-record-type 
    xcb-struct-name 
    (if switch-name (append basic-fields (list switch-name)) basic-fields)
    (lambda (rec port) (format port "#<~a>" xcb-struct-name)))
   switch
   types
   field-value-expressions
   list-length-expressions
   my-constructor-args
   minimum-length
   all-field-specifiers))

(define-public (xcb-switch-values xcb-struct rec)
  (define accessor
    (record-accessor
     (inner-type xcb-struct) (xcb-switch-name (switch xcb-struct))))
  (accessor rec))

(define* ((xcb-struct-field-ref-proc xcb-struct rec #:optional alist) field)
  "Returns a procedure that will return the value for FIELD in the
given REC. If FIELD is not present in REC, it will check to see if
FIELD is actually in the form LIST_len, and if so, return the length
of list LIST in REC"
  (define (look-general accessor fields)
    (cond
     ((memq field fields) (accessor rec field))
     ((string= (string-take-right (symbol->string field) 4) "_len")
      (let ((len-field
             (string->symbol (string-drop-right (symbol->string field) 4))))
        (if (memq len-field fields) (vector-length (accessor rec len-field)) 
            #f)))
     (else #f)))
  (define (look-in-alist) 
    (define (accessor rec field) (assq-ref alist field))
    (define fields (map car alist))
    (look-general accessor fields))
  (define (look-in-rec)
    (define (accessor rec field) 
      (define val ((record-accessor (inner-type xcb-struct) field) rec))
      (if (vector? val) val (typed-value-value val)))
    (define fields (record-type-fields (inner-type xcb-struct)))
    (look-general accessor fields))
  (or (if alist (look-in-alist) #f) 
      (look-in-rec)
      (error "xml-xcb: False or missing value for field " field rec)))

(define (type-wrap el xcb-type)
  (define item (xcb-maybe-get-from-enum xcb-type el))
  (typecheck (if (typed-value? item) item (make-typed-value item xcb-type))))

(define (xcb-struct-constructor xcb-struct fields)   
  (define constructor (record-constructor 
                       (inner-type xcb-struct) 
                       (cons 'xcb-struct-type fields)))
  (define (process-value arg field)
    (define xcb-type (hashq-ref (types xcb-struct) field))
    (if (xcb-type-list? xcb-type)
        (vector-map (lambda (el) (type-wrap el xcb-type)) arg)
        (type-wrap arg xcb-type)))
  (lambda args 
    (apply constructor 
           (cons xcb-struct (map-in-order process-value args fields)))))

(define-public (construct-xcb-struct xcb-struct field-arg-alist)
  (define (box-value field-arg)
    (define xcb-type (hashq-ref (types xcb-struct) (car field-arg)))
    (define (require-type value) 
      (if (typed-value? value) value
          (error "xml-xcb: Passing unboxed value of opaque type" value)))
    (define switch-name 
      (if (switch xcb-struct) (xcb-switch-name (switch xcb-struct)) #f))
    (define value (cdr field-arg))
    (cond
     ((eq? (car field-arg) switch-name) (cdr field-arg))
     ((xcb-type-opaque? xcb-type)
      (if (xcb-type-list? xcb-type)
          (vector-map require-type value)
          (require-type value)))
     ((xcb-type-list? xcb-type)
      (if (vector? value) value
          (error "xml-xcb: Attempt to pass a non-vector \
as an xcb list type" value))
      (vector-map (lambda (el) (type-wrap el xcb-type)) value))
     (else (type-wrap value xcb-type))))

  (apply 
   (record-constructor 
    (inner-type xcb-struct) 
    (cons 'xcb-struct-type (map car field-arg-alist)))
   (cons xcb-struct (map box-value field-arg-alist))))

(define (xcb-struct-for-rec rec)
  (define rtd (record-type-descriptor rec))
  ((record-accessor rtd 'xcb-struct-type) rec))

(define (xcb-struct-predicate xcb-struct) 
  (record-predicate (inner-type xcb-struct)))

(define-public xref
  (case-lambda
    ((rec field)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field) rec))
    ((rec field n)
     ((xcb-struct-accessor (xcb-struct-for-rec rec) field) rec n))))

(define-public xset!
  (case-lambda
    ((rec field val)
     ((xcb-struct-modifier (xcb-struct-for-rec rec) field) rec val))
    ((rec field n val)
     ((xcb-struct-modifier (xcb-struct-for-rec rec) field) rec n val))))

(define (xcb-struct-accessor xcb-struct field-tag)
  (define (maybe-unbox val) 
    (define opaque? (xcb-type-opaque? (typed-value-type val)))
    (if opaque? val (typed-value-value-or-enum val)))
  (define (accessor rec)
    (define value ((record-accessor (inner-type xcb-struct) field-tag) rec))
    (if (vector? value) (vector-map maybe-unbox value) (maybe-unbox value)))
  (case-lambda
    ((rec) (accessor rec))
    ((rec n) (vector-ref (accessor rec) n))))

(define (xcb-maybe-get-from-enum xcb-type arg)
  (define (mask-or mask) (apply xenum-or mask arg))
  (define (enum-get enum) 
    (or (xenum-ref enum arg)
        (if (xcb-type-require-enum? xcb-type)
            (error "xcb-xml: No enum value with name " arg)
            arg)))
  (cond
   ((xcb-type-mask xcb-type) => mask-or)
   ((xcb-type-enum xcb-type) => enum-get)
   (else arg)))

(define (xcb-struct-modifier xcb-struct field-tag)
  (define xcb-type (hashq-ref (types xcb-struct) field-tag))
  (define modifier (record-modifier (inner-type xcb-struct) field-tag))
  (case-lambda 
    ((rec val)
     (define wrapped 
       (if (xcb-type-list? xcb-type) 
           (vector-map (lambda (el) (type-wrap el xcb-type)) val)
           (type-wrap val xcb-type)))
     (modifier rec wrapped))
    ((rec n val)
     (define accessor (record-accessor (inner-type xcb-struct) field-tag))
     (vector-set! (accessor rec) n (type-wrap val xcb-type)))))

(define (check-list-length xcb-struct rec field value alist)
  (define list-length (cadddr field))
  (define (check-length)
    (define expected-length 
      (list-length (xcb-struct-field-ref-proc xcb-struct rec alist)))
    (if (not (= expected-length (vector-length value)))
        (error "xml-xcb: Wrong length list in struct."
               expected-length (vector-length value))))
  (or (not list-length) (check-length)))

(define ((pack-field xcb-struct rec port alist) field)
 (define (write-pad-bytes n) (do-ec (: i 0 n) (put-u8 port 0)))
 (define (exprfield-resolve xcb-struct rec field)
   (and-let* 
       ((field-value-expression
         (hashq-ref (field-value-expressions xcb-struct)
                    (car field))))
     (make-typed-value
      (field-value-expression 
       (xcb-struct-field-ref-proc xcb-struct rec alist))
      (cadr field))))
 (define (pack-value value) (typed-value-pack value port))
 (define (pack-value-or-list value)
   (cond
    ((vector? value)
     (check-list-length xcb-struct rec field value alist)
     (vector-for-each pack-value value)
     (if (and (> (vector-length value) 0)
              (eq? (typed-value-type (vector-ref value 0)) char))
         (write-pad-bytes 
          (- (ceiling-remainder (vector-length value) 4)))))
    (else (pack-value value))))  
 (define ((pack-value-in-assoc xcb-type) key-val)
   (pack-value-or-list (type-wrap (cdr key-val) xcb-type)))
 (define field-name (car field))
 (define field-type (cadr field))
 (if (eq? field-name '*pad*) (write-pad-bytes field-type)
     (cond
      ((exprfield-resolve xcb-struct rec field) => pack-value)
      ((assq field-name alist) => (pack-value-in-assoc field-type))
      (else 
       (pack-value-or-list
        ((record-accessor (inner-type xcb-struct) field-name) rec))))))

(define-public (xcb-struct-pack xcb-struct rec port)
  (define ((pack-switch port) struct-switch)
    (define struct-switch (switch xcb-struct))
    (define accessor 
      (record-accessor (inner-type xcb-struct) (xcb-switch-name struct-switch)))
    (switch-pack struct-switch xcb-struct rec (accessor rec) port))
  (define packed
   (call-with-values (lambda () (open-bytevector-output-port))
     (lambda (port get-bytevector)
       (for-each (pack-field xcb-struct rec port '()) (field-order xcb-struct))
       (and=> (switch xcb-struct) (pack-switch port))
       (get-bytevector))))
  (define min (minimum-length xcb-struct))
  (define len (bytevector-length packed))
  (put-bytevector port packed)
  (if (< len min) (do-ec (: i 0 (- min len)) (put-u8 port 0))))

(define ((unpack-field xcb-struct rec port alist modify?) field)
  (define skip-pad-bytes (lambda (n) (do-ec (: i 0 n) (get-u8 port))))
  (define (unpack-value field-type) 
    (make-typed-value (typed-value-unpack field-type port) field-type))
  (define (unpack-list field-name field-type modifier)
    (define list-length-expression 
      (or
       (hashq-ref (list-length-expressions xcb-struct) field-name)
       (if (> (length field) 2) (cadddr field) #f)))
    (define size 
      (if list-length-expression
          (list-length-expression
           (xcb-struct-field-ref-proc xcb-struct rec alist))
          (error "xml-xcb: No length provided for list in" xcb-struct)))
    (modifier rec (vector-ec (: i 0 size) (unpack-value field-type))))
  (define field-name (car field))
  (define field-type (cadr field))
  (define (modify-or-return)
    (define modifier 
      (if modify? (record-modifier (inner-type xcb-struct) field-name)
          (lambda (rec a) a)))
    (if (eq? (caddr field) '*list*)
        (unpack-list field-name field-type modifier)
        (modifier rec (unpack-value field-type))))
  (cond
   ((eq? field-name '*pad*) (skip-pad-bytes field-type))
   ((eq? (caddr field) '*expr*) #f)
   (else (modify-or-return))))

(define-public (xcb-struct-unpack xcb-struct port)
  (define rec ((record-constructor 
                (inner-type xcb-struct)
                '(xcb-struct-type)) xcb-struct))
  (define (unpack-switch switch) 
    (define values (switch-unpack switch xcb-struct rec '() port))
    (define modifier 
      (record-modifier (inner-type xcb-struct) (xcb-switch-name switch)))
    (modifier rec values))
  (define fields (field-order xcb-struct))
  (for-each (unpack-field xcb-struct rec port '() #t) fields)
  (and=> (switch xcb-struct) unpack-switch)
  rec)

(define-public (valueparam enum els)
  (define (number-replace el) (cons (xenum-ref enum (car el)) (cdr el)))
  (define with-numbers (map number-replace els))
  (define sorted (sort with-numbers (lambda (el1 el2) (< (car el1) (car el2)))))
  (values
   (apply logior (map car sorted))
   (list->vector (map-in-order cdr sorted))))

(define-public (xcb-struct-unpack-from-bytevector xcb-struct bv)
  (define port (open-bytevector-input-port bv))
  (xcb-struct-unpack xcb-struct port))

(define-public (xcb-struct-pack-to-bytevector xcb-struct rec)
  (call-with-values (lambda () (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (xcb-struct-pack xcb-struct rec port)
      (get-bytevector))))

(define (bitmatch? match mask) (not (zero? (logand match mask))))

(define (switch-unpack switch xcb-struct rec alist port)
  (define bitmask ((xcb-switch-expression switch) 
                   (xcb-struct-field-ref-proc xcb-struct rec alist)))
  (define (case-expression-unpack case-expression alist)
    (define bitmatch ((xcb-case-expression-expression case-expression)
                      (xcb-struct-field-ref-proc xcb-struct rec alist)))
    (define fields (xcb-case-expression-fields case-expression))
    (define ((unpack-switch so-far) switch) 
      (switch-unpack switch xcb-struct rec so-far port))
    (define (unpack-accum field so-far)
      (define unpacked ((unpack-field xcb-struct rec port so-far #f) field))      
      (define unboxed 
        (cond
         ((unspecified? unpacked) *unspecified*)
         ((vector? unpacked) (vector-map typed-value-value unpacked))
         (else (typed-value-value unpacked))))
      (if (not (unspecified? unpacked)) 
          (cons (cons (car field) unboxed) so-far) so-far))
    (define new-alist
      (if (bitmatch? bitmatch bitmask) (fold unpack-accum alist fields) alist))
    (append new-alist (or (and=> (xcb-case-expression-switch case-expression)
                                 (unpack-switch new-alist)) '())))
  (define (unpack-default default)
    (define typed-value ((unpack-field xcb-struct rec port alist #f) default))
    (list (cons (car default) (typed-value-value typed-value))))
  (define (case-expression-accum ce so-far)
    (case-expression-unpack ce so-far))
  (define unpack-results
    (fold case-expression-accum '() (xcb-switch-case-expressions switch)))
  (if (null? unpack-results) 
      (or (and=> (xcb-switch-default switch) unpack-default) '()) 
      unpack-results))

(define (switch-pack switch xcb-struct rec alist port)
  (define bitmask 
    ((xcb-switch-expression switch) (xcb-struct-field-ref-proc xcb-struct rec)))
  (define (case-expression-pack case-expression)
    (define bitmatch ((xcb-case-expression-expression case-expression)
                      (xcb-struct-field-ref-proc xcb-struct rec)))
    (define (pack-fields)
      (define fields (xcb-case-expression-fields case-expression))
      (define (pack-switch switch)
        (switch-pack switch xcb-struct rec alist port))
      (map-in-order (pack-field xcb-struct rec port alist) fields)
      (and=> (xcb-case-expression-switch case-expression) pack-switch)
      (filter (lambda (a) (memq (car a) (map car fields))) alist))
    (if (bitmatch? bitmatch bitmask) (pack-fields) '()))
  (define (pack-default default)
    (define (pack-with-match key-val)
      ((pack-field xcb-struct rec port alist) default))
    (or (and=> (assq (car default) alist) pack-with-match)
        (error "xml-xcb: No default value provided for switch")))
  (define (case-expression-accum ce fields) 
    (append (case-expression-pack ce) fields))
  (define pack-results 
    (fold case-expression-accum '() (xcb-switch-case-expressions switch)))
  (if (null? pack-results) (and=> (xcb-switch-default switch) pack-default) '()))
