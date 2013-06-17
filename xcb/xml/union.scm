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

(define-module (xcb xml union)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml type)
  #:use-module (xcb xml records)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 receive)
  #:export (define-xcb-union))

(define-syntax define-xcb-union
  (syntax-rules ()
    ((define-xcb-union xcb-union-name constructor-name
       (field-name field-type . more) ...)
     (begin 
      (define xcb-union-name 
	(make-xcb-union-for-fields 
	 (quote xcb-union-name)
	 (list
	  (define-xcb-union-fields field-name field-type . more) ...)))
      (define (constructor-name data)
	((record-constructor 
          (xcb-union-underlying-record-type xcb-union-name) '(xcb-union-type data)) 
         xcb-union-name
         data))))))

(define-syntax define-xcb-union-fields
  (syntax-rules (*list*)
    ((_ field-tag xcb-type *list* list-length-expression)
     (list (quote field-tag) xcb-type '*list* list-length-expression))
    ((_ field-tag xcb-type)
     (list (quote field-tag) xcb-type '*field* #f))))

(define (make-xcb-union-for-fields name fields)
  (define types (make-hash-table))
  (define list-length-expressions (make-hash-table))
  (map
   (lambda (field-specifier)
     (define field-type
      (if (eq? (caddr field-specifier) '*list*)
          (begin
            (hashq-set!
             list-length-expressions
             (car field-specifier) 
             (cadddr field-specifier))
            (list-type (cadr field-specifier)))
          (cadr field-specifier)))
     (hashq-set! types (car field-specifier) field-type))
   fields)
  (make-xcb-union
   (caar fields)
   types
   list-length-expressions
   (make-record-type name '(xcb-union-type data))))

(define-public (xcb-type-for-union xcb-union)
  (make-xcb-type
   (symbol-append 
    (record-type-name (xcb-union-underlying-record-type xcb-union)) '-type)
   (type-predicate 
    (record-predicate (xcb-union-underlying-record-type xcb-union)))
   (lambda (port rec) (xcb-union-pack xcb-union rec port))
   (lambda (port) (xcb-union-unpack xcb-union port))
   #f))

(define-public (xcb-union-pack xcb-union rec port)
  (put-bytevector 
   port 
   ((record-accessor (xcb-union-underlying-record-type xcb-union) 'data) rec)))

(define (xcb-union-unpack-list xcb-union type port)
  (define list-length 
    ((hashq-ref 
      (xcb-union-list-length-expressions xcb-union) 
      (first-field-name xcb-union)) #f))
  (u8-list->bytevector 
   (fold-right 
    append '() 
    (map (lambda (n) 
           (bytevector->u8-list 
            (xcb-union-unpack-single-item xcb-union type port)))
         (iota list-length)))))

(define (xcb-union-unpack-single-item xcb-union test-type port)
  (define obj ((xcb-type-unpack test-type) port))
  (receive (bytevector-port get-bytevector) 
      (open-bytevector-output-port)
    ((xcb-type-pack test-type) bytevector-port obj)
    (get-bytevector)))

(define-public (xcb-union-unpack xcb-union port) 
  (define test-type
   (hashq-ref (xcb-union-types xcb-union) (first-field-name xcb-union)))
  (define bv 
    (if (xcb-type-list? test-type)
        (xcb-union-unpack-list xcb-union test-type port)
        (xcb-union-unpack-single-item xcb-union test-type port)))
  ((record-constructor (xcb-union-underlying-record-type xcb-union) '(xcb-union-type data)) 
   xcb-union bv))

(define (xcb-union-read-list-data xcb-union port field type)
  (define list-length 
    ((hashq-ref (xcb-union-list-length-expressions xcb-union) field) #f))
  (list->vector
   (map 
    (lambda (n) (xcb-union-read-data xcb-union port type)) 
    (iota list-length))))

(define (xcb-union-read-data xcb-union port type)
  (define obj ((xcb-type-unpack type) port)) 
  (make-typed-value obj type))

(define-public (xunion-ref rec field)
  (define xcb-union ((record-accessor (record-type-descriptor rec) 'xcb-union-type) rec))
  (define type (hashq-ref (xcb-union-types xcb-union) field))
  (define port 
    (open-bytevector-input-port 
     ((record-accessor 
       (xcb-union-underlying-record-type xcb-union) 'data) rec)))
  (if (xcb-type-list? type)
      (xcb-union-read-list-data xcb-union port field type)
      (xcb-union-read-data xcb-union port type)))
