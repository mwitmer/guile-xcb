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


;;; A whole lot of record types. Do NOT recompile this file unless you
;;; want to invalidate all the compiled xml-xcb files (that's why
;;; these are all kept in one place)

(define-module (xcb xml records)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (xcb-case-expression-expression
            xcb-case-expression-fields
            xcb-case-expression
            make-xcb-switch 
            xcb-struct?
            inner-type
            xcb-switch-name
            types
            switch
            make-xcb-struct
            get-constructor-args
            field-value-expressions
            field-order
            list-length-expressions
            xcb-switch-expression
            xcb-case-expression-switch
            xcb-switch-case-expressions
            xcb-switch-default
            make-xcb-case-expression
            value-key-hash
            key-value-hash
            make-xcb-enum-internal
            xcb-union-list-length-expressions
            xcb-union-underlying-record-type
            first-field-name
            xcb-union-types
            make-xcb-union
            xcb-union?
            make-xcb-type
            xcb-type
            xcb-type-name
            typed-value-value
            xcb-type-pack
            xcb-type-unpack
            xcb-type-predicate
            typed-value-type
            make-typed-value
            xcb-type-opaque?
            xcb-type-enum
            xcb-type-list?
            xcb-type-mask
            xcb-type-require-enum?
            typed-value?
            xcb-type?
            xcb-type-size
            xcb-enum-name))

(define-record-type xcb-struct-type
  (make-xcb-struct 
   inner-type
   switch
   types
   field-value-expressions
   list-length-expressions
   constructor-args
   field-order)
  xcb-struct?
  (inner-type inner-type)
  (switch switch)
  (types types)
  (field-value-expressions field-value-expressions)
  (list-length-expressions list-length-expressions)
  (constructor-args get-constructor-args)
  (field-order field-order))

(set-record-type-printer! 
 xcb-struct-type
 (lambda (rec port)
   (format port "#<xcb-struct-type: ~a" (record-type-name (inner-type rec)))
   (for-each 
    (lambda (field)
      (let ((field-name (car field))
            (field-type (hashq-ref (types rec) (car field))))
        (format port " ~a: ~a" field-name field-type))) 
    (filter 
     (lambda (field) (not (eq? (car field) '*pad*)))
     (field-order rec)))
   (display ">" port)))

(define-record-type xcb-switch
  (make-xcb-switch name expression case-expressions default)
  xcb-switch?
  (name xcb-switch-name)
  (expression xcb-switch-expression)
  (case-expressions xcb-switch-case-expressions)
  (default xcb-switch-default))

(define-record-type xcb-case-expression
  (make-xcb-case-expression name expression fields switch)
  xcb-case-expression?
  (name xcb-case-expression-name)
  (expression xcb-case-expression-expression)
  (fields xcb-case-expression-fields)
  (switch xcb-case-expression-switch))

(define-record-type xcb-enum
  (make-xcb-enum-internal name key-value-hash value-key-hash)
  xcb-enum?
  (name xcb-enum-name)
  (key-value-hash key-value-hash)
  (value-key-hash value-key-hash))

(set-record-type-printer! xcb-enum
  (lambda (rec port)
    (format port "#<xcb-enum:")
    (hash-for-each (lambda (k v) (format port " ~a" k))
                   (key-value-hash rec))
    (display ">" port)))

(define-record-type xcb-union
  (make-xcb-union 
   first-field-name types 
   list-length-expressions underlying-record-type)
  xcb-union?
  (types xcb-union-types)
  (list-length-expressions xcb-union-list-length-expressions)
  (first-field-name first-field-name)
  (underlying-record-type xcb-union-underlying-record-type))

(define-record-type xcb-type
  (make-xcb-type name predicate pack unpack opaque?)
  xcb-type?
  (name xcb-type-name)
  (predicate xcb-type-predicate)
  (pack xcb-type-pack)
  (unpack xcb-type-unpack)
  (opaque? xcb-type-opaque?)
  (list? xcb-type-list?)
  (mask xcb-type-mask)
  (enum xcb-type-enum)
  (require-enum? xcb-type-require-enum?))

(define-record-type typed-value
  (make-typed-value value type)
  typed-value?
  (value typed-value-value)
  (type typed-value-type))

(set-record-type-printer! 
 typed-value 
 (lambda (value port)
   (format 
    port "<~a (~a)>"     
    (typed-value-value value)
    (typed-value-type value))))

(set-record-type-printer!
 xcb-type
 (lambda (value port)
   (format port "<~a>" (xcb-type-description value))))

(define (xcb-type-description type)
  (define name 
    (format #f "~a~a"
            (xcb-type-name type)
            (if (xcb-type-list? type) "[]" "")))
  (cond 
   ((xcb-type-enum type) => 
    (lambda (enum) (format #f "~a ~a" name (xcb-enum-name enum))))
   ((xcb-type-mask type) =>
    (lambda (mask) (format #f "~a ~a" name (xcb-enum-name mask))))
   (else name)))
