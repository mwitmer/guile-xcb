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

(define-module (xcb xml doc)
  #:use-module (srfi srfi-2))

(define-public (empty-xcb-doc) (make-hash-table))
(define-public documentation (make-hash-table))

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
  (hash-force-nested-set!
   dochash
   (if (list? path) 
       (cons type path)
       (list type path)) doc))

(define-public (register-xcb-documentation header doc)
  (hash-set! documentation header doc))

(define (display-documentation-multifield name lst)
  (format #t "\n~a:\n-------" name)
  (for-each 
   (lambda (field)
     (format #t "\n~a~a\n----------\n" 
             (assq-ref field 'name)
             (if (assq-ref field 'type)
                 (format #f " ~a" (assq-ref field 'type)) ""))
     (if (assq-ref field 'description)
         (format #t "~a" (assq-ref field 'description)))) lst)
  (newline))

(define-public (print-documentation documentation)
  (define fields (assq-ref documentation 'fields))
  (define errors (assq-ref documentation 'errors))
  (define see (assq-ref documentation 'see))
  (define description (assq-ref documentation 'description))
  (define example (assq-ref documentation 'example))
  (print-documentation-brief documentation)
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
      (format #t "\nExample: ~a\n\n" example))
  (newline))

(define-public (print-documentation-brief documentation)
  (and-let* ((brief (assq-ref documentation 'brief)))
    (format #t "\nBrief: ~a\n\n" brief)))

(define-public (document-brief package type . path)
  (document package type print-documentation-brief path))

(define-public (document-full package type . path)
  (document package type print-documentation path))

(define (document package type proc path)
  (define my-documentation
    (hash-nested-ref
     documentation
     (cons package (cons type path))))
  (if my-documentation
      (if (hash-table? my-documentation)
          my-documentation
          (proc my-documentation))
      (format #t "No documentation found for ~a in type ~a\n" path type)))
