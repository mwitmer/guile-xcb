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

(define-module (language xml-xcb support)
  #:use-module (ice-9 regex))

(define-public (false-or pred?) (lambda (exp) (or (not exp) (pred? exp))))

(define-public (xml-boolean? value)
  (and
   (string? value)
   (or (equal? (string-downcase value) "true") 
       (equal? (string-downcase value) "false"))))

(define hex-integer-regexp "^ *(0x[0-9a-fA-F]+) *$")
(define xml-integer-regexp "^ *([-+]?[0-9]+) *$")

(define-public (xml-integer? value)
  (if (string-match xml-integer-regexp value) #t #f))

(define-public (remove-prefix sym)
  (define sym-str (symbol->string sym))
  (define in (string-index sym-str #\:))
  (string->symbol
   (if in (string-drop sym-str (+ in 1)) sym-str)))

(define-public (dec-or-hex-integer? value)
  (or 
   (xml-integer? value)
   (and
    (string? value)
    (if (string-match hex-integer-regexp value) #t #f))))

(define-public (parse-dec-or-hex-integer value)
  (define trim-value (string-trim-both value))
  (if (string-match hex-integer-regexp trim-value)
      (string->number (substring trim-value 2) 16)
      (string->number trim-value)))
