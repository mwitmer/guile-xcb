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
  #:use-module (ice-9 regex)
  #:export (normalize-name))

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

(define-public (remove-prefix str)
  (define in (string-index str #\:))
  (if in (string-drop str (+ in 1)) str))

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

(define-public (underscores->dashes str)
  (let make-dash ((start 0) (str str))
    (define (process i)
      (make-dash (+ i 1)
                 (string-append
                  (string-take str i) "-"
                  (string-drop str (+ i 1)))))
    (or (and=> (string-index str #\_ start) process) str)))

(define-public (camelcase->dash str)
  (let make-dash ((start 0) (str str))
    (define (process i)
      (define j (string-index str (lambda (c) (not (char-upper-case? c))) i))
      (if (< i (string-length str))
       (make-dash (+ i 1)
                  (string-append
                   (string-take str i)
                   (if (or (= i 0) (char=? (string-ref str (- i 1)) #\-))
                       "" "-")
                   (string-downcase
                    (substring str i (or j (string-length str))))
                   (if j (string-drop str (+ i (- j i))) "")))))
    (or (and=> (string-index str char-upper-case? start) process) str)))

(define* (normalize-name str #:optional (prepend-x? #t))
  (define (fold-upper c uc?)
    (and uc? (or (char-numeric? c)
                 (char=? c #\_)
                 (char-upper-case? c))))
  (define dstr (underscores->dashes str))
  (string->symbol
   (if (string-fold fold-upper #t dstr)
       (if (> (string-length dstr) 1)
           (if prepend-x?
               (string-append "x" (string-downcase dstr))
               (string-downcase dstr))
           (string-downcase dstr))
       (camelcase->dash dstr))))

(define-public valueparam-enums
  '((create-window . xcw)
    (configure-window . config-window)
    (change-window-attributes . xcw)
    (create-gc . xgc)
    (change-gc . xgc)
    (change-keyboard-control . xkb)
    (create-picture . xcp)
    (change-picture . xcp)
    (create-alarm . xca)
    (change-alarm . xca)
    (set-attributes . xcw)))

(define built-in-types
  '(CARD8 CARD16 CARD32 BYTE char BOOL void INT8 INT16 INT32))

(define-public (make-type-name name)
  (define sym (string->symbol name))
  (if (memq sym built-in-types)
      sym
      (normalize-name name)))

(define-public (make-event-name name)
  (symbol-append (normalize-name name) '-event))

(define-public (make-error-name name)
  (symbol-append (normalize-name name) '-error))
