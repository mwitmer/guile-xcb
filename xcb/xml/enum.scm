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

(define-module (xcb xml enum)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml records)
  #:use-module (srfi srfi-9 gnu))

(define-public (make-xcb-enum name)
  (make-xcb-enum-internal name (make-hash-table) (make-hash-table)))

(define-public (xenum-set! enum key val)
  (hashq-set! (key-value-hash enum) key val)
  (hashq-set! (value-key-hash enum) val key))

(define-public (xenum-ref enum key)
  "-- Scheme Procedure: xenum-ref enum key
     Returns the numeric value stored in ENUM with key KEY."
  (hashq-ref (key-value-hash enum) key))

(define-public (xenum-keys enum)
  "-- Scheme Procedure: xenum-keys enum
     Return a list of all the keys in the enum."
  (hash-map->list (lambda (k v) k) (key-value-hash enum)))

(define-public (xenum-values enum)
  "-- Scheme Procedure: xenum-values enum
     Return a list of all the values in the enum."
  (hash-map->list (lambda (k v) k) (value-key-hash enum)))

(define-public (xenum-key-ref enum val)
  "-- Scheme Procedure: xenum-key-ref enum val
     Returns the symbol stored in ENUM for value VAL."
  (hashq-ref (value-key-hash enum) val))

(define-public (xenum-or enum . keys)
  "-- Scheme Procedure: xenum-or enum . keys
     Return the values associated with KEYS in xcb enum ENUM combined
     with `logior'."
  (apply logior (map (lambda (value) (xenum-ref enum value)) keys)))

