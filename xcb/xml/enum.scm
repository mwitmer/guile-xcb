(define-module (xcb xml enum)
  #:use-module (srfi srfi-9)
  #:use-module (xcb xml records)
  #:use-module (srfi srfi-9 gnu))

(define-public (make-xcb-enum name)
  (make-xcb-enum-internal name (make-hash-table) (make-hash-table)))

(define-public (xcb-enum-set! enum key val)
  (hashq-set! (key-value-hash enum) key val)
  (hashq-set! (value-key-hash enum) val key))

(define-public (xcb-enum-get enum key)
  (hashq-ref (key-value-hash enum) key))

(define-public (xcb-enum-key-get enum val)
  (hashq-ref (value-key-hash enum) val))

(define-public (xcb-enum-or enum . keys)
  (apply logior (map (lambda (value) (xcb-enum-get enum value)) keys)))
