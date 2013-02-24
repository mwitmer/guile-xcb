(define-module (xcb xml enum)
  #:use-module (srfi srfi-9))

(define-record-type xcb-enum
  (make-xcb-enum-internal key-value-hash value-key-hash)
  xcb-enum?
  (key-value-hash key-value-hash)
  (value-key-hash value-key-hash))

(define-public (make-xcb-enum)
  (make-xcb-enum-internal (make-hash-table) (make-hash-table)))

(define-public (xcb-enum-set! enum key val)
  (hashq-set! (key-value-hash enum) key val)
  (hashq-set! (value-key-hash enum) val key))

(define-public (xcb-enum-get enum key)
  (hashq-ref (key-value-hash enum) key))

(define-public (xcb-enum-key-get enum val)
  (hashq-ref (value-key-hash enum) val))
