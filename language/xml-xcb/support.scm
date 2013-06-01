(define-module (language xml-xcb support)
  #:use-module (ice-9 regex))

(define hex-integer-regexp "^0x[0-9a-fA-F]+$")
(define xml-integer-regexp "^[-+]?[0-9]+$")

(define-public (parse-dec-or-hex-integer value)
  (if (string-match hex-integer-regexp value)
      (string->number (substring value 2) 16)
      (string->number value)))
