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
