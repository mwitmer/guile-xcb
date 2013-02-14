(define-module (language xml-xcb spec)
  #:use-module (sxml simple)
  #:use-module (language xml-xcb compile-scheme)
  #:use-module (system base language)
  #:export (xml-xcb))

(define-language xml-xcb
  #:title "xml-xcb"
  #:reader (lambda (port env) 
	     (while (and (not (eof-object? (peek-char port))) 
			 (char-whitespace? (peek-char port))) 
	       (read-char port))
	     (if (eof-object? (peek-char port))
		 (peek-char port)
		 (xml->sxml port #:trim-whitespace? #t)))
  #:compilers `((scheme . ,compile-scheme))
  #:printer write)
