(define-module (xcb xml ext xproto) 
  #:use-module (system base compile)
  #:use-module (language xml-xcb spec)
  #:use-module (xcb xml type) 
  #:use-module (xcb xml struct) 
  #:use-module (xcb xml enum) 
  #:use-module (xcb xml common))

(compile-and-load "/home/mark/source/guile-xcb/test.xml" #:from xml-xcb) 
