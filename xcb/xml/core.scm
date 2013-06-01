(define-module (xcb xml core)
  #:use-module (srfi srfi-1)
  #:use-module (xcb xml connection)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs base) #:select (vector-map))
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml xproto)
  #:use-module ((xcb xml ext bigreq) #:renamer (symbol-prefix-proc 'bigreq:))
  #:use-module ((xcb xml ext xc_misc) #:renamer (symbol-prefix-proc 'xc-misc:))
  #:export (enable-extension))

(define-public (bv->xcb-string bv)
  (let ((vec (make-vector (bytevector-length bv))))
    (for-each 
     (lambda (n)
       (vector-set! 
        vec n 
        (make-typed-value (integer->char (bytevector-u8-ref bv n)) char))) 
     (iota (bytevector-length bv)))
    vec))

(define-public (xcb-pad-bv bv)
  (let ((newbv (make-bytevector (* 2 (bytevector-length bv)) 0)))
    (for-each
     (lambda (n)
       (bytevector-u8-set! newbv n (bytevector-u8-ref bv n)))
     (iota (bytevector-length bv)))
    newbv))

(define-public (xcb-pad-string str)
  (string-pad-right str (* (string-length str) 2) (integer->char 0)))

(define-public (string->xcb str)
  (list->vector 
   (map (lambda (ch) (make-typed-value ch char))
        (string->list str))))

(define-public (xcb->string str)
  (apply string (vector->list (vector-map typed-value-value str))))

(define-public (xcb-bytes->string str)
  (apply string (map integer->char (vector->list (vector-map typed-value-value str)))))

(define-public (string->xcb2b str)
  (let ((str-bv (string->utf16 str (native-endianness))))
    (list->vector
     (fold-right
      (lambda (el prev)  
	(cons (make-CHAR2B 
	       (bytevector-u8-ref str-bv el)
	       (bytevector-u8-ref str-bv (+ el 1))) prev)) 
      '() (iota (string-length str) 0 2)))))

(define-public (next-xid-value xcb-conn)
  (if (update-xid-range-promise)
      (force (update-xid-range-promise)))
  (let* ((setup (xcb-connection-setup xcb-conn))
         (base (Setup-get-resource_id_base setup))
         (mask (Setup-get-resource_id_mask setup))
         (inc (logand mask (- mask)))
         (last-xid (xcb-connection-last-xid xcb-conn))
         (current-xid 
          (if (> (+ last-xid inc) mask)
              (begin
                (enable-xc-misc xcb-conn)
                (if (xc-misc-enabled? xcb-conn)
                    (update-xid-range! xcb-conn inc)
                    (error "xml-xcb: Not more xids available!")))
              (+ last-xid inc))))
    (if (= (xcb-connection-last-xid xcb-conn) last-xid)
        (set-xcb-connection-last-xid! xcb-conn current-xid))
    (logior current-xid base)))

(define-public (make-new-xid xcb-conn xcb-type)
  (make-typed-value (next-xid-value xcb-conn) xcb-type))

(define xc-misc-promise (make-parameter #f))

(define update-xid-range-promise (make-parameter #f))
(define (update-xid-range! xcb-conn inc)
  (update-xid-range-promise
   (xcb-await ((range (xc-misc:GetXIDRange xcb-conn)))
     (let ((xid-count (xc-misc:GetXIDRange-reply-get-count range))
           (xid-start (xc-misc:GetXIDRange-reply-get-start_id range))
           (setup (xcb-connection-setup xcb-conn)))
       (if (and (= xid-start 0) (= xid-count 1))
           (error "xml-xcb: Not more xids available!"))
       (set-xcb-connection-last-xid! xcb-conn xid-start)
       (Setup-set-resource_id_mask! 
        setup (* (+ xid-start (- xid-count 1)) inc))
       (update-xid-range-promise #f)
       xid-start)))
  (force (update-xid-range-promise)))

(define-public (enable-xc-misc xcb-conn) 
  (or (xc-misc-promise) 
      (xc-misc-promise
       (xcb-await ((q (QueryExtension xcb-conn 7 (string->xcb "XC-MISC"))))
         (define present? (QueryExtension-reply-get-present q))
         (xc-misc:set-extension-opcode! 
          (QueryExtension-reply-get-major_opcode q))
         (xc-misc-promise #f)
         (set-xc-misc-enabled! xcb-conn present?))))
  (force (xc-misc-promise)))

(define-public (enable-big-requests xcb-conn)
  (enable-extension 
   xcb-conn "BIG-REQUESTS" 
   bigreq:set-extension-opcode! 
   bigreq:xcb-events
   bigreq:xcb-errors 
   (lambda (reply)
     (xcb-await ((enable (bigreq:Enable xcb-conn)))
       (define setup (xcb-connection-setup xcb-conn))
       (set-big-requests-enabled! xcb-conn #t)
       (set-maximum-request-length! 
        xcb-conn
        (bigreq:Enable-reply-get-maximum_request_length enable))))))

(define* (enable-extension xcb-conn name set-opcode! events errors #:optional proc)
  (xcb-await ((reply (QueryExtension 
                      xcb-conn (string-length name) (string->xcb name))))
    (if (QueryExtension-reply-get-present reply)
        (begin
          (let ((opcode (QueryExtension-reply-get-major_opcode reply)))
            (set-opcode! opcode)
            (xcb-connection-register-events xcb-conn events opcode)
            (xcb-connection-register-errors xcb-conn errors opcode)
            (if proc (proc reply) #t)))
        #f)))
