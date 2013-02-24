(define-module (xcb xml test)
  #:use-module (srfi srfi-64)
  #:use-module (sxml simple)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base compile)
  #:use-module (language xml-xcb spec)
  #:use-module (language scheme spec)
  #:use-module (xcb xml common)
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml struct))

(define (test-reader string)
  (xml->sxml string #:trim-whitespace? #t))

(define (pack-xcb-struct-to-bytevector xcb-struct rec)
 (call-with-values
     (lambda ()
       (open-bytevector-output-port))
   (lambda (port get-bytevector)
     (xcb-struct-pack xcb-struct rec port)
     (get-bytevector))))

(define test-xml "<xcb header=\"xproto\">
  <!-- Core protocol types -->
  
  <struct name=\"CHAR2B\">
    <field type=\"CARD8\" name=\"byte1\" />
    <field type=\"CARD8\" name=\"byte2\" />
  </struct>

  <struct name=\"FORMAT\">
    <field type=\"CARD8\" name=\"depth\" />
    <field type=\"CARD8\" name=\"bits_per_pixel\" />
    <pad bytes=\"5\" />
    <field type=\"CARD8\" name=\"scanline_pad\" />
  </struct>

  <typedef oldname=\"CARD32\" newname=\"VISUALID\" />

  <enum name=\"VisualClass\">
    <item name=\"StaticGray\"> <value>0</value></item>
    <item name=\"GrayScale\">  <value>1</value></item>
    <item name=\"StaticColor\"><value>2</value></item>
    <item name=\"PseudoColor\"><value>3</value></item>
    <item name=\"TrueColor\">  <value>4</value></item>
    <item name=\"DirectColor\"><value>5</value></item>
  </enum>

  <struct name=\"VISUALTYPE\">
    <field type=\"VISUALID\" name=\"visual_id\" />
    <field type=\"CARD8\" name=\"class\" enum=\"VisualClass\" />
    <field type=\"CARD8\" name=\"bits_per_rgb_value\" altenum=\"VisualClass\" />
    <field type=\"CARD16\" name=\"colormap_entries\" />
    <field type=\"CARD32\" name=\"red_mask\" />
    <field type=\"CARD32\" name=\"green_mask\" />
    <field type=\"CARD32\" name=\"blue_mask\" />
    <pad bytes=\"4\" />
  </struct>

  <struct name=\"DEPTH\">
    <field type=\"CARD8\" name=\"depth\" />
    <pad bytes=\"1\" />
    <field type=\"CARD16\" name=\"visuals_len\" />
    <pad bytes=\"4\" />
    <list type=\"VISUALTYPE\" name=\"visuals\">
      <fieldref>visuals_len</fieldref>
    </list>
  </struct>

  <enum name=\"BitEnum\">
    <item name=\"One\"><bit>0</bit></item>
    <item name=\"Two\"><bit>1</bit></item>
    <item name=\"Four\"><bit>2</bit></item>
    <item name=\"Eight\"><bit>3</bit></item>
    <item name=\"Sixteen\"><bit>4</bit></item>
  </enum>

  <struct name=\"MASKSTRUCT\">
    <field type=\"CARD8\" name=\"masked\" mask=\"BitEnum\" />
  </struct>

  <struct name=\"MASKLISTSTRUCT\">
    <field type=\"CARD8\" name=\"mask_len\" />
    <list type=\"CARD8\" mask=\"BitEnum\" name=\"mask_list\">
      <fieldref>mask_len</fieldref>
    </list>
  </struct>

  <struct name=\"ENUMLISTSTRUCT\">
    <field type=\"CARD8\" name=\"enum_len\" />
    <list type=\"CARD8\" enum=\"BitEnum\" name=\"enum_list\">
      <fieldref>enum_len</fieldref>
    </list>
    <field type=\"CARD8\" name=\"altenum_len\" />
    <list type=\"CARD8\" altenum=\"BitEnum\" name=\"altenum_list\">
      <fieldref>altenum_len</fieldref>
    </list>
  </struct>

</xcb>")

(define-public (xml->scheme) 
  (pretty-print (compile (test-reader test-xml) #:from xml-xcb #:to scheme)))

(test-begin "struct-test")
(compile (test-reader test-xml) #:from xml-xcb #:env (current-module))
(let ((my-char2b (make-CHAR2B 20 10)))
  (test-eqv (CHAR2B-get-byte1 my-char2b) 20)
  (test-eqv (CHAR2B-get-byte2 my-char2b) 10))

(let* ((port (open-bytevector-input-port #vu8(10 20))))
  (receive (size my-char2b)
      (xcb-struct-unpack CHAR2B 2 port)
    (test-eqv (CHAR2B-get-byte1 my-char2b) 10)
    (test-eqv (CHAR2B-get-byte2 my-char2b) 20)))

(let* ((port (open-bytevector-input-port #vu8(1 2 0 0 0 0 0 3))))
  (receive (size my-format)
      (xcb-struct-unpack FORMAT 8 port)
    (test-eqv (FORMAT-get-depth my-format) 1)
    (test-eqv (FORMAT-get-bits_per_pixel my-format) 2)
    (test-eqv (FORMAT-get-scanline_pad my-format) 3)))

(test-equal
 (pack-xcb-struct-to-bytevector FORMAT (make-FORMAT 3 4 5))
 #vu8(3 4 0 0 0 0 0 5))

(typecheck (make-typed-value 
	    (make-VISUALTYPE 3 'StaticColor 3 3 3 3 3)
	    VISUALTYPE-type))

(let ((my-visualtype (make-VISUALTYPE 3 'StaticColor 3 3 3 3 3)))
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 3)
  (VISUALTYPE-set-bits_per_rgb_value! my-visualtype 'StaticColor)
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 2)
  (VISUALTYPE-set-bits_per_rgb_value! my-visualtype 8)
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 8))

(let ((my-visualtype (make-VISUALTYPE 3 'StaticColor 'PseudoColor 3 3 3 3)))
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 3))

(let ((my-depth (make-DEPTH 2 1)))
  (DEPTH-set-visuals! my-depth 0 (make-VISUALTYPE 3 'PseudoColor 3 3 3 3 3))
  (test-equal
   (pack-xcb-struct-to-bytevector DEPTH my-depth)
   #vu8(2 0 1 0 0 0 0 0 3 0 0 0 3 3 3 0 3 0 0 0 3 0 0 0 3 0 0 0 0 0 0 0)))

(let* ((port 
	(open-bytevector-input-port #vu8(2 0 1 0 0 0 0 0 
					   3 0 0 0 3 3 3 
					   0 3 0 0 0 3 0 
					   0 0 3 0 0 0 0
					   0 0 0))))
  (receive (size my-depth)
      (xcb-struct-unpack DEPTH 32 port)
    (test-eqv size 32)
    (test-eqv (DEPTH-get-depth my-depth) 2)
    (let ((visual (DEPTH-get-visuals my-depth 0)))
      (test-eqv (VISUALTYPE-get-green_mask visual) 3)
      (test-eqv (VISUALTYPE-get-class visual) 3))))

(let ((my-maskstruct (make-MASKSTRUCT 3)))
  (test-eqv (MASKSTRUCT-get-masked my-maskstruct) 3)
  (MASKSTRUCT-set-masked! my-maskstruct 5)
  (test-eqv (MASKSTRUCT-get-masked my-maskstruct) 5)
  (MASKSTRUCT-set-masked! my-maskstruct 'Eight #t)
  (test-eqv (MASKSTRUCT-get-masked my-maskstruct) 13)
  (MASKSTRUCT-set-masked! my-maskstruct '(Four Eight) #f)
  (test-eqv (MASKSTRUCT-get-masked my-maskstruct) 1)
  (MASKSTRUCT-set-masked! my-maskstruct '(Four Sixteen) #t)
  (test-eqv (MASKSTRUCT-get-masked my-maskstruct) 21)
  (test-error (MASKSTRUCT-set-masked! my-maskstruct '(Four Sixteen))))

(let ((my-maskliststruct (make-MASKLISTSTRUCT 2)))
  (MASKLISTSTRUCT-set-mask_list! my-maskliststruct 0 '(Four Eight) #t)
  (test-eqv (MASKLISTSTRUCT-get-mask_list my-maskliststruct 0) 12)
  (MASKLISTSTRUCT-set-mask_list! my-maskliststruct 0 'Eight #f)
  (test-eqv (MASKLISTSTRUCT-get-mask_list my-maskliststruct 0) 4))

(let ((my-enumliststruct (make-ENUMLISTSTRUCT 1 1)))
  (ENUMLISTSTRUCT-set-altenum_list! my-enumliststruct 0 'Eight)
  (test-eqv (ENUMLISTSTRUCT-get-altenum_list my-enumliststruct 0) 8)
  (ENUMLISTSTRUCT-set-altenum_list! my-enumliststruct 0 64)
  (test-eqv (ENUMLISTSTRUCT-get-altenum_list my-enumliststruct 0) 64)
  (ENUMLISTSTRUCT-set-enum_list! my-enumliststruct 0 'Four)
  (test-eqv (ENUMLISTSTRUCT-get-enum_list my-enumliststruct 0) 4)
  (test-error (ENUMLISTSTRUCT-set-enum_list! my-enumliststruct 0 8)))

(test-end "struct-test")
