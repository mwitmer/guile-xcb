(define-module (xcb xml test)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (sxml simple)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 pretty-print)
  #:use-module (rnrs bytevectors)
  #:use-module (system base compile)
  #:use-module (language xml-xcb spec)
  #:use-module (language scheme spec)
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (xcb xml union)
  #:use-module ((xcb xml records) #:select (make-typed-value typed-value-value))
  #:use-module (xcb xml connection)
  #:use-module (xcb xml struct))

(define (test-reader string)
  (xml->sxml string #:trim-whitespace? #t))

(define-public (pack-xcb-struct-to-bytevector xcb-struct rec)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (xcb-struct-pack xcb-struct rec port)
      (get-bytevector))))

(define in-extension? #f)

(define-public test-xml "<test-root><xcb header=\"xproto\"></xcb>
  <!-- Core protocol types -->
  
  <struct name=\"CHAR2B\">
    <field type=\"CARD8\" name=\"byte1\" />
    <field type=\"CARD8\" name=\"byte2\" />
  </struct>

  <xidtype name=\"FONT\" />
  <xidtype name=\"GCONTEXT\" />

  <xidunion name=\"FONTABLE\">
    <type>FONT</type>
    <type>GCONTEXT</type>
  </xidunion>

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

  <enum name=\"CW\">
    <item name=\"BackPixmap\">      <bit>0</bit></item>
    <item name=\"BackPixel\">       <bit>1</bit></item>
    <item name=\"BorderPixmap\">    <bit>2</bit></item>
    <item name=\"BorderPixel\">     <bit>3</bit></item>
    <item name=\"BitGravity\">      <bit>4</bit></item>
    <item name=\"WinGravity\">      <bit>5</bit></item>
    <item name=\"BackingStore\">    <bit>6</bit></item>
    <item name=\"BackingPlanes\">   <bit>7</bit></item>
    <item name=\"BackingPixel\">    <bit>8</bit></item>
    <item name=\"OverrideRedirect\"><bit>9</bit></item>
    <item name=\"SaveUnder\">       <bit>10</bit></item>
    <item name=\"EventMask\">       <bit>11</bit></item>
    <item name=\"DontPropagate\">   <bit>12</bit></item>
    <item name=\"Colormap\">        <bit>13</bit></item>
    <item name=\"Cursor\">          <bit>14</bit></item>
  </enum>

  <enum name=\"WindowClass\">
    <item name=\"CopyFromParent\"><value>0</value></item>
    <item name=\"InputOutput\">   <value>1</value></item>
    <item name=\"InputOnly\">     <value>2</value></item>
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

  <xidtype name=\"WINDOW\" />

  <union name=\"ClientMessageData\">
    <!-- The format member of the ClientMessage event determines which array
         to use. -->
    <list type=\"CARD8\"  name=\"data8\" ><value>20</value></list> <!--  8 -->
    <list type=\"CARD16\" name=\"data16\"><value>10</value></list> <!-- 16 -->
    <list type=\"CARD32\" name=\"data32\"><value>5</value></list>  <!-- 32 -->
  </union>

  <request name=\"CreateWindow\" opcode=\"1\">
    <field type=\"CARD8\" name=\"depth\" />
    <field type=\"WINDOW\" name=\"wid\" />
    <field type=\"WINDOW\" name=\"parent\" />
    <field type=\"INT16\" name=\"x\" />
    <field type=\"INT16\" name=\"y\" />
    <field type=\"CARD16\" name=\"width\" />
    <field type=\"CARD16\" name=\"height\" />
    <field type=\"CARD16\" name=\"border_width\" />
    <field type=\"CARD16\" name=\"class\" enum=\"WindowClass\" />
    <field type=\"VISUALID\" name=\"visual\" />
    <valueparam value-mask-type=\"CARD32\"
                value-mask-name=\"value_mask\"
                value-list-name=\"value_list\" />
  </request>

  <enum name=\"Atom\">
    <item name=\"None\"> <value>0</value></item>
    <item name=\"Any\">  <value>0</value></item>
    <item name=\"PRIMARY\" />
    <item name=\"SECONDARY\" />
    <item name=\"ARC\" />
    <item name=\"ATOM\" />
    <item name=\"BITMAP\" />
    <item name=\"CARDINAL\" />
    <item name=\"COLORMAP\" />
    <item name=\"CURSOR\" />
    <item name=\"CUT_BUFFER0\" />
    <item name=\"CUT_BUFFER1\" />
    <item name=\"CUT_BUFFER2\" />
    <item name=\"CUT_BUFFER3\" />
    <item name=\"CUT_BUFFER4\" />
    <item name=\"CUT_BUFFER5\" />
    <item name=\"CUT_BUFFER6\" />
    <item name=\"CUT_BUFFER7\" />
    <item name=\"DRAWABLE\" />
    <item name=\"FONT\" />
    <item name=\"INTEGER\" />
    <item name=\"PIXMAP\" />
    <item name=\"POINT\" />
    <item name=\"RECTANGLE\" />
    <item name=\"RESOURCE_MANAGER\" />
    <item name=\"RGB_COLOR_MAP\" />
    <item name=\"RGB_BEST_MAP\" />
    <item name=\"RGB_BLUE_MAP\" />
    <item name=\"RGB_DEFAULT_MAP\" />
    <item name=\"RGB_GRAY_MAP\" />
    <item name=\"RGB_GREEN_MAP\" />
    <item name=\"RGB_RED_MAP\" />
    <item name=\"STRING\" />
    <item name=\"VISUALID\" />
    <item name=\"WINDOW\" />
    <item name=\"WM_COMMAND\" />
    <item name=\"WM_HINTS\" />
    <item name=\"WM_CLIENT_MACHINE\" />
    <item name=\"WM_ICON_NAME\" />
    <item name=\"WM_ICON_SIZE\" />
    <item name=\"WM_NAME\" />
    <item name=\"WM_NORMAL_HINTS\" />
    <item name=\"WM_SIZE_HINTS\" />
    <item name=\"WM_ZOOM_HINTS\" />
    <item name=\"MIN_SPACE\" />
    <item name=\"NORM_SPACE\" />
    <item name=\"MAX_SPACE\" />
    <item name=\"END_SPACE\" />
    <item name=\"SUPERSCRIPT_X\" />
    <item name=\"SUPERSCRIPT_Y\" />
    <item name=\"SUBSCRIPT_X\" />
    <item name=\"SUBSCRIPT_Y\" />
    <item name=\"UNDERLINE_POSITION\" />
    <item name=\"UNDERLINE_THICKNESS\" />
    <item name=\"STRIKEOUT_ASCENT\" />
    <item name=\"STRIKEOUT_DESCENT\" />
    <item name=\"ITALIC_ANGLE\" />
    <item name=\"X_HEIGHT\" />
    <item name=\"QUAD_WIDTH\" />
    <item name=\"WEIGHT\" />
    <item name=\"POINT_SIZE\" />
    <item name=\"RESOLUTION\" />
    <item name=\"COPYRIGHT\" />
    <item name=\"NOTICE\" />
    <item name=\"FONT_NAME\" />
    <item name=\"FAMILY_NAME\" />
    <item name=\"FULL_NAME\" />
    <item name=\"CAP_HEIGHT\" />
    <item name=\"WM_CLASS\" />
    <item name=\"WM_TRANSIENT_FOR\" />
  </enum>

  <typedef oldname=\"CARD32\" newname=\"TIMESTAMP\" />
  <xidtype name=\"ATOM\" />

  <enum name=\"Time\">
    <item name=\"CurrentTime\"> <value>0</value> </item>
  </enum>

  <enum name=\"Window\">
    <item name=\"None\"> <value>0</value></item>
  </enum>

<request name=\"TestStructPack\" opcode=\"4\">
    <pad bytes=\"1\" />
    <field type=\"DEPTH\" name=\"depth\" />
    <field type=\"CARD32\" name=\"last\" />
</request>

  <enum name=\"KeyButMask\">
    <item name=\"Shift\">  <bit>0</bit></item>
    <item name=\"Lock\">   <bit>1</bit></item>
    <item name=\"Control\"><bit>2</bit></item>
    <item name=\"Mod1\">   <bit>3</bit></item>
    <item name=\"Mod2\">   <bit>4</bit></item>
    <item name=\"Mod3\">   <bit>5</bit></item>
    <item name=\"Mod4\">   <bit>6</bit></item>
    <item name=\"Mod5\">   <bit>7</bit></item>
    <item name=\"Button1\"><bit>8</bit></item>
    <item name=\"Button2\"><bit>9</bit></item>
    <item name=\"Button3\"><bit>10</bit></item>
    <item name=\"Button4\"><bit>11</bit></item>
    <item name=\"Button5\"><bit>12</bit></item>
  </enum>


<request name=\"SetSelectionOwner\" opcode=\"22\">
    <pad bytes=\"1\" />
    <field type=\"WINDOW\" name=\"owner\" altenum=\"Window\" />
    <field type=\"ATOM\" name=\"selection\" />
    <field type=\"TIMESTAMP\" name=\"time\" altenum=\"Time\" />
  </request>

  <typedef oldname=\"CARD8\" newname=\"KEYCODE\" />

  <event name=\"ClientMessageTest\" number=\"4\">
    <pad bytes=\"1\" />
    <field type=\"ClientMessageData\" name=\"data\" />
  </event>

  <event name=\"KeyPress\" number=\"2\">
    <field type=\"KEYCODE\" name=\"detail\" />
    <field type=\"TIMESTAMP\" name=\"time\" />
    <field type=\"WINDOW\" name=\"root\" />
    <field type=\"WINDOW\" name=\"event\" />
    <field type=\"WINDOW\" name=\"child\" altenum=\"Window\" />
    <field type=\"INT16\" name=\"root_x\" />
    <field type=\"INT16\" name=\"root_y\" />
    <field type=\"INT16\" name=\"event_x\" />
    <field type=\"INT16\" name=\"event_y\" />
    <field type=\"CARD16\" name=\"state\" mask=\"KeyButMask\" />
    <field type=\"BOOL\" name=\"same_screen\" />
    <pad bytes=\"1\" />
  </event>
  <eventcopy name=\"KeyRelease\" number=\"3\" ref=\"KeyPress\" />

  <error name=\"Value\" number=\"2\">
    <field type=\"CARD32\" name=\"bad_value\" />
    <field type=\"CARD16\" name=\"minor_opcode\" />
    <field type=\"CARD8\" name=\"major_opcode\" />
    <pad bytes=\"1\" />
  </error>

  <errorcopy name=\"Window\" number=\"3\" ref=\"Value\" />

  <enum name=\"FontDraw\">
    <item name=\"LeftToRight\"><value>0</value></item>
    <item name=\"RightToLeft\"><value>1</value></item>
  </enum>

  <enum name=\"ImageFormat\">
    <item name=\"XYBitmap\"><value>0</value></item>
    <item name=\"XYPixmap\"><value>1</value></item>
    <item name=\"ZPixmap\"> <value>2</value></item>
  </enum>

  <xidtype name=\"PIXMAP\" />

  <xidunion name=\"DRAWABLE\">
    <type>WINDOW</type>
    <type>PIXMAP</type>
  </xidunion>

  <!-- FIXME: data array in reply will include padding, but ought not to. -->
  <request name=\"GetImage\" opcode=\"73\">
    <field type=\"CARD8\" name=\"format\" enum=\"ImageFormat\" />
    <field type=\"DRAWABLE\" name=\"drawable\" />
    <field type=\"INT16\" name=\"x\" />
    <field type=\"INT16\" name=\"y\" />
    <field type=\"CARD16\" name=\"width\" />
    <field type=\"CARD16\" name=\"height\" />
    <field type=\"CARD32\" name=\"plane_mask\" />
    <reply>
      <field type=\"CARD8\" name=\"depth\" />
      <field type=\"VISUALID\" name=\"visual\" />
      <pad bytes=\"20\" />
      <list type=\"BYTE\" name=\"data\">
        <op op=\"*\">
          <fieldref>length</fieldref>
          <value>4</value>
        </op>
      </list>
    </reply>
  </request>

  <request name=\"QueryTextExtents\" opcode=\"48\">
    <exprfield type=\"BOOL\" name=\"odd_length\">
      <op op=\"&amp;\"><fieldref>string_len</fieldref><value>1</value></op>
    </exprfield>
    <field type=\"FONTABLE\" name=\"font\" />
    <list type=\"CHAR2B\" name=\"string\" />
    <reply>
      <field type=\"BYTE\" name=\"draw_direction\" enum=\"FontDraw\" />
      <field type=\"INT16\" name=\"font_ascent\" />
      <field type=\"INT16\" name=\"font_descent\" />
      <field type=\"INT16\" name=\"overall_ascent\" />
      <field type=\"INT16\" name=\"overall_descent\" />
      <field type=\"INT32\" name=\"overall_width\" />
      <field type=\"INT32\" name=\"overall_left\" />
      <field type=\"INT32\" name=\"overall_right\" />
    </reply>
  </request></test-root>")

(define no-new-xcb-module? #f)

(define-public (print-scheme)
  (set! no-new-xcb-module? #f)
  (for-each
   (lambda (sxml)
     (pretty-print (compile sxml #:from xml-xcb #:env (current-module) #:to scheme)))
   (cdadr (test-reader test-xml)))
  (set! no-new-xcb-module? #t))

(define xcb-events (make-hash-table))
(define xcb-errors (make-hash-table))

(map 
 (lambda (sxml)
   (compile sxml #:from xml-xcb #:env (current-module)))
 (cdadr (test-reader test-xml)))

(define-public (string->xcb-char2b-vector str) 
  (let ((str-bv (string->utf16 str (native-endianness))))
    (list->vector
     (fold-right
      (lambda (el prev)  
	(cons (make-CHAR2B 
	       (bytevector-u8-ref str-bv el)
	       (bytevector-u8-ref str-bv (+ el 1))) prev)) 
      '() (iota (string-length str) 0 2)))))

(test-begin "xcb-test")

(let ((my-char2b (make-CHAR2B 20 10)))
  (test-eqv (CHAR2B-get-byte1 my-char2b) 20)
  (test-eqv (CHAR2B-get-byte2 my-char2b) 10))

(let* ((port (open-bytevector-input-port #vu8(10 20))))
  (let ((my-char2b
         (xcb-struct-unpack CHAR2B port)))
    (test-eqv (CHAR2B-get-byte1 my-char2b) 10)
    (test-eqv (CHAR2B-get-byte2 my-char2b) 20)))

(let* ((port (open-bytevector-input-port #vu8(1 2 0 0 0 0 0 3))))
  (let ((my-format
         (xcb-struct-unpack FORMAT port)))
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
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 'PseudoColor)
  (VISUALTYPE-set-bits_per_rgb_value! my-visualtype 'StaticColor)
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 'StaticColor)
  (VISUALTYPE-set-bits_per_rgb_value! my-visualtype 8)
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 8))

(let ((my-visualtype (make-VISUALTYPE 3 'StaticColor 'PseudoColor 3 3 3 3)))
  (test-eqv (VISUALTYPE-get-bits_per_rgb_value my-visualtype) 'PseudoColor))

(let ((my-depth (make-DEPTH 2 1 `#(,(make-VISUALTYPE 3 'PseudoColor 3 3 3 3 3)))))
  (test-equal
   (pack-xcb-struct-to-bytevector DEPTH my-depth)
   #vu8(2 0 1 0 0 0 0 0 3 0 0 0 3 3 3 0 3 0 0 0 3 0 0 0 3 0 0 0 0 0 0 0)))

(let ((port
       (open-bytevector-input-port #vu8(1 2 3 4 5 6 7 8 9 0
                                          1 2 3 4 5 6 7 8 9 0))))
  (let ((my-ClientMessageData
         (xcb-union-unpack ClientMessageData port)))
    (let ((data8-vec (xcb-union-get ClientMessageData my-ClientMessageData 'data8))
	  (data16-vec (xcb-union-get ClientMessageData my-ClientMessageData 'data16))
	  (data32-vec (xcb-union-get ClientMessageData my-ClientMessageData 'data32)))
      (test-eqv (typed-value-value (vector-ref data8-vec 2)) 3)
      (test-eqv (typed-value-value (vector-ref data16-vec 2)) 1541)
      (test-eqv (typed-value-value (vector-ref data32-vec 2)) 33619977))))

(let ((port
       (open-bytevector-input-port #vu8(2 0 1 0 0 0 0 0 
					  3 0 0 0 3 3 3 0 
					  3 0 0 0 3 0 0 0 
					  3 0 0 0 0 0 0 0))))
  (let ((my-depth
         (xcb-struct-unpack DEPTH port)))
    (test-eqv (DEPTH-get-depth my-depth) 2)
    (let ((visual (DEPTH-get-visuals my-depth 0)))
      (test-eqv (VISUALTYPE-get-green_mask visual) 3)
      (test-eqv (VISUALTYPE-get-class visual) 'PseudoColor))))

(let* ((my-maskstruct (make-MASKSTRUCT '(One Two)))
       (mask-list (MASKSTRUCT-get-masked my-maskstruct)))
  (test-assert (memq 'One mask-list))
  (test-assert (memq 'Two mask-list))
  (test-eqv (length mask-list) 2)
  (MASKSTRUCT-set-masked! my-maskstruct '(Two Four))
  (let ((new-mask-list (MASKSTRUCT-get-masked my-maskstruct)))
    (test-assert (memq 'Two new-mask-list))
    (test-assert (memq 'Four new-mask-list))
    (test-eqv (length new-mask-list) 2)))

(let ((my-maskliststruct (make-MASKLISTSTRUCT 2 #(() ()))))
  (MASKLISTSTRUCT-set-mask_list! my-maskliststruct 0 '(Four Eight))
  (let ((mask-list (MASKLISTSTRUCT-get-mask_list my-maskliststruct)))
    (test-assert (memq 'Four (vector-ref mask-list 0)))
    (test-assert (memq 'Eight (vector-ref mask-list 0)))
    (test-eqv (length (vector-ref mask-list 0)) 2))
  (MASKLISTSTRUCT-set-mask_list! my-maskliststruct 0 '(Eight))
  (test-equal (MASKLISTSTRUCT-get-mask_list my-maskliststruct 0) '(Eight)))

(let ((my-enumliststruct (make-ENUMLISTSTRUCT 1 #(One) 1 #(One))))
  (ENUMLISTSTRUCT-set-altenum_list! my-enumliststruct 0 'Eight)
  (test-eq (ENUMLISTSTRUCT-get-altenum_list my-enumliststruct 0) 'Eight)
  (ENUMLISTSTRUCT-set-altenum_list! my-enumliststruct 0 64)
  (test-eq (ENUMLISTSTRUCT-get-altenum_list my-enumliststruct 0) 64)
  (ENUMLISTSTRUCT-set-enum_list! my-enumliststruct 0 'Four)
  (test-eqv (ENUMLISTSTRUCT-get-enum_list my-enumliststruct 0) 'Four)
  (test-error (ENUMLISTSTRUCT-set-enum_list! my-enumliststruct 0 8)))

(receive (xcb-conn get-xcb-conn-result) (mock-connection #vu8() 
                                                         xcb-events xcb-errors)
  (CreateWindow xcb-conn 
		8 
		(mock-new-xid WINDOW #t) 
		(mock-new-xid WINDOW) 
		0 0 480 320 3 'InputOutput 12 CW `((BitGravity . 5)))
  (test-equal (get-xcb-conn-result)
	      #vu8(1 8 9 0 0 0 0 0 
                     1 0 0 0 0 0 0 0 
                     224 1 64 1 3 0 1 0
                     12 0 0 0 16 0 0 0
                     5 0 0 0)))

(receive (xcb-conn get-xcb-conn-result) (mock-connection #vu8() 
                                                         xcb-events xcb-errors)
  (SetSelectionOwner xcb-conn (mock-new-xid WINDOW #t) (mock-new-xid ATOM) 'CurrentTime)
  (test-equal (get-xcb-conn-result)
	      #vu8(22 0 4 0 0 0 0 0 
                      1 0 0 0 0 0 0 0)))

(receive (xcb-conn get-xcb-conn-result) (mock-connection #vu8()
                                                         xcb-events xcb-errors)
  (let ((my-depth (make-DEPTH 2 1 `#(,(make-VISUALTYPE 3 'PseudoColor 3 3 3 3 3)))))
    (TestStructPack xcb-conn my-depth 400)
    (test-equal (get-xcb-conn-result)
		#vu8(4 0 10 0 2 0 1 0
                       0 0 0 0 3 0 0 0
                       3 3 3 0 3 0 0 0
                       3 0 0 0 3 0 0 0
                       0 0 0 0 144 1 0 0))))

(clone-xcb-struct DEPTH newDEPTH make-newDEPTH newDEPTH? newDEPTH-type)
(let ((my-newDEPTH (make-newDEPTH 2 1 `#(,(make-VISUALTYPE 3 'StaticColor 3 3 3 3 3)))))
  (newDEPTH-set-depth! my-newDEPTH 4)
  (test-eqv (newDEPTH-get-depth my-newDEPTH) 4))

(define (poll-first xcb-conn) (receive (key val) (poll-xcb-connection xcb-conn) (val)))

(receive (xcb-conn get-xcb-conn-result) 
    (mock-connection 
     #vu8(2 2 1 0 2 0 0 0 
            3 0 0 0 4 0 0 0 
            5 0 0 0 1 0 2 0 
            3 0 4 0 7 0 1 0
            0 0 0 0 0 0 0 0)
     xcb-events xcb-errors)
  (let ((my-event (poll-first xcb-conn)))
    (test-eqv (KeyPress-get-detail my-event) 2)
    (test-eq (KeyPress? my-event) #t)))

(receive (xcb-conn get-xcb-conn-result) 
    (mock-connection 
     #vu8(3 2 2 0 2 0 0 0 
            3 0 0 0 4 0 0 0 
            5 0 0 0 1 0 2 0 
            3 0 4 0 7 0 1 0
            0 0 0 0 0 0 0 0)
     xcb-events xcb-errors)
  (let ((my-event (poll-first xcb-conn)))
    (test-eqv (KeyRelease-get-detail my-event) 2)
    (test-eqv (KeyRelease-get-sequence_number my-event) 2)
    (test-eq (KeyRelease? my-event) #t)))

(receive (xcb-conn get-xcb-conn-result) 
    (mock-connection 
     #vu8(4 0 2 0 1 2 3 4 
            5 6 7 8 9 0 1 2 
            3 4 5 6 7 8 9 0 
            0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0)
     xcb-events xcb-errors)
  (let* ((my-event (poll-first xcb-conn))
	 (my-client-message-data (ClientMessageTest-get-data my-event))
	 (data8-vec (xcb-union-get ClientMessageData my-client-message-data 'data8))
	 (data16-vec (xcb-union-get ClientMessageData my-client-message-data 'data16))
	 (data32-vec (xcb-union-get ClientMessageData my-client-message-data 'data32)))
    (test-eqv (typed-value-value (vector-ref data8-vec 2)) 3)
    (test-eqv (typed-value-value (vector-ref data16-vec 2)) 1541)
    (test-eqv (typed-value-value (vector-ref data32-vec 2)) 33619977) 
    (test-eq (ClientMessageTest? my-event) #t)))

(receive (xcb-conn get-xcb-conn-result)
    (mock-connection
     #vu8(0 2 10 0 0 0 4 0
            2 0 0 0 0 0 0 0 
            0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0
            3 2 2 0 2 0 0 0
            3 0 0 0 4 0 0 0
            5 0 0 0 1 0 2 0
            3 0 4 0 7 0 1 0
            0 0 0 0 0 0 0 0)
     xcb-events xcb-errors)
  (let ((my-error (poll-first xcb-conn))
	(my-event (poll-first xcb-conn)))
    (test-eqv (Value-error-get-bad_value my-error) 10)
    (test-eqv (KeyRelease-get-detail my-event) 2)
    (test-eqv (KeyRelease-get-sequence_number my-event) 2)
    (test-eq (KeyRelease? my-event) #t)))

(receive (xcb-conn get-xcb-conn-result) (mock-connection #vu8() 
                                                         xcb-events xcb-errors)
  (QueryTextExtents xcb-conn (mock-new-xid FONT #t) (string->xcb-char2b-vector "12345"))
  (test-equal (get-xcb-conn-result)
	      #vu8(48 1 5 0 0 0 0 0 49 0 50 0 51 0 52 0 53 0 0 0)))

(receive (xcb-conn get-xcb-conn-result) (mock-connection #vu8()
                                                         xcb-events xcb-errors)
  (QueryTextExtents xcb-conn (mock-new-xid FONT #t) (string->xcb-char2b-vector "123456"))
  (test-equal (get-xcb-conn-result)
	      #vu8(48 0 5 0 0 0 0 0 49 0 50 0 51 0 52 0 53 0 54 0)))

(receive (xcb-conn get-xcb-conn-result) 
    (mock-connection #vu8(1 0 1 0 0 0 0 0 
                          1 0 2 0 3 0 4 0 
                          5 0 0 0 6 0 0 0
                          7 0 0 0 0 0 0 0

                          1 1 2 0 0 0 0 0 
                          1 0 2 0 3 0 4 0 
                          5 0 0 0 6 0 0 0
                          7 0 0 0 0 0 0 0)
                     xcb-events xcb-errors)
  (let ((callback-reached? #f))
    (add-hook!
     (QueryTextExtents xcb-conn (mock-new-xid FONT #t) (string->xcb-char2b-vector "Test"))
     (lambda (reply)
       (test-eqv (QueryTextExtents-reply-get-draw_direction reply) 'LeftToRight)
       (set! callback-reached? #t)))
    (poll-first xcb-conn)
    (test-eq callback-reached? #t))
  (let ((callback-reached? #f))
    (add-hook!
     (QueryTextExtents xcb-conn (mock-new-xid FONT #t) (string->xcb-char2b-vector "Test"))
     (lambda (reply)
       (test-eqv (QueryTextExtents-reply-get-draw_direction reply) 'RightToLeft)
       (set! callback-reached? #t)))
    (poll-first xcb-conn)
    (test-eq callback-reached? #t)))

(receive (xcb-conn get-xcb-conn-result)
    (mock-connection #vu8(1 24 1 0 10 0 0 0
                          4  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0
                          1  2 3 4 5 6 7 8
                          9  1 1 2 3 4 5 6
                          7  8 9 1 0 0 0 0
                          0  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0
                          
                          1 24 2 0 10 0 0 0
                          4  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0
                          1  2 3 4 5 6 7 8
                          9  1 1 2 3 4 5 9
                          7  8 9 1 0 0 0 0
                          0  0 0 0 0 0 0 0
                          0  0 0 0 0 0 0 0)
                     xcb-events xcb-errors)
  (let ((callback-reached? #f))
    (add-hook!
     (GetImage xcb-conn 'ZPixmap (mock-new-xid WINDOW #t) 100 100 100 100 128)
     (lambda (reply)
       (test-eqv (GetImage-reply-get-data reply 5) 6)
       (test-eqv (GetImage-reply-get-data reply 15) 6)
       (set! callback-reached? #t)))
   (poll-first xcb-conn)
   (test-eq callback-reached? #t))
  (let ((callback-reached? #f))
    (add-hook!
     (GetImage xcb-conn 'ZPixmap (mock-new-xid WINDOW #t) 100 100 100 100 128)
     (lambda (reply)
       (test-eqv (GetImage-reply-get-data reply 5) 6)
       (test-eqv (GetImage-reply-get-data reply 15) 9)
       (set! callback-reached? #t)))
   (poll-first xcb-conn)
   (test-eq callback-reached? #t)))
(test-end "xcb-test")
