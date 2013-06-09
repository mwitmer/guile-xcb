(define-module (xcb xml switch-test)
  #:use-module (srfi srfi-64)
  #:use-module (sxml simple)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (xcb xml struct)
  #:use-module (xcb xml records)
  #:use-module (system base compile)
  #:use-module (xcb xml type)
  #:use-module (xcb xml enum)
  #:use-module (ice-9 pretty-print)
  #:use-module (language scheme spec)
  #:use-module (language xml-xcb spec))

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

(define-public test-xml "<test-root><xcb header=\"xkb\"></xcb>
	<enum name=\"NKNDetail\">
		<item name=\"Keycodes\"> <bit>0</bit> </item>
		<item name=\"Geometry\"> <bit>1</bit> </item>
		<item name=\"DeviceID\"> <bit>2</bit> </item>
	</enum>

	<enum name=\"StatePart\">
		<item name=\"ModifierState\">     <bit>0</bit> </item>
		<item name=\"ModifierBase\">      <bit>1</bit> </item>
		<item name=\"ModifierLatch\">     <bit>2</bit> </item>
		<item name=\"ModifierLock\">      <bit>3</bit> </item>
		<item name=\"GroupState\">        <bit>4</bit> </item>
		<item name=\"GroupBase\">         <bit>5</bit> </item>
		<item name=\"GroupLatch\">        <bit>6</bit> </item>
		<item name=\"GroupLock\">         <bit>7</bit> </item>
		<item name=\"CompatState\">       <bit>8</bit> </item>
		<item name=\"GrabMods\">          <bit>9</bit> </item>
		<item name=\"CompatGrabMods\">    <bit>10</bit> </item>
		<item name=\"LookupMods\">        <bit>11</bit> </item>
		<item name=\"CompatLookupMods\">  <bit>12</bit> </item>
		<item name=\"PointerButtons\">    <bit>13</bit> </item>
	</enum>

	<enum name=\"Control\" >
		<item name=\"GroupsWrap\">      <bit>27</bit> </item>
		<item name=\"InternalMods\">    <bit>28</bit> </item>
		<item name=\"IgnoreLockMods\">  <bit>29</bit> </item>
		<item name=\"PerKeyRepeat\">    <bit>30</bit> </item>
		<item name=\"ControlsEnabled\"> <bit>31</bit> </item>
	</enum>

	<enum name=\"NameDetail\">
		<item name=\"Keycodes\">        <bit>0</bit> </item>
		<item name=\"Geometry\">        <bit>1</bit> </item>
		<item name=\"Symbols\">         <bit>2</bit> </item>
		<item name=\"PhysSymbols\">     <bit>3</bit> </item>
		<item name=\"Types\">           <bit>4</bit> </item>
		<item name=\"Compat\">          <bit>5</bit> </item>
		<item name=\"KeyTypeNames\">    <bit>6</bit> </item>
		<item name=\"KTLevelNames\">    <bit>7</bit> </item>
		<item name=\"IndicatorNames\">  <bit>8</bit> </item>
		<item name=\"KeyNames\">        <bit>9</bit> </item>
		<item name=\"KeyAliases\">      <bit>10</bit> </item>
		<item name=\"VirtualModNames\"> <bit>11</bit> </item>
		<item name=\"GroupNames\">      <bit>12</bit> </item>
		<item name=\"RGNames\">         <bit>13</bit> </item>
	</enum>

	<enum name=\"CMDetail\">
		<item name=\"SymInterp\">   <bit>0</bit> </item>
		<item name=\"GroupCompat\"> <bit>1</bit> </item>
	</enum>

	<enum name=\"AXNDetail\">
		<item name=\"SKPress\">      <bit>0</bit> </item>
		<item name=\"SKAccept\">     <bit>1</bit> </item>
		<item name=\"SKReject\">     <bit>2</bit> </item>
		<item name=\"SKRelease\">    <bit>3</bit> </item>
		<item name=\"BKAccept\">     <bit>4</bit> </item>
		<item name=\"BKReject\">     <bit>5</bit> </item>
		<item name=\"AXKWarning\">   <bit>6</bit> </item>
	</enum>

	<enum name=\"XIFeature\">
		<item name=\"Keyboards\">      <bit>0</bit> </item>
		<item name=\"ButtonActions\">  <bit>1</bit> </item>
		<item name=\"IndicatorNames\"> <bit>2</bit> </item>
		<item name=\"IndicatorMaps\">  <bit>3</bit> </item>
		<item name=\"IndicatorState\"> <bit>4</bit> </item>
	</enum>

	<typedef oldname=\"CARD16\" newname=\"DeviceSpec\" />

	<enum name=\"EventType\">
		<item name=\"NewKeyboardNotify\">      <bit>0</bit> </item>
		<item name=\"MapNotify\">              <bit>1</bit> </item>
		<item name=\"StateNotify\">            <bit>2</bit> </item>
		<item name=\"ControlsNotify\">         <bit>3</bit> </item>
		<item name=\"IndicatorStateNotify\">   <bit>4</bit> </item>
		<item name=\"IndicatorMapNotify\">     <bit>5</bit> </item>
		<item name=\"NamesNotify\">            <bit>6</bit> </item>
		<item name=\"CompatMapNotify\">        <bit>7</bit> </item>
		<item name=\"BellNotify\">             <bit>8</bit> </item>
		<item name=\"ActionMessage\">          <bit>9</bit> </item>
		<item name=\"AccessXNotify\">          <bit>10</bit> </item>
		<item name=\"ExtensionDeviceNotify\">  <bit>11</bit> </item>
	</enum>

	<enum name=\"MapPart\">
		<item name=\"KeyTypes\">            <bit>0</bit> </item>
		<item name=\"KeySyms\">             <bit>1</bit> </item>
		<item name=\"ModifierMap\">         <bit>2</bit> </item>
		<item name=\"ExplicitComponents\">  <bit>3</bit> </item>
		<item name=\"KeyActions\">          <bit>4</bit> </item>
		<item name=\"KeyBehaviors\">        <bit>5</bit> </item>
		<item name=\"VirtualMods\">         <bit>6</bit> </item>
		<item name=\"VirtualModMap\">       <bit>7</bit> </item>
	</enum>

        <request name=\"SwitchInReply\" opcode=\"2\">
                <field name=\"thing1\" type=\"CARD32\" />
                <field name=\"thing2\" type=\"CARD32\" />
                <reply>
                        <field name=\"mask\" type=\"CARD8\" mask=\"MapPart\" />
                        <switch name=\"details\">
                                <fieldref>mask</fieldref>
                                <bitcase>
                                        <enumref ref=\"MapPart\">KeyTypes</enumref>
                                        <field name=\"list-length\" type=\"CARD16\" />
                                        <pad bytes=\"4\" />
                                        <list name=\"my-list\" type=\"CARD16\">
                                                <fieldref>list-length</fieldref>
                                        </list>
                                </bitcase>
                                <bitcase>
                                        <enumref ref=\"MapPart\">KeySyms</enumref>
                                        <field name=\"bob\" type=\"CARD8\" />
                                        <pad bytes=\"3\" />
                                </bitcase>
                                <field name=\"switch-default\" type=\"CARD32\" />
                        </switch>
                </reply>
        </request>

	<request name=\"SelectEvents\" opcode=\"1\">
		<field name=\"deviceSpec\" type=\"DeviceSpec\" />
		<field name=\"affectWhich\" type=\"CARD16\" mask=\"EventType\" />
		<field name=\"clear\" type=\"CARD16\" mask=\"EventType\" />
		<field name=\"selectAll\" type=\"CARD16\" mask=\"EventType\" />
		<field name=\"affectMap\" type=\"CARD16\" mask=\"MapPart\" />
		<field name=\"map\" type=\"CARD16\" mask=\"MapPart\" />
		<switch name=\"details\">
			<op op=\"&amp;\">
				<fieldref>affectWhich</fieldref>
				<op op=\"&amp;\">
					<unop op=\"~\"><fieldref>clear</fieldref></unop>
					<unop op=\"~\"><fieldref>selectAll</fieldref></unop>
				</op>
			</op>
			<bitcase>
				<enumref ref=\"EventType\">NewKeyboardNotify</enumref>
				<field name=\"affectNewKeyboard\" type=\"CARD16\" mask=\"NKNDetail\" />
				<field name=\"newKeyboardDetails\" type=\"CARD16\" mask=\"NKNDetail\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">StateNotify</enumref>
				<field name=\"affectState\" type=\"CARD16\" mask=\"StatePart\" />
				<field name=\"stateDetails\" type=\"CARD16\" mask=\"StatePart\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">ControlsNotify</enumref>
				<field name=\"affectCtrls\" type=\"CARD32\" mask=\"Control\" />
				<field name=\"ctrlDetails\" type=\"CARD32\" mask=\"Control\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">IndicatorStateNotify</enumref>
				<field name=\"affectIndicatorState\" type=\"CARD32\" />
				<field name=\"indicatorStateDetails\" type=\"CARD32\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">IndicatorMapNotify</enumref>
				<field name=\"affectIndicatorMap\" type=\"CARD32\" />
				<field name=\"indicatorMapDetails\" type=\"CARD32\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">NamesNotify</enumref>
				<field name=\"affectNames\" type=\"CARD16\" mask=\"NameDetail\" />
				<field name=\"namesDetails\" type=\"CARD16\" mask=\"NameDetail\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">CompatMapNotify</enumref>
				<field name=\"affectCompat\" type=\"CARD8\" mask=\"CMDetail\" />
				<field name=\"compatDetails\" type=\"CARD8\" mask=\"CMDetail\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">BellNotify</enumref>
				<field name=\"affectBell\" type=\"CARD8\" />
				<field name=\"bellDetails\" type=\"CARD8\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">ActionMessage</enumref>
				<field name=\"affectMsgDetails\" type=\"CARD8\" />
				<field name=\"msgDetails\" type=\"CARD8\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">AccessXNotify</enumref>
				<field name=\"affectAccessX\" type=\"CARD16\" mask=\"AXNDetail\" />
				<field name=\"accessXDetails\" type=\"CARD16\" mask=\"AXNDetail\" />
			</bitcase>
			<bitcase>
				<enumref ref=\"EventType\">ExtensionDeviceNotify</enumref>
				<field name=\"affectExtDev\" type=\"CARD16\" mask=\"XIFeature\" />
				<field name=\"extdevDetails\" type=\"CARD16\" mask=\"XIFeature\" />
			</bitcase>
                        <field name=\"default\" type=\"CARD16\" />
		</switch>
	</request>

</test-root>")

(define no-new-xcb-module? #t)

(define-public (print-scheme)
  (set! no-new-xcb-module? #f)
  (for-each
   (lambda (sxml)
     (pretty-print (compile sxml #:from xml-xcb #:env (current-module) #:to scheme)))
   (cdadr (test-reader test-xml)))
  (set! no-new-xcb-module? #t))

(map 
 (lambda (sxml)
   (compile sxml #:from xml-xcb #:env (current-module)))
 (cdadr (test-reader test-xml)))

(set-extension-opcode! 100)

(test-begin "xcb-switch-test")

(receive (conn get-bytevector) (mock-connection #vu8() (make-hash-table) (make-hash-table))
  (SelectEvents conn 12 
                '(NewKeyboardNotify)  
                '(StateNotify) 
                '(StateNotify) 
                '(KeyActions)
                '(KeyActions) 
                '((affectNewKeyboard . (Keycodes)) 
                  (newKeyboardDetails . (Geometry))))
  (test-equal
   (get-bytevector)
   #vu8(100 1 5 0 12 0 1 0 4 0 4 0 16 0 16 0 1 0 2 0)))

(receive (conn get-bytevector) (mock-connection #vu8() (make-hash-table) (make-hash-table))
  (SelectEvents conn 12 
                '(NewKeyboardNotify ControlsNotify)  
                '(StateNotify) 
                '(StateNotify) 
                '(KeyActions)
                '(KeyActions) 
                '((affectNewKeyboard . (Keycodes)) 
                  (newKeyboardDetails . (Geometry))
                  (affectCtrls . (GroupsWrap))
                  (ctrlDetails . (GroupsWrap))))
  
  (test-equal
   (get-bytevector)
   #vu8(100 1 7 0 12 0 9 0 4 0 4 0 16 0 16 0
            1 0 2 0 0 0 0 8 0 0 0 8)))

(receive (conn get-bytevector) (mock-connection #vu8(1 1 1 0 1 0 0 0
                                                     4 0 0 0 0 0 1 0 2 0 3 0
                                                     4 0 0 0 0 0 0 0
                                                     0 0 0 0 0 0 0 0)
                                                (make-hash-table) (make-hash-table))
  (add-hook!
   (SwitchInReply conn 12 24)
   (lambda (reply) 
     (define switch-values (xcb-switch-values SwitchInReply-reply reply))
     (test-eq (assq-ref switch-values 'list-length) 4)
     (test-equal (assq-ref switch-values 'my-list) #(1 2 3 4))))
  (poll-xcb-connection conn))

(receive (conn get-bytevector) (mock-connection #vu8(1 3 1 0 1 0 0 0
                                                     4 0 0 0 0 0 2 0 4 0 6 0
                                                     8 0 12 0 0 0 0 0
                                                     0 0 0 0 0 0 0 0)
                                                (make-hash-table) (make-hash-table))
  (add-hook!
   (SwitchInReply conn 12 24)
   (lambda (reply) 
     (define switch-values (xcb-switch-values SwitchInReply-reply reply))
     (test-eq (assq-ref switch-values 'list-length) 4)
     (test-equal (assq-ref switch-values 'bob) 12)
     (test-equal (assq-ref switch-values 'my-list) #(2 4 6 8))))
  (poll-xcb-connection conn))

(receive (conn get-bytevector) (mock-connection #vu8(1 0 1 0 0 0 0 0
                                                     4 0 0 0 0 0 0 0
                                                     0 0 0 0 0 0 0 0
                                                     0 0 0 0 0 0 0 0)
                                                (make-hash-table) (make-hash-table))
  (add-hook!
   (SwitchInReply conn 12 24)
   (lambda (reply) 
     (define switch-values (xcb-switch-values SwitchInReply-reply reply))
     (test-equal (assq-ref switch-values 'switch-default) 4)))
  (poll-xcb-connection conn))

(test-end "xcb-switch-test")
