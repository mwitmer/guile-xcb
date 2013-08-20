 ;; This file is part of Guile XCB.

 ;;    Guile XCB is free software: you can redistribute it and/or modify
 ;;    it under the terms of the GNU General Public License as published by
 ;;    the Free Software Foundation, either version 3 of the License, or
 ;;    (at your option) any later version.

 ;;    Guile XCB is distributed in the hope that it will be useful,
 ;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;;    GNU General Public License for more details.

 ;;    You should have received a copy of the GNU General Public License
 ;;    along with Guile XCB.  If not, see <http://www.gnu.org/licenses/>.

;;; partial xrandr clone: prints display info and lets you change a
;;; monitor's resolutions and offsets or disable it

;;; USAGE: (xrandr {OP args ...} {OP args ...} ...)
;;;
;;; (The curly braces are just to show logical separation between
;;; arguments; they shouldn't be included in the actual function
;;; call.)
;;;
;;; {OP args ...} is one of the following:
;;;
;;; 'print : Print display info a la the xrandr command line tool
;;;
;;; 'rotate output-name ROTATION-LIST: Apply a list of rotations to
;;; the given output
;;;
;;; 'resolution output-name width height : Update the resolution for
;;; monitor OUTPUT-NAME to WIDHTxHEIGHT pixels (must be a valid
;;; resolution).
;;;
;;; 'disable output-name : Disable monitor OUTPUT-NAME
;;;
;;; 'offset output-name x y : Have monitor OUTPUT-NAME show the
;;; contents of the virtual screen starting with the upper-left corner
;;; at coordinates (X, Y).
;;;
;;; Example:
;;;
;;; (xrandr 'resolution "VGA-0" 1680 1050 'offset "DVI-0" 1680 0)
;;; Set the resolution of monitor VGA-0 to 1680x1050 and move DVI-0 to (1680, 0)

(define-module (xcb xml sample randr)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (xcb xml)
  #:use-module (xcb event-loop)
  #:use-module (xcb xml xproto)
  #:use-module (xcb xml ext randr)
  #:export (xrandr))

;; Handling modes

(define ((mode-res= info1) info2)
  (and
   (= (xref info1 'width) (xref info2 'width))
   (= (xref info1 'height) (xref info2 'height))))

(define (mode-refresh mode)
  (define (get n) (xref mode n))
  (define vtotal
    (cond
     ((memq 'double-scane (get 'mode-flags)) (* (get 'vtotal) 2))
     ((memq 'interlace (get 'mode-flags)) (/ (get 'vtotal) 2))
     (else (get 'vtotal))))
  (exact->inexact
   (if (logand (get 'htotal) vtotal)
       (/ (get 'dot-clock) (* (get 'htotal) vtotal))
       0)))

;; Handling rotations

(define all-rotations
  '(rotate-0 rotate-270 rotate-180 rotate-90 reflect-x reflect-y))

(define (sort-rotations rotations)
  (sort rotations
        (lambda (rot1 rot2)
          (> (length (memq rot1 all-rotations))
             (length (memq rot2 all-rotations))))))

(define rotation-names
  '((rotate-0 . "normal")
    (reflect-x . "x axis")
    (reflect-y . "y axis")
    (rotate-90 . "right")
    (rotate-270 . "left")
    (rotate-180 . "inverted")))

;; Make sure the x connection is always cleaned up

(define* xrandr
  (lambda args
    (define xconn (make-parameter #f))
    (dynamic-wind
      (lambda () (xconn (xcb-connect!)))
      (lambda () (with-connection (xconn) (xrandr-inner (xconn) args)))
      (lambda () (xcb-disconnect! (xconn))))))

(define* (xrandr-inner xconn args)
  ;; Basic X connection stuff

  (define setup (xcb-connection-setup xconn))
  (define enable-randr (solicit (delay-enable-extension 'randr)))
  (define randr-version (reply-for query-version 1 4))
  (define screen (xref setup 'roots 0))
  (define root (xref screen 'root))

  ;; Screen resources

  (define screen-resources (reply-for get-screen-resources-current root))

  (define outputs (xref screen-resources 'outputs))
  (define crtcs (xref screen-resources 'crtcs))

  ;; output/crtc/mode info from X server

  (define (mode-pair mode-info) (cons (xref mode-info 'id) mode-info))
  (define (get-infos request xids)
    (define solicit-cdr (lambda (p) (cons (car p) (solicit (cdr p)))))
    (define (make-request xid)
      ;; Avoid performance penalty of out-of-order replies.
      (cons (xid->integer xid) (delay-reply request xid xcb-current-time)))
    (map solicit-cdr (map make-request (vector->list xids))))

  (define screen-info (reply-for get-screen-info root))
  (define output-infos (get-infos get-output-info outputs))
  (define crtc-infos (get-infos get-crtc-info crtcs))
  (define mode-infos
    (map mode-pair
         (vector->list (xref screen-resources 'modes))))

  (define (xid-lookup info-alist xid) (assv-ref info-alist (xid->integer xid)))
  (define (info-lookup info-alist info xid-type)
    (define (swap p) (cons (cdr p) (car p)))
    (define (=>xid n) (make-xid n xid-type))
    (and=> (assq-ref (map swap info-alist) info) =>xid))

  (define change-xids (make-hash-table)) ;For marking x resources to update
  (define (mark-xid! xid) (hashv-set! change-xids (xid->integer xid) #t))

  ;; Organize randr data

  (define (get-current-mode output-info)
    (define crtc-info (xid-lookup crtc-infos (xref output-info 'crtc)))
    (if (get-crtc-info-reply? crtc-info)
        (xid-lookup mode-infos (xref crtc-info 'mode))
        #f))

  (define (get-mode-by-resolution width height output-info)
    (define (match mode)
      (and (= (xref mode 'width) width)
           (= (xref mode 'height) height)
           (memv (xref mode 'id)
                 (map xid->integer
                      (vector->list (xref output-info 'modes))))))
    (find match (map cdr mode-infos)))

  (define (get-output-info-by-name str)
    (define (oi= oi) (string= (xref-string oi 'name) str))
    (find oi= (map cdr output-infos)))

  (define (get-crtc-for-output output-info)
    (define (crtc-lookup xid) (xid-lookup crtc-infos xid))
    (define (first-available-crtc crtcs)
      (define (available? crtc) (= (vector-length (xref crtc 'outputs)) 0))
      (find available? crtcs))
    (or (xid-lookup crtc-infos (xref output-info 'crtc))
        (first-available-crtc
         (map crtc-lookup (vector->list (xref output-info 'crtcs))))))

  (define (get-crtc-xid crtc-info) (info-lookup crtc-infos crtc-info xcrtc))

  ;; Query dimensions

  (define (crtc-dimens ci)
    (define (get p) (xref ci p))
    (if (or (not ci) (bad-crtc-error? ci))
        '((x . 0) (y . 0) (height . 0) (width . 0))
        `((x . ,(get 'x)) (y . ,(get 'y))
          (height . ,(get 'height)) (width . ,(get 'width)))))

  (define (get-output-dimensions)
    (define (get-crtc-info oi) (xid-lookup crtc-infos (xref oi 'crtc)))
    (map (lambda (oi) (crtc-dimens (get-crtc-info oi))) (map cdr output-infos)))

  (define (get-screen-size dimens)
    (let current ((width 0) (height 0) (dimens dimens))
      (if (null? dimens) (cons width height)
          (let ((dimen (car dimens)))
            (current
             (max (+ (assoc-ref dimen 'width) (assoc-ref dimen 'x)) width)
             (max (+ (assoc-ref dimen 'height) (assoc-ref dimen 'y)) height)
             (cdr dimens))))))

  (define original-screen-dimensions (get-screen-size (get-output-dimensions)))
  (define (dimensions-too-small? dimensions)
    (or (< (car dimensions) (car original-screen-dimensions))
        (< (cdr dimensions) (cdr original-screen-dimensions))))

  ;; Update X resources

  (define (crtc-modify! ci-entry transform)
    (define result (transform (cdr ci-entry) (make-xid (car ci-entry) xcrtc)))
    (if (set-crtc-config-reply? result)
        (case (xref result 'status)
          ((success) result)
          ((invalid-time) (error "Invalid time provided in call \
to SetCrtcConfig"))
          ((failed) (error "Call to SetCrtcConfig failed")))
        result))

  (define (disable-crtc! ci-entry)
    (define (disable ci xid)
      (reply-for set-crtc-config
                 xid xcb-current-time (xref screen-resources 'timestamp)
                 (xref ci 'x) (xref ci 'y) (xcb-none xmode)
                 (xref ci 'rotation) #()))
    (crtc-modify! ci-entry disable))

  (define (update!)
    (define ((if-changed proc) entry)
      (if (hashv-ref change-xids (car entry)) (proc entry)))
    (define (update-screen-size! dimens)
      (set-screen-size root (car dimens) (cdr dimens)
                         (xref screen 'width-in-millimeters)
                         (xref screen 'height-in-millimeters)))
    (define (update-crtc! ci-entry)
      (define (update ci xid)
        (reply-for set-crtc-config
                   xid
                   (xref ci 'timestamp) (xref screen-resources 'timestamp)
                   (xref ci 'x) (xref ci 'y) (xref ci 'mode)
                   (xref ci 'rotation) (xref ci 'outputs)))
      (define new-screen-size (get-screen-size (get-output-dimensions)))
      (if (dimensions-too-small? new-screen-size) (disable-crtc! ci-entry))
      (update-screen-size! new-screen-size)
      (crtc-modify! ci-entry update))
    (grab-server)
    (for-each (if-changed update-crtc!) crtc-infos)
    (ungrab-server))

  ;; Format output

  (define (format-screen-info)
    (define screen-sizes (xref screen-info 'sizes))
    (define size-range (reply-for get-screen-size-range root))
    (define current-size (get-screen-size (get-output-dimensions)))

    (let ((get (lambda (n) (xref size-range n))))
      (format #f "Screen ~a: minimum ~a x ~a, current ~a x ~a, maximum ~a x ~a"
              (string-take-right (xcb-connection-display xconn) 1)
              (get 'min-width) (get 'min-height)
              (car current-size) (cdr current-size)
              (get 'max-width) (get 'max-height))))

  (define (format-connected-output-info output-info)
    (define output-name (xref-string output-info 'name))
    (define crtc-info (get-crtc-for-output output-info))
    (define rotations
      (map (lambda (rotation) (assq-ref rotation-names rotation))
           (sort-rotations (xref crtc-info 'rotations))))
    (format #f "~a connected ~ax~a+~a+~a ~a ~amm x ~amm"
            output-name
            (xref crtc-info 'width) (xref crtc-info 'height)
            (xref crtc-info 'x) (xref crtc-info 'y)
            rotations
            (xref output-info 'mm-width) (xref output-info 'mm-height)))

  (define (format-disconnected-output-info output-info)
    (define output-name (xref-string output-info 'name))
    (define (get-rotation-name rotation) (assq-ref rotation-names rotation))
    (format #f "~a disconnected ~a"
            output-name (map get-rotation-name all-rotations)))

  ;; Basic operations

  (define (print)
    (define (print-output-info output-info)
      (define current-mode (get-current-mode output-info))
      (define mode-xids (xref output-info 'modes))
      (define (mode-match xid) (xid-lookup mode-infos xid))
      (define (preferred-mode? mode-info)
        (define num-preferred (xref output-info 'num-preferred))
        (let get-preferred ((i 0))
          (cond
           ((= i num-preferred) #f)
           ((= (xid->integer (vector-ref mode-xids i)) (xref mode-info 'id)) #t)
           (else (get-preferred (+ i 1))))))
      (define (print-modes mode-infos)
        (define info (car mode-infos))
        (define (print-refresh info)
          (format #t " ~6,1f~a~a" (mode-refresh info)
                  (if (eq? info current-mode) "*" " ")
                  (if (preferred-mode? info) "+" " ")))
        (receive (infos rest) (partition (mode-res= info) mode-infos)
          (format #t "   ~12a"
                  (format #f "~ax~a" (xref info 'width) (xref info 'height)))
          (for-each print-refresh infos) (newline)
          (if (not (null? rest)) (print-modes rest))))
      (format #t "~a\n"
              (case (xref output-info 'connection)
                (() (format-connected-output-info output-info))
                (else (format-disconnected-output-info output-info))))
      (if (> (vector-length mode-xids) 0)
          (print-modes (map mode-match (vector->list mode-xids)))))
    (format #t "~a\n" (format-screen-info))
    (for-each print-output-info (map cdr output-infos)))

  (define (call-if-crtc-present output-name proc)
    (define (with-output output-info)
      ((and=> (get-crtc-for-output output-info) proc) output-info))
    (and=> (get-output-info-by-name output-name) with-output))

  (define (rotate output-name rotation)
    (define ((do-it crtc-info) display-info)
      (xset! crtc-info 'rotation rotation)
      (mark-xid! (get-crtc-xid crtc-info)))
    (call-if-crtc-present output-name do-it))

  (define (disable output-name)
    (define ((do-it crtc-info) display-info)
      (xset! crtc-info 'mode (xcb-none xmode))
      (xset! crtc-info 'outputs #())
      (mark-xid! (get-crtc-xid crtc-info)))
    (call-if-crtc-present output-name do-it))

  (define (offset output-name x y)
    (define ((do-it crtc-info) output-info)
      (define crtc-xid (info-lookup crtc-infos crtc-info xcrtc))
      (xset! crtc-info 'x x)
      (xset! crtc-info 'y y)
      (mark-xid! (get-crtc-xid crtc-info)))
    (call-if-crtc-present output-name do-it))

  (define (resolution output-name width height)
    (define ((do-it crtc-info) output-info)
      (define output-xid (info-lookup output-infos output-info xoutput))
      (define new-mode (get-mode-by-resolution width height output-info))
      (xset! crtc-info 'mode (make-xid (xref new-mode 'id) xmode))
      (xset! crtc-info 'height (xref new-mode 'height))
      (xset! crtc-info 'width (xref new-mode 'width))
      (if (= (vector-length (xref crtc-info 'outputs)) 0)
          (xset! crtc-info 'outputs (vector output-xid)))
      (mark-xid! (get-crtc-xid crtc-info)))
    (call-if-crtc-present output-name do-it))

  (define (process args)
    (define op
      (case (car args)
        ((print) (cons print 0))
        ((rotate) (cons rotate 2))
        ((resolution) (cons resolution 3))
        ((offset) (cons offset 3))
        ((disable) (cons disable 1))
        (else (error "Malformed xrandr args" args))))
    (apply (car op) (take (cdr args) (cdr op)))
    (drop (cdr args) (cdr op)))

  (let eat-args ((args args)) (if (not (null? args)) (eat-args (process args))))
  (update!)
  *unspecified*)
