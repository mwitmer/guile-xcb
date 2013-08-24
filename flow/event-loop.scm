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

;;; Commentary:

;; These procedures provide a basic framework for running an event
;; loop. Once inside the dynamic extent of a call to 'do-event-loop',
;; the program is effectively transformed into a cooperative
;; multithreaded environment. 'solicit' and 'notify' are provided to
;; switch control to and from an event dispatcher as needed.

(define-module (flow event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 q)
  #:export (solicit))

(define-public (make-tag data)
  "Create a unique object based on DATA to use as a tag for calls to
'solicit'/'notify'."
  (list data))

(define notify-param (make-parameter #f))
(define solicit-param (make-parameter #f))
(define post-param (make-parameter #f))

(define-public (notify tag . vals)
  "Notify the event loop of the presence of a values VALS associated
with object TAG. If any continuation or procedure is pending on the
event loop for TAG as a result from a call to 'solicit', it will be
called once and removed from the event loop. Otherwise, VALS will be
queued for use in future calls to 'solicit'."
  (if (notify-param)
      (apply (notify-param) tag vals)
      (error "event-loop: Call to notify outside an event-loop")))

(define* (solicit tag #:optional (proc *unspecified*))
  "Place a procedure on the event loop to run when some call to
'notify' provides a value for object TAG. If PROC is provided,
'solicit' returns immediately with an unspecified value. Otherwise,
control flow is yielded to the event loop until 'notify' is called for
TAG, at which point this procedure returns the given value(s) and
control flow returns to its continuation ."
  (if (solicit-param)
      ((solicit-param) tag proc)
      (error "event-loop: Call to solicit outside an event-loop")))

(define-public (post-to-event-loop thunk)
  "Queues THUNK to be executed the next time control is yielded to the
event loop."
  (if (post-param)
      ((post-param) thunk)
      (error "event-loop: Call to post-to-event-loop outside an event-loop")))

(define error-tag (make-tag 'error))
(define tick-tag (make-tag 'tick))

(define-public (event-loop-tick)
  "Yields control flow to the event loop for only a single call to the
event dispatcher. Returns the result of that call."
  (solicit tick-tag))

(define-public (event-loop-error . args)
  "Signals an error to be handled by the event loop's installed error
handler."
  (apply notify error-tag args))

(define-public (unsolicit tag)
  "Removes the pending procedure or continuation on the event loop for
TAG if one is present."
  (solicit tag #f))

(define-public (do-event-loop dispatcher proc on-error)
  "Runs PROC inside of an event loop. DISPATCHER is a non-blocking
thunk that reads and handles events.

See 'solicit' and 'notify' for an explanation on how to transfer
control flow between PROC and DISPATCHER.

ON-ERROR is a function that will be called when 'event-loop-error' is
called. For its arguments, it takes RESUME, a function that returns to
the event loop, and then all the values that were passed to
'event-loop-error'."
  (define conts (make-weak-key-hash-table))
  (define early (make-weak-key-hash-table))
  (define-syntax-rule (dispatch expr) (% expr run-dispatch))
  (define-syntax-rule (error-prompt expr) (% error-tag expr handle-error))
  (define post-q (make-q))
  (define (notify-proc tag . vals)
    (if (eq? tag error-tag) (apply abort-to-prompt error-tag vals)
        (let ((cont (hashq-ref conts tag)))
          (hashq-remove! conts tag)
          (if cont (apply cont vals)
              (let ((earlies (or (hashq-ref early tag)
                                 (hashq-set! early tag (make-q)))))
                (enq! earlies vals))))))
  (define (solicit-proc key proc)
    (define (on-miss)
      (if (unspecified? proc) (abort key) (hashq-set! conts key proc)))
    (define (on-hit early-vals)
      (define vals (deq! early-vals))
      (if (q-empty? early-vals) (hashq-remove! early key))
      (if (unspecified? proc) (apply values vals) (apply proc vals)))
    (cond
     ((not proc) (hashq-remove! conts key))
     ((hashq-ref early key) => on-hit)
     (else (on-miss))))
  (define (post-proc thunk) (q-push! post-q thunk))
  (define (run-dispatch cont key)
    (hashq-set! conts key cont)
    (let get-result ((result (dispatch (dispatcher))))
      (while (not (q-empty? post-q)) (dispatch ((q-pop! post-q))))
      (cond
       ((eq? key tick-tag) (dispatch (notify tick-tag result)))
       ((eq? cont (hashq-ref conts key)) (get-result (dispatch (dispatcher))))
       (else result))))
  (define (handle-error cont . args)
    (error-prompt (apply on-error cont args)))
  (error-prompt (parameterize ((notify-param notify-proc)
                               (solicit-param solicit-proc)
                               (post-param post-proc))
                  (dispatch (proc)))))

(define-public (notify-map reply-tags)
  "Solicits all tags in the list REPLY-TAGS and returns a single tag
that will be notified when all of the other tags have been notified."
  (define not-ready (make-tag 'not-ready))
  (define replies
    (map (lambda (reply-tag) (cons reply-tag (make-parameter not-ready)))
         reply-tags))
  (define notify-tag (make-tag `(notify-map ,@reply-tags)))
  (define (update tag val)
    (define (eval-param param) (apply param '()))
    (define reply-param (assq-ref replies tag))
    (reply-param val)
    (let ((results (map-in-order eval-param (map-in-order cdr replies))))
      (if (not (memq not-ready results)) (notify notify-tag results))))
  (define (solicit-reply reply-tag)
    (solicit reply-tag (lambda (reply) (update reply-tag reply))))
  (for-each solicit-reply reply-tags)
  notify-tag)
