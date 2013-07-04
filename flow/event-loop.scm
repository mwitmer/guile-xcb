(define-module (flow event-loop)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-9)
  #:export (with-notifies do-event-loop))

(define-public (make-tag data) (list data))
(define-public notify-param (make-parameter #f))
(define-public (notify tag val) 
  (if (notify-param)
      ((notify-param) tag val)
      (error "event-loop: Call to notify outside an event-loop")))
(define-public default-error-tag (make-parameter '(error)))
(define (default-loop-proc) (abort (make-tag 'forever)))

(define* (do-event-loop dispatcher finished? 
                        #:optional (proc default-loop-proc) 
                        #:key after on-error)
  (define (run)
    (define conts (make-hash-table))
    (define early (make-hash-table))
    (define done? (make-parameter #f))
    (define finish-tag (make-tag 'finished))
    (define (notify-proc tag val)
      (cond 
       ((and on-error (eq? tag (default-error-tag))) 
        (abort-to-prompt (default-error-tag) val))
       ((eq? tag (default-error-tag))
        (error "event-loop: Throw to nonexistant error handler")))
      (let ((cont (hashq-ref conts tag)))
        (hashq-remove! conts tag)
        (if cont (cont val) (hashq-set! early tag val))))
    (define* (loop cont key #:optional proc)
      (define early-val (hashq-ref early key))
      (define (on-miss)
        (define (dispatch-loop) 
          (let inner-loop ()
            (define result (dispatcher))
            (if (or (done?) (finished?)) result (inner-loop))))
        (hashq-set! conts key (or proc cont))
        (if proc (cont #t) (dispatch-loop)))
      (define (use-early-val) 
        (hashq-remove! early key) 
        ((or proc cont) early-val))
      (if early-val (% (use-early-val) loop) (% (on-miss) loop)))
    (parameterize ((notify-param notify-proc))
      (define final (% (let ((result (proc))) (done? #t) result) loop))
      (if (and (finished?) after) (after))
      final))
  (if on-error (call-with-prompt (default-error-tag) run on-error) (run)))

(define-syntax with-notifies
  (syntax-rules ()
    ((_ listen! ((reply arg ...) ...) stmt stmt* ...)
     ((lambda () 
        (define not-ready '(not-ready))
        (define replies (list (cons 'reply (make-parameter not-ready)) ...))
        (define notify-tag '(with-notifies (reply <- proc) ...))
        (define (update tag val)
          (define eval-param (lambda (param) (apply param '())))
          (define reply-param (assq-ref replies tag))
          (reply-param val)
          (let  ((results (map-in-order eval-param (map-in-order cdr replies))))
            (if (not (memq not-ready results))
                (notify notify-tag
                        (apply (lambda (reply ...) stmt stmt* ...) results)))))
        (listen! update 'reply arg ...) ... 
        notify-tag)))))
