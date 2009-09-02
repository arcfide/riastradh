;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Testing Utility for Scheme
;;;; Scheme48 Restart Kludgery

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This file's existence is very annoying.  It was cribbed from
;;; SLIME48.  Scheme48 has no built-in restart system, so SLIME48
;;; defines its own.  The testing utility wants to use it as well.  But
;;; the testing utility is useful independent of SLIME48, so we can't
;;; just assume the presence of the RESTARTING structure from SLIME48.
;;; And Scheme48 has no conditional configuration mechanism, so the
;;; only way to work around this is to ask folks to load this file if
;;; and only if they are not using SLIME48.

(define-interface restarting-interface
  (export
    make-restarter
    restarter?
    restarter-tag
    restarter-description
    restart
    restart-interactively
    current-restarters
    restarters-in-thread
    with-restarter
    find-restarter
    call-with-restarter
    call-with-interactive-restarter
    with-exiting-restarter
    call-with-exiting-restarter
    ))

(define-structures ((restarting restarting-interface)
                    (restarting-hooks
                     (export with-restarter-invoker-hook)))
  (open scheme
        fluids
        receiving
        define-record-types
        simple-signals
        ;; These two are for the RESTARTERS-IN-THREAD crock.
        (subset fluids-internal (get-dynamic-env set-dynamic-env!))
        (subset threads-internal (thread-dynamic-env))
        )
  (optimize auto-integrate)
  (begin

(define-record-type restarter :restarter
  (%make-restarter tag description invoker interactor)
  restarter?
  (tag restarter-tag)
  (description restarter-description)
  (invoker restarter-invoker)
  (interactor restarter-interactor))

(define (make-restarter tag description invoker interactor)
  (%make-restarter tag description
                   ((fluid $restarter-invoker-hook) invoker)
                   interactor))

(define-record-discloser :restarter
  (lambda (r) (list 'restarter (restarter-tag r))))

;++ Bletch!  This is an outrageous crock.

(define $restarter-invoker-hook (make-fluid values))

(define (with-restarter-invoker-hook hook thunk)
  (let-fluid $restarter-invoker-hook hook
    thunk))

; (put 'with-restarter-invoker-hook 'scheme-indent-function 1)

(define (restart spec . args)
  (let ((win (lambda (r) (apply (restarter-invoker r) args))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (apply restart
                  (error "invalid restarter specifier"
                         `(RESTART ,spec ,@args))
                  args)))))

(define (restart-interactively spec)
  (let ((win (lambda (r)
               (receive args ((restarter-interactor r))
                 (apply (restarter-invoker r) args)))))
    (cond ((restarter? spec)
           (win spec))
          ((find-restarter spec)
           => win)
          (else
           (restart-interactively
            (error "invalid restarter specifier"
                   `(RESTART-INTERACTIVELY ,spec)))))))

(define $restarters (make-fluid '()))

(define (current-restarters) (fluid $restarters))

(define (with-restarter restarter thunk)
  (let-fluid $restarters (cons restarter (fluid $restarters))
    thunk))

; (put 'with-restarter 'scheme-indent-function 1)

;++ This is a crock that does not belong here.

(define (restarters-in-thread thread)
  (let ((saved (get-dynamic-env)))
    (set-dynamic-env! (thread-dynamic-env thread))
    (let ((rs (current-restarters)))
      (set-dynamic-env! saved)
      rs)))

(define (find-restarter tag . restarters)
  (let loop ((restarters (if (pair? restarters)
                             (car restarters)
                             (current-restarters))))
    (cond ((null? restarters)
           #f)
          ((eqv? (restarter-tag (car restarters)) tag)
           (car restarters))
          (else
           (loop (cdr restarters))))))

(define (call-with-restarter tag description invoker receiver)
  (call-with-interactive-restarter tag description invoker #f
    receiver))

(define (call-with-interactive-restarter tag description
            invoker interactor
          receiver)
  (let ((restarter
         (make-restarter tag description invoker interactor)))
    (with-restarter restarter
      (lambda ()
        (receiver restarter)))))

; (put 'call-with-restarter 'scheme-indent-functioon 3)
; (put 'call-with-interactive-restarter 'scheme-indent-function 4)

(define (with-exiting-restarter tag description thunk)
  (call-with-exiting-restarter tag description
    (lambda (r) (thunk))))

(define (call-with-exiting-restarter tag description receiver)
  (call-with-current-continuation
    (lambda (exit)
      (call-with-interactive-restarter
          tag description
          (lambda () (exit))            ; invoker
          (lambda () (values))          ; interactor
        receiver))))

; (put 'with-exiting-restarter 'scheme-indent-function 2)
; (put 'call-with-exiting-restarter 'scheme-indent-function 2)

))
