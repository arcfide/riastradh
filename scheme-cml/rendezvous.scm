;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Rendezvous

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Reppy uses the term `event' for what we call a `rendezvous'.  An
;;; event is something that happens once.  A rendezvous is a more
;;; useful entity that is, at any given time, in one of two states:
;;; enabled or blocked.  If it is enabled, it has an associated value.
;;; If it is blocked, then a process can wait for it to become enabled
;;; with SYNCHRONIZE.

(define-record-type <rendezvous>
    (make-rendezvous generator)
    rendezvous?
  (generator rendezvous.generator))

(define (prv->rendezvous prv)
  (make-rendezvous
   (let ((prv*
          (map-prv prv
            (lambda (thunk)
              (lambda (condvars)
                (values condvars thunk))))))
     (lambda ()
       (values '() (cons prv* '()))))))

(define (base-rendezvous poller enabler blocker)
  (prv->rendezvous
   (make-prv poller enabler blocker (lambda (thunk) thunk))))

(define (choosing-rendezvous* choices)
  (reduce-right binary-choosing-rendezvous (quiescent-rendezvous) choices))

(define (binary-choosing-rendezvous left right)
  (make-rendezvous
   (let ((left-generator (rendezvous.generator left))
         (right-generator (rendezvous.generator right)))
     (lambda ()
       (receive (left-condvars left-prvs) (left-generator)
         (receive (right-condvars right-prvs) (right-generator)
           (define (mappify these-prvs those-condvars)
             (map (lambda (this-prv)
                    (map-prv this-prv
                      (lambda (constructor)
                        (lambda (condvars)
                          (constructor (append those-condvars condvars))))))
                  these-prvs))
           (values (append left-condvars right-condvars)
                   (append (mappify left-prvs right-condvars)
                           (mappify right-prvs left-condvars)))))))))

(define (delayed-rendezvous generator)
  (make-rendezvous
   (lambda ()
     ((rendezvous.generator (generator))))))

(define (delayed-rendezvous-with-nack generator)
  (make-rendezvous
   (lambda ()
     (let ((condvar (make-condvar)))
       (receive (condvars prvs)
                ((rendezvous.generator
                  (generator (prv->rendezvous (condvar-prv condvar)))))
         (values (cons condvar condvars) prvs))))))

;;;; Synchronize

(define (force-rendezvous rendezvous)
  (receive (condvars prvs) ((rendezvous.generator rendezvous))
    condvars                            ;ignore
    prvs))

(define (synchronize rendezvous)
  (receive (condvars thunk)
           ((primitive-synchronize (force-rendezvous rendezvous)) '())
    (for-each set-condvar! condvars)
    (thunk)))

(define (poll rendezvous if-enabled if-blocked)
  (primitive-poll (force-rendezvous rendezvous)
    (lambda (constructor)
      (receive (condvars thunk) (constructor '())
        (for-each set-condvar! condvars)
        (call-with-values thunk if-enabled)))
    if-blocked))

;;; These are so verbose I can't imagine anyone would ever use them,
;;; particularly in lieu of RENDEZVOUS-CASE.  What was I thinking?

(define (synchronize/timeout rendezvous duration if-timed-out if-enabled)
  (synchronize-chosen-rendezvous
   (map-rendezvous rendezvous if-enabled)
   (map-rendezvous (after-time-rendezvous duration) if-timed-out)))

(define (synchronize-chosen-rendezvous . choices)
  (synchronize (choosing-rendezvous* choices)))

(define (synchronize-chosen-rendezvous/timeout duration
                                               if-timed-out
                                               if-enabled
                                               . choices)
  (synchronize-chosen-rendezvous
   (map-rendezvous (choosing-rendezvous* choices) if-enabled)
   (map-rendezvous (after-time-rendezvous duration) if-timed-out)))

;;;; Miscellaneous Rendezvous

(define (map-rendezvous rendezvous procedure)
  (make-rendezvous
   (let ((generator (rendezvous.generator rendezvous)))
     (lambda ()
       (receive (condvars prvs) (generator)
         (values condvars
                 (map (lambda (prv)
                        (map-prv prv
                          (lambda (constructor)
                            (lambda (condvars)
                              (receive (condvars* thunk) (constructor condvars)
                                (values condvars*
                                        (compose-nullary procedure thunk)))))))
                      prvs)))))))

(define (choosing-rendezvous . choices)
  (choosing-rendezvous* choices))

(define quiescent-rendezvous
  (let ((rendezvous (make-rendezvous (lambda () (values '() '())))))
    (lambda ()
      rendezvous)))

(define (values-rendezvous . the-values)
  (base-rendezvous
   (lambda () -1)                       ;poll: default priority
   (lambda (if-enabled if-disabled)     ;enable
     if-disabled ;ignore
     (if-enabled (lambda () (apply values the-values))))
   (lambda (suspension if-enabled if-blocked) ;block
     suspension if-enabled if-blocked ;ignore
     (error "VALUES rendezvous should not be blocked!" the-values))))

(define (thunk-rendezvous thunk)
  (base-rendezvous
   (lambda () -1)                       ;poll: default priority
   (lambda (if-enabled if-disabled)     ;enable
     if-disabled ;ignore
     (if-enabled thunk))
   (lambda (suspension if-enabled if-blocked) ;block
     suspension if-enabled if-blocked ;ignore
     (error "Thunk rendezvous should not be blocked!" thunk))))

;;;; `Condition Variables'

;;; These are not actually condition variables in the usual sense of
;;; the term, but this is the unfortunate term that Reppy &c. chose for
;;; the mechanism of signalling negative acknowledgements.

;;; To reduce the amount of code, we could replace condvars altogether
;;; by placeholders, which are currently implemented separately.

(define-locked-record-type <condvar>
    (%make-condvar priority waiters)
    (priority waiters)
    condvar?
    with-condvar-locked
  (priority condvar.priority set-condvar.priority!)
  (waiters condvar.waiters set-condvar.waiters!))

(define (make-condvar)
  (%make-condvar #f '()))

(define (set-condvar! condvar)
  (enter-critical-section
    (lambda (critical-token)
      ;** Do not beta-reduce -- bug in Scheme48's auto-integrator.
      (let ((continuation
             ((with-condvar-locked condvar
                (lambda ()
                  (if (condvar.priority condvar)
                      (lambda () (lambda () (values)))
                      (let ((waiters (condvar.waiters condvar)))
                        (set-condvar.waiters! condvar #f)
                        (lambda ()
                          (for-each
                           (lambda (waiter)
                             (maybe-resume waiter (lambda () (values))))
                           waiters)
                          (lambda () (values))))))))))
        (exit-critical-section critical-token continuation)))))

(define (condvar-prv condvar)
  (make-prv
   (lambda ()                           ;poll
     (with-condvar-locked condvar
       (lambda ()
         (let ((priority (condvar.priority condvar)))
           (if priority
               (begin (set-condvar.priority! condvar (+ priority 1))
                      priority)
               #f)))))
   (lambda (if-enabled if-disabled)     ;enable
     if-disabled ;ignore
     (with-condvar-locked condvar
       (lambda ()
         (set-condvar.priority! condvar 1)))
     (if-enabled (lambda () (values))))
   (lambda (suspension if-enabled if-blocked) ;block
     ((with-condvar-locked condvar
        (lambda ()
          (let ((waiters (condvar.waiters condvar)))
            (if waiters
                (begin (set-condvar.waiters! condvar (cons suspension waiters))
                       if-blocked)
                (lambda ()
                  (if-enabled (lambda () (values))))))))))
   (lambda (thunk) thunk)))
