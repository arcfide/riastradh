;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Placeholders: Single-Assignment Synchronized Cells

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

(define-locked-record-type <placeholder>
    (%make-placeholder priority waiters-then-value)
    (priority waiters-then-value)
    placeholder?
    with-placeholder-locked
  (priority placeholder.priority set-placeholder.priority!)
  (waiters-then-value placeholder.waiters-then-value
                      set-placeholder.waiters-then-value!))

(define (make-placeholder)
  (%make-placeholder #f '()))

(define (set-placeholder! placeholder value)
  (enter-critical-section
    (lambda (critical-token)
      ;** Do not beta-reduce -- bug in Scheme48's auto-integrator.
      (let ((continuation
             (with-placeholder-locked placeholder
               (lambda ()
                 ;; Read WAITERS-THEN-VALUE first: it will not change
                 ;; *after* the priority is set to a number, but if we
                 ;; read the priority first and find that it is #F, the
                 ;; placeholder may still be set before we read the
                 ;; WAITERS-THEN-VALUE field.
                 (let ((waiters
                        (placeholder.waiters-then-value placeholder)))
                   (if (placeholder.priority placeholder)
                       (lambda ()
                         (error "Placeholder is already assigned:"
                                placeholder))
                       (begin
                         (set-placeholder.waiters-then-value! placeholder
                                                              value)
                         (set-placeholder.priority! placeholder 1)
                         (lambda ()
                           (for-each (lambda (waiter)
                                       (maybe-resume waiter (lambda () value)))
                                     waiters)))))))))
        (exit-critical-section critical-token continuation)))))

(define (placeholder-value placeholder)
  (synchronize (placeholder-value-rendezvous placeholder)))

(define (placeholder-value-rendezvous placeholder)

  (define (poll)
    (with-placeholder-locked placeholder
      (lambda ()
        (let ((priority (placeholder.priority placeholder)))
          (if priority
              (begin (set-placeholder.priority! placeholder (+ priority 1))
                     priority)
              #f)))))

  (define (enable if-enabled if-disabled)
    if-disabled                         ;ignore
    ;; Locking the placeholder isn't really necessary here: the
    ;; WAITERS-THEN-VALUE field will never be changed once the priority
    ;; field is set to an integer (we are guaranteed by POLL that it
    ;; has been when we enter ENABLE), and setting the priority field
    ;; probably shouldn't require locking it, but the locked record
    ;; abstraction may require that we have it locked in order to read
    ;; or write any of the fields.
    (let ((value
           (with-placeholder-locked placeholder
             (lambda ()
               (set-placeholder.priority! placeholder 1)
               (placeholder.waiters-then-value placeholder)))))
     (if-enabled (lambda () value))))

  (define (block suspension if-enabled if-blocked)
    ((with-placeholder-locked placeholder
       (lambda ()
         (let ((waiters-then-value
                (placeholder.waiters-then-value placeholder)))
           (if (placeholder.priority placeholder)
               (lambda () (if-enabled (lambda () waiters-then-value)))
               (begin
                 (set-placeholder.waiters-then-value!
                  placeholder
                  (cons suspension waiters-then-value))
                 if-blocked)))))))

  (base-rendezvous poll enable block))
