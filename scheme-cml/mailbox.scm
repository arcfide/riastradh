;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Mailboxes: Asynchronous Interprocess Communication

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

(define-locked-record-type <mailbox>
    (%make-mailbox priority queue)
    (priority)
    mailbox?
    with-mailbox-locked
  (priority mailbox.priority set-mailbox.priority!)
  (queue mailbox.queue set-mailbox.queue!))

(define (make-mailbox)
  (%make-mailbox #f (make-queue)))

(define (mailbox-send mailbox message)
  (enter-critical-section
    (lambda (critical-token)
      ;** Do not beta-reduce -- bug in Scheme48's auto-integrator.
      (let ((continuation
             (let ((queue (mailbox.queue mailbox)))
               (let loop ()
                 ((with-mailbox-locked mailbox
                    (lambda ()
                      (if (or (mailbox.priority mailbox)
                              (and (queue-empty? queue)
                                   (begin (set-mailbox.priority! mailbox 1)
                                          #t)))
                          (begin (enqueue! queue message)
                                 (lambda () (lambda () (values))))
                          (with-suspension-claimed (dequeue! queue)
                            (lambda (resume disclaim)
                              disclaim  ;ignore
                              (lambda ()
                                (lambda ()
                                  (resume (lambda () message)))))
                            (lambda ()
                              loop))))))))))
        (exit-critical-section critical-token continuation)))))

(define (mailbox-receive mailbox)
  (synchronize (mailbox-receive-rendezvous mailbox)))

(define (mailbox-receive-rendezvous mailbox)

  (define (frobnitz if-enabled if-lost/locked)
    ((let ((queue (mailbox.queue mailbox)))
       (with-mailbox-locked mailbox
         (lambda ()
           (if (queue-empty? queue)
               (if-lost/locked)
               (let ((message (dequeue! queue)))
                 (set-mailbox.priority! mailbox (if (queue-empty? queue) #f 1))
                 (lambda ()
                   (if-enabled (lambda () message))))))))))

  (define (poll)
    (with-mailbox-locked mailbox
      (lambda ()
        (let ((priority (mailbox.priority mailbox)))
          (if priority
              (begin (set-mailbox.priority! mailbox (+ priority 1))
                     priority)
              #f)))))

  (define (enable if-enabled if-disabled)
    (frobnitz if-enabled (lambda () if-disabled)))

  (define (block suspension if-enabled if-blocked)
    (frobnitz if-enabled
              (lambda ()
                (enqueue! (mailbox.queue mailbox) suspension)
                if-blocked)))

  (base-rendezvous poll enable block))
