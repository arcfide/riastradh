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
      (with-mailbox-locked mailbox
        (lambda ()
          (let ((queue (mailbox.queue mailbox)))
            (if (mailbox.priority mailbox)
                (enqueue! queue message)
                (let loop ()
                  (cond ((queue-empty? queue)
                         (set-mailbox.priority! mailbox 1)
                         (enqueue! queue message))
                        ((not
                          (maybe-resume (dequeue! queue) (lambda () message)))
                         (loop))))))))
      (exit-critical-section critical-token (lambda () (values))))))

(define (mailbox-receive mailbox)
  (synchronize (mailbox-receive-rendezvous mailbox)))

(define (mailbox-receive-rendezvous mailbox)

  (define (seek-message if-found if-not-found)
    (let ((queue (mailbox.queue mailbox)))
      (with-mailbox-locked mailbox
        (lambda ()
          (if (queue-empty? queue)
              (if-not-found)
              (let ((message (dequeue! queue)))
                (set-mailbox.priority! mailbox (if (queue-empty? queue) #f 1))
                (if-found message)))))))

  (define (poll)
    (with-mailbox-locked mailbox
      (lambda ()
        (let ((priority (mailbox.priority mailbox)))
          (if priority
              (begin (set-mailbox.priority! mailbox (+ priority 1))
                     priority)
              #f)))))

  (define (enable if-enabled if-disabled)
    ((seek-message
      (lambda (message) (lambda () (if-enabled (lambda () message))))
      (lambda () if-disabled))))

  (define (block suspension if-enabled if-blocked)
    ((seek-message
      (lambda (message) (lambda () (if-enabled (lambda () message))))
      (lambda ()
        (enqueue! (mailbox.queue mailbox) suspension)
        if-blocked))))

  (base-rendezvous poll enable block))
