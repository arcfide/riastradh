;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Semaphore Example

;;; Copyright (c) 2005-2009, Taylor R. Campbell
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

;;; This example demonstrates a variety of the facilities provided by
;;; the rendezvous concurrency abstractions and libraries therefor:
;;;
;;; - Mailboxes, asynchronous communication channels, are used to send
;;;   messages to a control thread for each semaphore.  The messages
;;;   are dispatched by the thread and contain the requisite devices to
;;;   send information back, if necessary.
;;; - Placeholders, single-assignment cells on which readers block, are
;;;   used to return single values from the semaphore control thread in
;;;   response to simple information inquiries from a client thread.
;;; - Channels (synchronous) are used to send semaphore acquisition
;;;   responses from semaphore control threads back to client threads
;;;   that requested them.  The control thread must wait until the
;;;   client receives the message, which is why synchronous channels
;;;   are used instead of mailboxes.
;;; - Negative acknowledgements are used to abort acquisition requests
;;;   if they were not chosen among a set of rendezvous that a client
;;;   synchronized.

(define-record-type <semaphore>
    (%make-semaphore mailbox)
    semaphore?
  (mailbox semaphore.mailbox))

(define (make-semaphore initial-value)
  (let ((mailbox (make-mailbox)))
    (spawn (lambda ()
             (semaphore-loop mailbox initial-value (make-queue)))
           'SEMAPHORE)
    (%make-semaphore mailbox)))

(define-record-type <request:acquire>
    (make-request:acquire reply-channel nack-rv)
    request:acquire?
  (reply-channel request:acquire.reply-channel)
  (nack-rv request:acquire.nack-rv))

(define-record-type <request:release>
    (make-request:release)
    request:release?)

(define-record-type <request:inquire>
    (make-request:inquire reply-placeholder)
    request:inquire?
  (reply-placeholder request:inquire.reply-placeholder))

(define (semaphore-acquire semaphore)
  (synchronize
   (%semaphore-acquire-rendezvous
    semaphore
    ;; No nack rendezvous because there is nothing else to select.
    (quiescent-rendezvous))))

(define (semaphore-acquire-rendezvous semaphore)
  (delayed-rendezvous-with-nack
   (lambda (nack-rv)
     (%semaphore-acquire-rendezvous semaphore nack-rv))))

(define (%semaphore-acquire-rendezvous semaphore nack-rv)
  (let ((reply-channel (make-channel)))
    (mailbox-send (semaphore.mailbox semaphore)
                  (make-request:acquire reply-channel nack-rv))
    (channel-receive-rendezvous reply-channel)))

(define (semaphore-release semaphore)
  (mailbox-send (semaphore.mailbox semaphore)
                (make-request:release)))

(define (semaphore-value semaphore)
  (let ((placeholder (make-placeholder)))
    (mailbox-send (semaphore.mailbox semaphore)
                  (make-request:inquire placeholder))
    (placeholder-value placeholder)))

;;;; Semaphore Controller Thread

(define (semaphore-loop mailbox value pending)
  (let ((request (mailbox-receive mailbox)))
    (cond ((request:acquire? request)
           (enqueue! pending request)
           ((if (positive? value)
                semaphore-grant
                semaphore-loop)
            mailbox
            value
            pending))
          ((request:release? request)
           (semaphore-grant mailbox (+ value 1) pending))
          ((request:inquire? request)
           (set-placeholder! (request:inquire.reply-placeholder request) value)
           (semaphore-loop mailbox value pending))
          (else
           ;; Ignore bogus request.
           ;; (warn "Bogus semaphore request:" request)
           (semaphore-loop mailbox value pending)))))

(define (semaphore-grant mailbox value pending)
  (if (queue-empty? pending)
      (semaphore-loop mailbox value pending)
      (let ((request (dequeue! pending)))
        (rendezvous-case
          ((channel-send-rendezvous (request:acquire.reply-channel request)
                                    value)
           (semaphore-loop mailbox (- value 1) pending))
          ((request:acquire.nack-rv request)
           (semaphore-grant mailbox value pending))))))

;;;; Semaphore Test

;;; The output for (SEMAPHORE-TEST #T) should be:
;;;
;;;   1, a: Acquired semaphore -- value is 0
;;;   2, b: Spawned -- value is 0
;;;   3, b: Timed out -- value is 0
;;;   4, a: Released -- value is 1
;;;   5, a: Slept -- value is 1
;;;
;;; and the output for (SEMAPHORE-TEST #F) should be:
;;;
;;;   1, a: Acquired semaphore -- value is 0
;;;   2, b: Spawned -- value is 0
;;;   3, a: Released -- value is 1
;;;   4, b: Acquired -- value is 0
;;;   5, b: Slept -- value is 0
;;;   6, b: Released -- value is 1
;;;   7, a: Slept -- value is 1

(define (semaphore-test timeout?)
  (let* ((semaphore (make-semaphore 1))
         (counter 0)
         (note (lambda (client message)
                 (let ((value (semaphore-value semaphore)))
                   (set! counter (+ counter 1))
                   (write counter)
                   (display ", ")
                   (write client)
                   (display ": ")
                   (display message)
                   (display " -- value is ")
                   (write value)
                   (newline)))))
    (semaphore-acquire semaphore)
    (note 'A "Acquired semaphore")
    (sleep 1000)
    (spawn (lambda ()
             (note 'B "Spawned")
             (if (not timeout?) (sleep 2000))
             (rendezvous-case
               ((semaphore-acquire-rendezvous semaphore)
                (note 'B "Acquired")
                (sleep 1000)
                (note 'B "Slept")
                (sleep 1000)
                (semaphore-release semaphore)
                (note 'B "Released"))
               ((after-time-rendezvous (if timeout? 1000 2000))
                (note 'B "Timed out"))))
           'SEMAPHORE-TEST)
    (sleep (if timeout? 2000 1000))
    (semaphore-release semaphore)
    (note 'A "Released")
    (sleep (if timeout? 1000 4000))
    (note 'A "Slept")))
