;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Channels: Synchronous Interprocess Communication

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

(define-locked-record-type <channel>
    (%make-channel priority senders receivers)
    (priority)
    channel?
    with-channel-locked
  (priority channel.priority set-channel.priority!)
  (senders channel.senders)
  (receivers channel.receivers))

(define (make-channel)
  (%make-channel 1 (make-queue) (make-queue)))

(define (channel-send channel message)
  (synchronize (channel-send-rendezvous channel message)))

(define (channel-receive channel)
  (synchronize (channel-receive-rendezvous channel)))

(define (channel-rendezvous channel enqueue-accessor dequeue-accessor
                            waiter->suspension resumer waiter->enabler
                            make-waiter)

  (define (frobnitz if-enabled if-lost/locked)
    ((with-channel-locked channel
       (lambda ()
         (let ((queue (dequeue-accessor channel)))
           (let loop ()
             (if (queue-empty? queue)
                 (if-lost/locked)
                 (let ((waiter (dequeue! queue)))
                   (with-suspension-claimed (waiter->suspension waiter)
                     (lambda (resume disclaim)
                       disclaim         ;ignore
                       (set-channel.priority! channel 1)
                       (lambda ()
                         (if-enabled
                          (let ((enabler (waiter->enabler waiter)))
                            (lambda ()
                              (resume resumer)
                              (enabler))))))
                     loop)))))))))

  (define (poll)
    (with-channel-locked channel
      (lambda ()
        (if (queue-empty? (dequeue-accessor channel))
            (let ((priority (channel.priority channel)))
              (set-channel.priority! channel (+ priority 1))
              priority)
            #f))))

  (define (enable if-enabled if-disabled)
    (frobnitz if-enabled (lambda () if-disabled)))

  (define (block suspension if-enabled if-blocked)
    (frobnitz if-enabled
              (lambda ()
                (enqueue! (enqueue-accessor channel) (make-waiter suspension))
                if-blocked)))

  (base-rendezvous poll enable block))

(define (channel-send-rendezvous channel message)
  (channel-rendezvous channel channel.senders channel.receivers
    (lambda (receiver) receiver)        ;waiter->suspension
    (lambda () message)                 ;resumer
    (lambda (receiver)                  ;waiter->enabler
      receiver ;ignore
      (lambda () (values)))
    (lambda (suspension)                ;make-waiter
      (cons suspension message))))

(define (channel-receive-rendezvous channel)
  (channel-rendezvous channel channel.receivers channel.senders
    car                                 ;waiter->suspension
    values                              ;resumer
    (lambda (sender.message)            ;waiter->enabler
      (let ((message (cdr sender.message)))
        (lambda () message)))
    (lambda (suspension)                ;make-waiter
      suspension)))
