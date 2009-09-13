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

(define (poll-channel channel queue-accessor)
  (with-channel-locked channel
    (lambda ()
      (if (queue-empty? (queue-accessor channel))
          (let ((priority (channel.priority channel)))
            (set-channel.priority! channel (+ priority 1))
            priority)
          #f))))

;;; Argh blargle nearly symmetric blurf.

(define (channel-send-rendezvous channel message)

  (define (seek-receiver if-found if-not-found)
    (with-channel-locked channel
      (lambda ()
        (let ((receivers (channel.receivers channel)))
          (let loop ()
            (if (queue-empty? receivers)
                (if-not-found)
                (let ((receiver (dequeue! receivers)))
                  (if (maybe-resume receiver (lambda () message))
                      (if-found)
                      (loop)))))))))

  (define (poll)
    (poll-channel channel channel.receivers))

  (define (enable if-enabled if-disabled)
    ((seek-receiver (lambda () (lambda () (if-enabled (lambda () (values)))))
                    (lambda () if-disabled))))

  (define (block suspension if-enabled if-blocked)
    ((seek-receiver
      (lambda () (lambda () (if-enabled (lambda () (values)))))
      (lambda ()
        (enqueue! (channel.senders channel) (cons suspension message))
        if-blocked))))

  (base-rendezvous poll enable block))

(define (channel-receive-rendezvous channel)

  (define (seek-sender if-found if-not-found)
    (with-channel-locked channel
      (lambda ()
        (let ((senders (channel.senders channel)))
          (let loop ()
            (if (queue-empty? senders)
                (if-not-found)
                (let ((sender.message (dequeue! senders)))
                  (let ((sender (car sender.message))
                        (message (cdr sender.message)))
                    (if (maybe-resume sender (lambda () (values)))
                        (if-found message)
                        (loop))))))))))

  (define (poll)
    (poll-channel channel channel.senders))

  (define (enable if-enabled if-disabled)
    ((seek-sender
      (lambda (message) (lambda () (if-enabled (lambda () message))))
      (lambda () if-disabled))))

  (define (block suspension if-enabled if-blocked)
    ((seek-sender
      (lambda (message) (lambda () (if-enabled (lambda () message))))
      (lambda ()
        (enqueue! (channel.receivers channel) suspension)
        if-blocked))))

  (base-rendezvous poll enable block))
