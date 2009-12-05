;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Simple, Non-Synchronized FIFO Queue Data Structure

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

(define-record-type <queue>
    (%make-queue front back)
    queue?
  (front queue.front)
  (back queue.back set-queue.back!))

(define (make-queue)
  (let ((pair (cons 'SENTINEL '())))
    (%make-queue pair pair)))

(define (queue-empty? queue)
  (null? (cdr (queue.front queue))))

(define (enqueue! queue element)
  (let ((pair (cons element '())))
    (set-cdr! (queue.back queue) pair)
    (set-queue.back! queue pair)))

(define (dequeue! queue)
  (let* ((front (queue.front queue))
         (pair (cdr front)))
    (if (pair? pair)
        (let ((tail (cdr pair)))
          (set-cdr! front tail)
          (if (null? tail)
              (set-queue.back! queue front))
          (car pair))
        (error "Empty queue:" queue))))
