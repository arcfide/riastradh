;;; -*- Mode: Scheme -*-

;;;; Concurrent ML for Scheme
;;;; Primitive Rendezvous Substrate

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

(define-record-type <prv>
    (make-prv poller enabler blocker composition)
    prv?
  (poller prv.poller)
  (enabler prv.enabler)
  (blocker prv.blocker)
  (composition prv.composition))

(define (map-prv prv procedure)
  (make-prv (prv.poller prv)
            (prv.enabler prv)
            (prv.blocker prv)
            (compose-unary procedure (prv.composition prv))))

(define (primitive-synchronize prvs)
  (%primitive-poll prvs
    (lambda (critical-token thunk)
      (exit-critical-section critical-token (lambda () thunk)))
    (lambda (critical-token prvs)
      (suspend critical-token
        (lambda (make-suspension if-suspending if-not-suspending)
          (let loop ((prvs prvs))
            (if (null-list? prvs)
                (if-suspending)
                (let ((composition (prv.composition (car prvs))))
                  ((prv.blocker (car prvs))
                   (make-suspension composition)
                   (lambda (thunk)
                     (if-not-suspending (lambda () (composition thunk))))
                   (lambda ()
                     (loop (cdr prvs))))))))))))

(define (primitive-poll prv if-enabled if-blocked)
  (%primitive-poll prv
    (lambda (critical-token thunk)
      (exit-critical-section critical-token
        (lambda ()
          (if-enabled thunk))))
    (lambda (critical-token prvs)
      prvs                              ;ignore
      (exit-critical-section critical-token if-blocked))))

(define (%primitive-poll prvs if-enabled if-blocked)
  (let ((all-prvs prvs))
    (enter-critical-section
      (lambda (critical-token)
        (let loop ((prvs all-prvs) (priority.enabled-prv-list '()))
          (if (null-list? prvs)
              (let loop ((priority.enabled-prv-list
                          (sort-by-priority! priority.enabled-prv-list)))
                (if (null-list? priority.enabled-prv-list)
                    (if-blocked critical-token all-prvs)
                    (let ((prv (cdar priority.enabled-prv-list)))
                      ((prv.enabler prv)
                       (lambda (thunk)
                         (if-enabled critical-token
                                     ((prv.composition prv) thunk)))
                       (lambda ()
                         (loop (cdr priority.enabled-prv-list)))))))
              (loop (cdr prvs)
                    (let ((priority ((prv.poller (car prvs)))))
                      (if priority
                          (cons (cons priority (car prvs))
                                priority.enabled-prv-list)
                          priority.enabled-prv-list)))))))))

(define (sort-by-priority! priority.datum-list)
  ;++ This is not completely fair: it should use some randomization.
  (list-sort! (lambda (a b)
                (let ((ap (car a)) (bp (car b)))
                  (or (and (= ap -1) (= bp -1))
                      (< ap bp))))
              priority.datum-list))
