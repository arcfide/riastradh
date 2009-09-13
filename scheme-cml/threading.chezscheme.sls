;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Threading support for Scheme CML
;;; Version: 1
;;; 
;;; Copyright (c) 2009 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(library (riastradh scheme-cml threading)
	(export 
		enter-critical-section
		exit-critical-section
		suspend
		maybe-resume)
	(import (chezscheme))

(define-record-type (<suspension-token> make-suspension-token suspension-token?)
	(fields
		(mutable thunk suspension-token-thunk suspension-token-thunk-set!)
		(immutable mutex suspension-token-mutex)
		(immutable condition suspension-token-condition))
	(protocol (lambda (p) (lambda () (p #f (make-mutex) (make-condition))))))

(define-record-type suspension
	(fields token composition))

(define (enter-critical-section proc)
	(proc 'dummy))

(define (exit-critical-section token continuation)
	(continuation))

(define (suspend critical-token procedure)
	(let ((token (make-suspension-token)))
		((with-mutex (suspension-token-mutex token)
			 (procedure
				(lambda (composition) (make-suspension token composition))
				(lambda ()
					(condition-wait 
						(suspension-token-condition token)
						(suspension-token-mutex token))
					(suspension-token-thunk token))
				(lambda (k) k))))))

(define (maybe-resume suspension thunk)
	(let ([token (suspension-token suspension)])
		(with-mutex (suspension-token-mutex token)
			(if (not (suspension-token-thunk token))
				(begin 
					(suspension-token-thunk-set! token
						(lambda () ((suspension-composition suspension) thunk)))
					(condition-signal (suspension-token-condition token))
					#t)
				#f))))

)