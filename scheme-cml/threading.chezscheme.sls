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
		make-suspender
		suspender/abort suspender/lock suspender/resume suspender/resumed?
		suspender/suspend suspender/unlock)
	(import (chezscheme))

(define (enter-critical-section proc)
	(proc 'dummy))

(define (exit-critical-section token continuation)
	(continuation))

(define-record-type suspender
	(fields (immutable mutex) (immutable condition)
		(mutable set?) (mutable value))
	(protocol (lambda (p) (lambda () (p (make-mutex) (make-condition) #f #f)))))

(define (suspender/lock s)
	(mutex-acquire (suspender-mutex s)))

(define (suspender/unlock s)
	(mutex-release (suspender-mutex s)))

(define (suspender/resumed? s)
	(suspender-set? s))

(define (suspender/abort s)
	(suspender-set?-set! s #t))

(define (suspender/resume s v)
	(suspender-set?-set! s #t)
	(suspender-value-set! s v)
	(condition-signal (suspender-condition s)))

(define (suspender/suspend t s)
	(let loop ()
		(condition-wait (suspender-condition s) (suspender-mutex s))
		(if (suspender-set? s)
			(let ([v (suspender-value s)])
				(suspender-value-set! s #f)
				v)
			(loop))))

)