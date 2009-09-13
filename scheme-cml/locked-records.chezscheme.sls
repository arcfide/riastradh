;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Locked Records
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

(library (riastradh scheme-cml locked-records)
	(export define-locked-record-type)
	(import (chezscheme))

(define-syntax define-locked-record-type
	(lambda (x)
		(define (spec x)
			(syntax-case x ()
				[(name) #'name]
				[(name get) #'(immutable name get)]
				[(name get set) #'(mutable name get set)]))
		(syntax-case x ()
			[(_ name (make arg ...) (locked ...) pred with (field getter ...) ...)
				(with-syntax ([(specs ...) (map spec #'((field getter ...) ...))])
					#'(begin
						(define-record-type (name make pred)
							(fields (immutable mutex get-mutex) specs ...)
							(protocol (lambda (p) (lambda (arg ...) (p (make-mutex) arg ...)))))
						(define (with x thunk)
							(with-mutex (get-mutex x) (thunk)))))])))

)