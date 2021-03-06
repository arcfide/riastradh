;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channels for R6RS
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

(library (riastradh scheme-cml semaphores)
	(export 
		make-semaphore
		semaphore-acquire
		semaphore-acquire-rendezvous
		semaphore-release
		semaphore-value
		semaphore?
		semaphore-test)
	(import
		(rnrs base)
		(rnrs io simple)
		(riastradh scheme-cml queues)
		(riastradh scheme-cml rendezvous)
		(riastradh scheme-cml channels)
		(riastradh scheme-cml mailboxes)
		(riastradh scheme-cml placeholders)
		(srfi :9)
		(riastradh scheme-cml threading)
		(riastradh scheme-cml misc)
		(srfi private include))

(include/resolve-ci ("riastradh" "scheme-cml") "semaphore.scm")

)