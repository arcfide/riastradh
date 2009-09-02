;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapping for Taylor Campbell's Foof Loop macros
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

(library (riastradh foof-loop)
  (export 
    loop lazy-loop
    listing listing-reverse listing! listing-into!
    appending appending-reverse
    summing multiplying maximizing minimizing
    in-list in-lists
    in-vector in-vector-reverse
    in-string in-string-reverse
    in-port in-file
    up-from down-from
    => initial to by for with while until

    nested-loop nested-lazy-loop
    iterate* iterate iterate! iterate-values
    recur* lazy-recur* recur lazy-recur recur-values
    collect-list-reverse collect-list collect-list!
    collect-list-into! collect-stream 
    collect-vector collect-into-vector!
    collect-vector-of-length
    collect-string collect-into-string!
    collect-string-of-length
    collect-display
    collect-sum collect-product collect-count
    collect-average 
    collect-extremum collect-minimum collect-maximum
    collect-extremum* collect-minimum* collect-maximum*
    collect-extremum-by collect-minimum-by collect-maximum-by
    collect-first collect-last collect-or collect-and)
  (import (riastradh foof-loop loop) (riastradh foof-loop nested)))
