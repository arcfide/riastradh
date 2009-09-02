;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Library for matcomb
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

(library (riastradh parscheme matcomb)
  (export 
    match
    define-matcher
    
    matcher:at-least
    matcher:at-least-until
    matcher:at-most
    matcher:at-most-until
    matcher:between
    matcher:between-until
    matcher:bracketed
    matcher:bracketed*
    matcher:choice
    matcher:comparison
    matcher:deep-choice
    matcher:end
    matcher:epsilon
    matcher:error
    matcher:exactly
    matcher:if
    matcher:left-comparison
    matcher:optional
    matcher:peek
    matcher:repeated
    matcher:repeated-until
    matcher:right-comparison
    matcher:sequence
    matcher:token
    matcher:token-if

    comparator-matcher
    left-comparator-matcher
    right-comparator-matcher
    guarded-matcher

	enable-match-trace
    disable-match-trace)
  (import 
    (rnrs base)
    (rnrs io simple)
    (except (srfi :1) for-each map)
    (riastradh parscheme lazy)
    (riastradh parscheme stream)
    (srfi private include))
  
(include/resolve-ci ("riastradh" "parscheme") "matcomb.scm")

)
