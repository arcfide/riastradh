;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for TRC Text Parser Combinators
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

(library (riastradh parscheme partext)
  (export
    parse-file
    parse-input-chars
    parse-string
    parser:bracketed-string
    parser:char
    parser:char=
    parser:char/=
    parser:char-ci=
    parser:char-ci/=
    parser:char-in-set
    parser:char-not-in-set
    parser:list->string
    parser:match->string
    parser:reverse-list->string
    parser:string=
    parser:string-ci=
    parser:string:at-least
    parser:string:at-least-until
    parser:string:at-most
    parser:string:at-most-until
    parser:string:between
    parser:string:between-until
    parser:string:exactly
    parser:string:repeated
    parser:string:repeated-until)
  (import
    (rnrs base)
    (rnrs io simple)
    (rnrs unicode)
    (srfi :6)
    (srfi :14)
    (riastradh parscheme lazy)
    (riastradh parscheme stream)
    (riastradh parscheme parcomb)
    (srfi private include))

(include/resolve-ci ("riastradh" "parscheme") "partext.scm")

)
