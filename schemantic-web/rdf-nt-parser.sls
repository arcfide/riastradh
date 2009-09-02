;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for TRC RDF NT Parser
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

(library (riastradh schemantic-web rdf-nt-parser)
  (export
    nt-parser:document
    make-nt-parser-context)
  (import
    (except (rnrs base) for-each map error)
    (srfi :1)
    (srfi :9)
    (srfi :14)
    (srfi :23)
    (riastradh schemantic-web uri)
    (riastradh schemantic-web rdf)
    (riastradh parscheme matcomb)
    (riastradh parscheme mattext)
    (riastradh parscheme parcomb)
    (riastradh parscheme partext)
    (srfi private include))

(define ascii->char integer->char)
(define char->ascii char->integer)
(define ascii-limit #x110000)

(include/resolve-ci ("riastradh" "schemantic-web") "rdf-nt-parser.scm")

)