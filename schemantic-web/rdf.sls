;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for TRC Rdf Library
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

(library (riastradh schemantic-web rdf)
  (export
    make-rdf-triple
    rdf-triple?
    rdf-triple/subject
    rdf-triple/predicate
    rdf-triple/object
    rdf-triple=?
    rdf-triple-hash
    rdf-triple-hash-mod

    rdf-subject=?       rdf-subject-hash        rdf-subject-hash-mod
    rdf-predicate=?     rdf-predicate-hash      rdf-predicate-hash-mod
    rdf-object=?        rdf-object-hash         rdf-object-hash-mod

    make-rdf-bnode
    rdf-bnode?
    rdf-bnode/name
    rdf-bnode=?
    rdf-bnode-hash

    make-rdf-plain-literal
    make-rdf-typed-literal
    rdf-literal?
    rdf-plain-literal?
    rdf-typed-literal?
    rdf-literal/lexical-form
    rdf-plain-literal/language-tag
    rdf-typed-literal/datatype-uri
    rdf-literal=?
    rdf-literal-hash

    rdf-uri-ref?
    rdf-uri-ref->string
    string->rdf-uri-ref
    rdf-uri-ref=?
    rdf-uri-ref-hash)
  (import
    (except (rnrs base) error)
    (srfi :9)
    (rnrs hashtables)
    (srfi :23)
    (rename (rnrs arithmetic fixnums)
      (fxxor bitwise-xor))
    (riastradh schemantic-web uri)
    (srfi private include))

(define modulo mod)
(define string-hash-mod (lambda (s m) (mod (string-hash s) m)))

(include/resolve-ci ("riastradh" "schemantic-web") "rdf.scm")

)
