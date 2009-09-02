;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for URIs
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

(library (riastradh schemantic-web uri)
  (export
    make-uri
    uri?
    absolute-uri?
    relative-uri?
    uri-absolute?
    uri-relative?
    uri-scheme
    uri-authority
    uri-path
    uri-query
    uri-fragment
    uri=?

    ;; URI Authorities
    make-uri-authority
    uri-authority?
    uri-authority-userinfo
    uri-authority-host
    uri-authority-port
    uri-authority=?

    ;; URI Component Predicates
    uri-scheme?
    uri-userinfo?
    uri-host?
    uri-port?
    uri-path?
    uri-path-absolute?
    uri-path-relative?
    uri-query?
    uri-fragment?

    ;; URI Operations
    merge-uris

    ;; URI->String Conversion
    uri->string
    uri-authority->string
    write-uri
    write-uri-authority

    ;; String->URI Conversion
    object->uri
    object->absolute-uri
    object->relative-uri
    maybe-string->uri
    maybe-string->absolute-uri
    maybe-string->relative-uri
    string->uri
    string->absolute-uri
    string->relative-uri

    uri-parser:uri-reference
    uri-parser:uri
    uri-parser:relative-ref
    uri-parser:absolute-uri             ;No fragment
    uri-matcher:uri-reference
    uri-matcher:uri
    uri-matcher:relative-ref
    uri-matcher:absolute-uri)
  (import
    (except (rnrs base) error)
    (rnrs hashtables)
    (rnrs unicode)
    (rnrs io simple)
    (rnrs control)
    (rnrs r5rs)
    (except (srfi :1) map for-each)
    (srfi :6)
    (srfi :9)
    (except (srfi :13) 
      string-hash string-copy string->list string-for-each 
      string-downcase string-titlecase string-upcase)
    (srfi :14)
    (srfi :23)
    (only (scheme) 
      call-with-string-output-port weak-cons bwp-object?)
    (riastradh parscheme matcomb)
    (riastradh parscheme mattext)
    (riastradh parscheme parcomb)
    (riastradh parscheme partext)
    (riastradh parscheme perror)
    (srfi private include))

;;; Alias display -> write-string

(define write-string display)

;;; This is dangerous and probably willy break

(define char->ascii char->integer)
(define ascii->char integer->char)

;;; String Internments

(define make-string-internment-camp
  (lambda ()
    (make-hashtable string-hash string=?)))

(define intern
  (lambda (camp s generator)
    (cond 
      [(hashtable-ref camp s #f)
       => (lambda (w)
            (let ([val (car w)])
              (if (bwp-object? val) 
                  (let ([new-val (generator)])
                    (set-car! w new-val)
                    new-val)
                  val)))]
      [else
       (let ([val (generator)])
         (hashtable-set! camp s (weak-cons val '()))
         val)])))

(define soft-intern
  (lambda (camp s)
    (cond
      [(hashtable-ref camp s #f)
       => (lambda (w)
            (let ([v (car w)])
              (and (not (bwp-object? v)) v)))]
      [else #f])))

(include/resolve-ci ("riastradh" "schemantic-web") "uri.scm")

)