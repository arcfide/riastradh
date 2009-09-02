;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for TRC RDF Maps
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

(library (riastradh schemantic-web rdf-map)
  (export 
    (rename (map->alist rdf-object-map->alist))
    (rename (map/datum-list rdf-object-map/datum-list))
    (rename (map/delete! rdf-object-map/delete!))
    (rename (map/key-list rdf-object-map/key-list))
    (rename (map/insert! rdf-object-map/insert!))
    (rename (map/intern! rdf-object-map/intern!))
    (rename (map/lookup rdf-object-map/lookup))
    (rename (map/search rdf-object-map/search))
    (rename (map/size rdf-object-map/size))
    (rename (map/update! rdf-object-map/update!))
    (rename (map/modify! rdf-object-map/modify!))
    (rename (map/walk rdf-object-map/walk))
    (rename (in-map in-rdf-object-map))

    (rename (map->alist rdf-predicate-map->alist))
    (rename (map/datum-list rdf-predicate-map/datum-list))
    (rename (map/delete! rdf-predicate-map/delete!))
    (rename (map/key-list rdf-predicate-map/key-list))
    (rename (map/insert! rdf-predicate-map/insert!))
    (rename (map/intern! rdf-predicate-map/intern!))
    (rename (map/lookup rdf-predicate-map/lookup))
    (rename (map/search rdf-predicate-map/search))
    (rename (map/size rdf-predicate-map/size))
    (rename (map/update! rdf-predicate-map/update!))
    (rename (map/modify! rdf-predicate-map/modify!))
    (rename (map/walk rdf-predicate-map/walk))
    (rename (in-map in-rdf-predicate-map))

    (rename (map->alist rdf-subject-map->alist))
    (rename (map/datum-list rdf-subject-map/datum-list))
    (rename (map/delete! rdf-subject-map/delete!))
    (rename (map/key-list rdf-subject-map/key-list))
    (rename (map/insert! rdf-subject-map/insert!))
    (rename (map/intern! rdf-subject-map/intern!))
    (rename (map/lookup rdf-subject-map/lookup))
    (rename (map/search rdf-subject-map/search))
    (rename (map/size rdf-subject-map/size))
    (rename (map/update! rdf-subject-map/update!))
    (rename (map/modify! rdf-subject-map/modify!))
    (rename (map/walk rdf-subject-map/walk))
    (rename (in-map in-rdf-subject-map))

    make-rdf-object-map
    make-rdf-predicate-map
    make-rdf-subject-map)
  (import
    (rnrs base)
    (rnrs hashtables)
    (rnrs records syntactic)
    (only (scheme) let-values)
    (riastradh schemantic-web rdf)
    (riastradh foof-loop loop)
    (srfi private include))

(define make-table
  (lambda hasher
    (if (null? hasher)
        (make-eq-hashtable)
        (make-hashtable (car hasher) eq?))))

(define table-ref
  (lambda (t k)
    (hashtable-ref t k #f)))

(define table-set!
  (lambda (t k v)
    (hashtable-set! t k v)))

(define table-walk
  (lambda (p t)
    (let-values ([(keys vals) (hashtable-entries t)])
      (loop ([for k (in-vector keys)]
             [for v (in-vector vals)])
        (p k v)))))

(define make-table-maker
  (lambda (compare hash)
    (lambda () (make-hashtable hash compare))))

(include/resolve-ci ("riastradh" "schemantic-web") "s48-rdf-map.scm")

)