;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrapper for TRC Simple RDF Graphs
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

(library (riastradh schemantic-web rdf-simple-graph)
  (export
    make-rdf-graph
    rdf-graph?
    rdf-graph/add-triple!
    rdf-graph/all-objects
    rdf-graph/all-predicates
    rdf-graph/all-subjects
    rdf-graph/all-triples
    rdf-graph/for-each-matching-object
    rdf-graph/for-each-matching-predicate
    rdf-graph/for-each-matching-subject
    rdf-graph/for-each-object
    rdf-graph/for-each-predicate
    rdf-graph/for-each-subject
    rdf-graph/for-each-triple-by-object
    rdf-graph/for-each-triple-by-predicate
    rdf-graph/for-each-triple-by-subject
    rdf-graph/for-each-triple
    rdf-graph/matching-objects
    rdf-graph/matching-predicates
    rdf-graph/matching-subjects
    ;; rdf-graph/purge-matching-objects!
    ;; rdf-graph/purge-matching-predicates!
    ;; rdf-graph/purge-matching-subjects!
    ;; rdf-graph/purge-triples-by-object!
    ;; rdf-graph/purge-triples-by-predicate!
    ;; rdf-graph/purge-triples-by-subject!
    rdf-graph/remove-triple!
    rdf-graph/size
    rdf-graph/triples-by-object
    rdf-graph/triples-by-predicate
    rdf-graph/triples-by-subject)
  (import
    (except (rnrs base) for-each map)
    (srfi :1)
    (srfi :9)
    (riastradh schemantic-web rdf)
    (riastradh schemantic-web rdf-map)
    (riastradh foof-loop)
    (srfi private include))

(include/resolve-ci ("riastradh" "schemantic-web") "rdf-simple-graph.scm")

)