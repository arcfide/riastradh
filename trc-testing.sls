;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRC Testing R6RS Wrapper and Definitions
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

(library (riastradh trc-testing)
  (export 
    define-test-suite
    make-test-suite
    test-suite?
    test-suite/name
    test-suite/description
    test-suite/tests

    define-test-case
    test-case
    make-test-case
    test-case?
    test-case/name
    test-case/description
    test-case/constructor

    add-test!
    run-test-case
    run-test-suite
    run-test
    find-test

    test-predicate
    test-compare
    test-eq
    test-eqv
    test-equal

    test-failure
    test-failure:predicate-datum
    test-failure:compare-datum

    test-verbosity      with-test-verbosity     set-test-verbosity!
    test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!)
  (import 
    (rnrs base)
    (rnrs mutable-pairs)
    (rnrs lists)
    (only (scheme) void)
    (srfi :8)
    (srfi :9)
    (riastradh trc-testing parameters)
    (srfi private include))

(include/resolve-ci ("riastradh" "trc-testing") "syn-param.scm")
(include/resolve-ci ("riastradh" "trc-testing") "test.ss")

)