;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Testing Utility for Scheme
;;;; Scheme48 Interfaces

;;; Copyright (C) 2007, 2009 Taylor R. Campbell.
;;;
;;; This file is part of TRC-Testing.
;;;
;;; TRC-Testing is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; TRC-Testing is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with TRC-Testing.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-interface testing-interface
  (export
    (define-test-suite :syntax)
    make-test-suite
    test-suite?
    test-suite/name
    test-suite/description
    test-suite/tests

    (define-test-case :syntax)
    (test-case :syntax)
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

    (test-predicate :syntax)
    (test-compare :syntax)
    (test-eq :syntax)
    (test-eqv :syntax)
    (test-equal :syntax)

    ;; Reexport these for convenience.  Ordinarily reexportation is
    ;; anathema, but I think that here it is probably safe, because
    ;; most users will never use TESTING-PARAMETERS anyway.
    test-failure
    test-failure:predicate-datum
    test-failure:compare-datum

    test-verbosity      with-test-verbosity     set-test-verbosity!
    test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!
    ))

(define-interface testing-parameters-interface
  (export
    with-test-case-run
    with-test-suite-run
    niladic-test
    monadic-test
    polyadic-test
    component-test
    test-failure
    test-failure:predicate-datum
    test-failure:compare-datum

    ;; Customization
    test-verbosity      with-test-verbosity     set-test-verbosity!
    test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!
    ))
