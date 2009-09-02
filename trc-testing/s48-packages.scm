;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Testing Utility for Scheme
;;;; Scheme48 Packages

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

(define-module (make-testing testing-parameters)
  (structure testing-interface
    (open scheme
          receiving
          srfi-9                        ;define-record-type
          simple-signals
          testing-parameters
          )
    (optimize auto-integrate)
    (files syn-param test)

    ;; Make test suites & cases print nicely.
    (open (subset define-record-types (define-record-discloser)))
    (begin
      (define-record-discloser <test-suite>
        (lambda (test-suite)
          `(TEST-SUITE ,(test-suite/name test-suite)
                       ,@(cond ((test-suite/description test-suite)
                                => list)
                               (else '())))))
      (define-record-discloser <test-case>
        (lambda (test-case)
          `(TEST-CASE ,(test-case/name test-case)
                      ,@(cond ((test-case/description test-case)
                               => list)
                              (else '()))))))))

(define-structure simple-testing-parameters testing-parameters-interface
  (open scheme
        fluids
        cells
        simple-signals
        handle
        display-conditions
        restarting
        simple-conditions
        i/o
        extended-ports
        )
  (optimize auto-integrate)
  (files s48-test))

(def simple-testing (make-testing simple-testing-parameters))
