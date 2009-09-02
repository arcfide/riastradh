;;; -*- Mode: Scheme -*-

;;;; Testing Utility for Scheme
;;;; MIT Scheme Parameters

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

;;; Options

(define test:verbosity 'NORMAL)
(define test:debug-errors? #f)

;;; Dynamic state

(define test:name)
(define test:description)

(define test:testing-suite? #f)

(define test-suite:passages)
(define test-suite:failures)

(define (testing-suite?) test:testing-suite?)
(define (test-passages) test-suite:passages)
(define (test-failures) test-suite:failures)

(define (record-test-passages count)
  (set! test-suite:passages (+ test-suite:passages count))
  unspecific)

(define (record-test-failures failures)
  (set! test-suite:failures (append-reverse failures test-suite:failures))
  unspecific)

(define (with-test-case-run name description thunk)
  (with-test-notification name description thunk))

(define (with-test-suite-run name description thunk)
  (receive (passages failures)
           (with-test-notification name description
             (lambda ()
               (fluid-let ((test:testing-suite? #t)
                           (test-suite:passages 0)
                           (test-suite:failures '()))
                 (thunk)
                 (values test-suite:passages test-suite:failures))))
    (if test:testing-suite?
        (begin
          (record-test-passages passages)
          (record-test-failures failures)
          (if (not (eq? test:verbosity 'QUIET))
              (report-test-suite passages failures)))
        (report-test-suite passages failures))))

(define (report-test-suite passages failures)
  (write-notification-line
   (lambda (port)
     (let* ((failures (length failures))
            (total (+ passages failures)))
       (write total port)
       (write-string " tests, " port)
       (write passages port)
       (write-string " passed (" port)
       (write (round (* 100 (/ passages total))) port)
       (write-string "%), " port)
       (write failures port)
       (write-string " failed (" port)
       (write (round (* 100 (/ failures total))) port)
       (write-string "%)" port)
       (newline port)))))

(define (with-test-notification name description thunk)
  (fluid-let ((test:name name) (test:description description))
    (if (eq? test:verbosity 'QUIET)
        (thunk)
        (with-notification report-test thunk))))

(define (report-test port)
  (write-string "Test " port)
  (write test:name port)
  (if (and test:description (eq? test:verbosity 'VERBOSE))
      (begin
        (write-string ":" port)
        (newline port)
        (write-notification-line
         (lambda (port)
           (write-string test:description port))))))

(define (niladic-test)
  (warn "Null test:" test:name))

(define (monadic-test thunk)
  (with-test-restarts #f
    (lambda ()
      (with-test-condition-handler #f thunk))))

(define (polyadic-test thunks)
  (with-test-restarts #f
    (lambda ()
      (do ((thunks thunks (cdr thunks))
           (index 0 (+ index 1)))
          ((not (pair? thunks)))
        (with-test-notification index #f
          (lambda ()
            (with-test-restarts index
              (lambda ()
                (with-test-condition-handler index (car thunks))))))))))

(define (component-test thunk)
  (thunk))

(define (with-test-restarts index thunk)
  (with-simple-restart 'ABORT (test-restart-reporter index "Abort")
    (lambda ()
      (let loop ()
        (with-simple-restart 'RETRY (test-restart-reporter index "Retry")
          (lambda ()
            (thunk)
            (abort)))
        (loop)))))

(define (test-restart-reporter index verb)
  (let ((name test:name))
    (lambda (port)
      (write-string verb port)
      (if index
          (begin
            (write-string " test #" port)
            (write index port)
            (write-string " of " port))
          (write-string " testing " port))
      (write name port)
      (write-string "." port))))

(define (with-test-condition-handler index thunk)
  (bind-condition-handler (list condition-type:error)
      (lambda (condition)
        (if (testing-suite?)
            (record-test-failures (list condition)))
        (handle-test-failure index condition))
    (lambda ()
      (thunk)
      (if (testing-suite?)
          (record-test-passages 1)))))

(define (handle-test-failure index condition)
  (let ((handle-failure
         (lambda (condition)
           (report-test-failure index condition)
           (abort))))
    (cond ((test-failure? condition)
           (handle-failure condition))
          (test:debug-errors?
           ;; This should invoke the debugger.
           (standard-error-handler condition))
          (else
           (handle-failure
            (make-test-failure:error condition))))))

(define (report-test-failure index condition)
  index                                 ;ignore
  (if (eq? test:verbosity 'QUIET)
      (write-notification-line report-test))
  (write-notification-line
   (lambda (port)
     (write-string "*** " port)
     (write-condition-report condition port)
     (newline port))))

;;;; Test Failure Conditions

(define condition-type:test-failure
  (make-condition-type 'TEST-FAILURE condition-type:error '()
    (lambda (condition port)
      condition                         ;ignore
      (write-string "The test failed in an unspecified manner." port))))

(define test-failure?
  (condition-predicate condition-type:test-failure))

(define condition-type:test-failure:error
  (make-condition-type 'TEST-FAILURE:ERROR condition-type:test-failure
      '(CONDITION)
    (lambda (condition port)
      (write-string "The test yielded the error: " port)
      (write-condition-report (access-condition condition 'CONDITION) port))))

(define make-test-failure:error
  (let ((constructor
         (condition-constructor condition-type:test-failure:error
                                '(CONDITION))))
    (named-lambda (make-test-failure:error condition)
      (constructor (condition/continuation condition)
                   (condition/restarts condition)
                   condition))))

(define condition-type:simple-test-failure
  (make-condition-type 'SIMPLE-TEST-FAILURE condition-type:test-failure
      '(MESSAGE IRRITANTS)
    (lambda (condition port)
      (write-string (access-condition condition 'MESSAGE) port)
      (for-each (lambda (irritant)
                  (write-char #\space port)
                  (write irritant port))
                (access-condition condition 'IRRITANTS)))))

(define test-failure
  (let ((signaller
         (condition-signaller condition-type:simple-test-failure
                              '(MESSAGE IRRITANTS)
                              standard-error-handler)))
    (named-lambda (test-failure message . irritants)
      (signaller message irritants))))

(define condition-type:test-failure:predicate-datum
  (make-condition-type 'TEST-FAILURE:PREDICATE-DATUM
      condition-type:test-failure
      '(PREDICATE EXPRESSION DATUM)
    (lambda (condition port)
      (write-string "The object " port)
      (write (access-condition condition 'DATUM) port)
      (write-string " does not satisfy the predicate " port)
      (write (access-condition condition 'PREDICATE) port)
      (write-string "." port))))

(define test-failure:predicate-datum
  (let ((signaller
         (condition-signaller condition-type:test-failure:predicate-datum
                              '(PREDICATE EXPRESSION DATUM)
                              standard-error-handler)))
    (named-lambda (test-failure:predicate-datum predicate expression datum)
      (signaller predicate expression datum))))

(define condition-type:test-failure:compare-datum
  (make-condition-type 'TEST-FAILURE:COMPARE-DATUM
      condition-type:test-failure
      '(COMPARATOR EXPECTED-EXPRESSION EXPECTED-DATUM
                   ACTUAL-EXPRESSION ACTUAL-DATUM)
    (lambda (condition port)
      (write-string "The object " port)
      (write (access-condition condition 'ACTUAL-DATUM) port)
      (write-string " is not equal to the expected value " port)
      (write (access-condition condition 'EXPECTED-DATUM) port)
      (write-string " in the sense of " port)
      (write (access-condition condition 'COMPARATOR) port)
      (write-string "." port))))

(define test-failure:compare-datum
  (let ((signaller
         (condition-signaller condition-type:test-failure:compare-datum
                              '(COMPARATOR EXPECTED-EXPRESSION EXPECTED-DATUM
                                           ACTUAL-EXPRESSION ACTUAL-DATUM)
                              standard-error-handler)))
    (named-lambda
        (test-failure:compare-datum comparator
                                    expected-expression expected-datum
                                    actual-expression actual-datum)
      (signaller comparator
                 expected-expression expected-datum
                 actual-expression actual-datum))))
