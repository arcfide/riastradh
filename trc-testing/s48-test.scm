;;; -*- Mode: Scheme; scheme48-package: simple-testing-parameters -*-

;;;; Testing Utility for Scheme
;;;; Scheme48 Parameters

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

(define $test-verbosity (make-fluid (make-cell 'NORMAL)))
(define $test-debug-errors? (make-fluid (make-cell #f)))

(define (test-verbosity) (fluid-cell-ref $test-verbosity))

(define (with-test-verbosity verbosity thunk)
  (fluid-bind (($test-verbosity (make-cell verbosity)))
    (thunk)))

(define (set-test-verbosity! verbosity)
  (fluid-cell-set! $test-verbosity verbosity))

(define (test-debug-errors?) (fluid-cell-ref $test-debug-errors?))

(define (with-test-debug-errors? flag thunk)
  (fluid-bind (($test-debug-errors? (make-cell flag)))
    (thunk)))

(define (set-test-debug-errors?! flag)
  (fluid-cell-set! $test-debug-errors? flag))

;;; Dynamic State

(define $test-name (make-fluid #f))
(define $test-description (make-fluid #f))

(define (test-name) (fluid $test-name))
(define (test-description) (fluid $test-description))

(define $testing-suite? (make-fluid #f))

(define $test-suite-passages (make-fluid #f))
(define $test-suite-failures (make-fluid #f))

(define (testing-suite?) (fluid $testing-suite?))

(define (record-test-passages count)
  (let ((cell (fluid $test-suite-passages)))
    (if (not (cell? cell))
        (error "Not currenty testing suite."))
    (cell-set! cell (+ (cell-ref cell) count))))

(define (record-test-failures failures)
  (let ((cell (fluid $test-suite-failures)))
    (if (not (cell? cell))
        (error "Not currenty testing suite."))
    (cell-set! cell (append-reverse failures (cell-ref cell)))))

(define (append-reverse list tail)
  (if (pair? list)
      (append-reverse (cdr list) (cons (car list) tail))
      tail))

;;;; Test Parameter Implementations

(define (with-test-case-run name description thunk)
  (with-test-notification name description thunk))

(define (with-test-suite-run name description thunk)
  (let ((passages-cell (make-cell 0))
        (failures-cell (make-cell '())))
    (with-test-notification name description
      (lambda ()
        (fluid-bind (($testing-suite? #t)
                     ($test-suite-passages passages-cell)
                     ($test-suite-failures failures-cell))
          (thunk))))
    (let ((passages (cell-ref passages-cell))
          (failures (cell-ref failures-cell)))
      (if (testing-suite?)
          (begin (record-test-passages passages)
                 (record-test-failures failures)
                 (if (not (eq? (test-verbosity) 'QUIET))
                     (report-test-suite passages failures)))
          (report-test-suite passages failures)))))

(define (niladic-test)
  (warn "Null test:" (test-name)))

(define (monadic-test thunk)
  (with-test-restarters #f
    (lambda ()
      (with-test-condition-handler #f thunk))))

(define (polyadic-test thunks)
  (with-test-restarters #f
    (lambda ()
      (do ((thunks thunks (cdr thunks))
           (index 0 (+ index 1)))
          ((not (pair? thunks)))
        (with-test-notification index #f
          (lambda ()
            (with-test-restarters index
              (lambda ()
                (with-test-condition-handler index (car thunks))))))))))

(define (component-test thunk)
  (thunk))

;;;; Test Reporting

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
  (fluid-bind (($test-name name)
               ($test-description description))
    (if (eq? (test-verbosity) 'QUIET)
        (thunk)
        (with-notification report-test thunk))))

(define (report-test port)
  (write-string "Test " port)
  (write (test-name) port)
  (if (and (test-description) (eq? (test-verbosity) 'VERBOSE))
      (begin
        (write-string ":" port)
        (write-notification-line
         (lambda (port)
           (write-string (test-description) port))))))

(define (report-test-failure index condition)
  index                                 ;ignore
  ;; This seems slightly strange, but the reason is that we want to be
  ;; sure to report what tests failed, and if the verbosity setting was
  ;; quiet, then it wouldn't have been reported before the failure.
  (if (eq? (test-verbosity) 'QUIET)
      (write-notification-line report-test))
  ;++ DISPLAY-CONDITION doesn't play nicely with the notification
  ;++ output utility.  Blah.
  (display-condition condition (notification-output-port)))

(define (test-restarter-description index verb)
  (string-append verb
                 (if index
                     (string-append " test #"
                                    (number->string index)
                                    " of ")
                     " testing ")
                 (call-with-string-output-port
                   (lambda (output-port)
                     (limited-write (test-name) output-port 5 3)))
                 "."))

;;;; Test Conditions

(define (with-test-restarters index thunk)
  (call-with-exiting-restarter 'ABORT
      (test-restarter-description index "Abort")
    (lambda (exit)
      (let loop ()
        (with-exiting-restarter 'RETRY
            (test-restarter-description index "Retry")
          (lambda ()
            (thunk)
            (restart exit)))
        (loop)))))

(define (with-test-condition-handler index thunk)
  (with-handler (lambda (condition propagate)
                  (if (testing-suite?)
                      (record-test-failures (list condition)))
                  (handle-test-failure index condition propagate))
    (lambda ()
      (thunk)
      (if (testing-suite?)
          (record-test-passages 1)))))

(define (handle-test-failure index condition propagate)
  (if (or (test-failure? condition)
          (and (error? condition)
               (not (test-debug-errors?))))
      (begin
        (report-test-failure index condition)
        (restart 'ABORT))
      (propagate)))

(define-condition-type 'TEST-FAILURE '(ERROR))

(define test-failure? (condition-predicate 'TEST-FAILURE))

(define (test-failure message . irritants)
  (signal-condition (make-condition 'TEST-FAILURE (cons message irritants))))

(define (test-failure:predicate-datum predicate expression datum)
  (test-failure "expression's value failed to satisfy predicate"
                predicate
                expression
                datum))

(define (test-failure:compare-datum comparator
                                    expected-expression expected-datum
                                    actual-expression actual-datum)
  (test-failure "expression's value failed comparison"
                `(EXPECTED ,expected-expression => ,expected-datum)
                `(GOT ,actual-expression => ,actual-datum)))

;;;; Nested Notification Utility

;;; This should be built into Scheme48.  It would be nice if ports also
;;; tracked their position by default.

(define $notification-output-port
  (make-fluid (make-tracking-output-port (current-output-port))))

(define $notification-level (make-fluid 0))

(define (notification-output-port)
  (fluid $notification-output-port))

(define (start-notification-line output-port)
  (fresh-line output-port)
  (write-char #\; output-port)
  (write-string (make-string (* 2 (fluid $notification-level))
                             #\space)
                output-port))

(define (write-notification-line message-writer)
  (let ((output-port (notification-output-port)))
    (start-notification-line output-port)
    (message-writer output-port)
    (force-output output-port)))

(define (with-notification message-writer thunk)
  (let ((output-port (notification-output-port)))
    (define (start-notification suffix)
      (start-notification-line output-port)
      (message-writer output-port)
      (write-string suffix output-port))
    (let ((row #f)
          (column #f))
      (dynamic-wind
        (lambda ()
          (start-notification "...")
          (force-output output-port)
          (set! row (current-row output-port))
          (set! column (current-column output-port)))
        (lambda ()
          (fluid-bind (($notification-level (+ (fluid $notification-level) 1)))
            (thunk)))
        (lambda ()
          (if (not (and row
                        column
                        (= row (current-row output-port))
                        (= column (current-column output-port))))
              (start-notification " --"))
          (write-string " done" output-port)
          (newline output-port)
          (force-output output-port))))))

;;;; Randomness

;;; This uses a very clever SYNTAX-RULES trick to conveniently expand
;;; to an application of the LET-FLUIDS procedure.  FLUID-BIND exists
;;; only to check the syntax; %FLUID-BIND is where the real trick
;;; happens.

(define-syntax fluid-bind
  (syntax-rules ()
    ((FLUID-BIND ((fluid value)) body0 body1+ ...)
     (LET-FLUID fluid value (LAMBDA () body0 body1+ ...)))
    ((FLUID-BIND ((fluid value) ...) body0 body1+ ...)
     (%FLUID-BIND ((fluid value) ...) body0 body1+ ...))))

(define-syntax %fluid-bind
  (syntax-rules ()
    ((%FLUID-BIND ((argument ...) ...) body0 body1+ ...)
     (LET-FLUIDS argument ... ... (LAMBDA () body0 body1+ ...)))))

;;; Local Variables:
;;; Eval: (put 'fluid-bind 'scheme-indent-function 1)
;;; End:
