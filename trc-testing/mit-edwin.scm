;;; -*- Mode: Scheme -*-

;;;; Testing Utility for Scheme
;;;; Edwin Utilities

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

(declare (usual-integrations))

(define-command eval-test
  "Run the test defined at the point.
Test cases are evaluated; test suites are not."
  ()
  (lambda ()
    (process-test-command extract-and-run-test-suite
                          (lambda (suite-region)
                            ((ref-command eval-defun))
                            (extract-and-run-test-case suite-region)))))

(define-command run-test
  "Run the test defined at the point.
Neither test suites nor test cases are evaluated."
  ()
  (lambda ()
    (process-test-command extract-and-run-test-suite
                          extract-and-run-test-case)))

(define (process-test-command if-suite if-case)
  (define (lose)
    (editor-failure "Point is not on a valid test definition."))
  (cond ((match-test-definition (current-point))
         => (lambda (type.region)
              (let ((type (car type.region)) (region (cdr type.region)))
                (case type
                  ((SUITE) (if-suite region))
                  ((CASE) (if-case region))
                  (else (lose))))))
        (else (lose))))

(define (test-definition-rexp)
  (rexp-sequence (rexp-line-start)
                 "(define-test-"
                 (rexp-group (rexp-alternatives "case" "suite"))
                 (rexp-syntax-char 'WHITESPACE)))

(define (test-definition-regexp)
  (rexp->regexp (test-definition-rexp)))

(define (match-test-definition point)
  (cond ((re-match-forward (test-definition-regexp)
                           (this-definition-start point)
                           (buffer-end (mark-buffer point))
                           #t)          ;Do case-fold
         => (lambda (mark)
              (cons (intern
                     (extract-string (re-match-start 1) (re-match-end 1)))
                    (let* ((suite-end (forward-sexp mark 1 'ERROR))
                           (suite-start (backward-sexp suite-end 1)))
                      (make-region suite-start suite-end)))))
        (else #f)))

(define (extract-and-run-test-suite suite-region)
  (call-with-current-continuation
    (lambda (exit)
      (let ((suite-name (extract-test-name "suite" suite-region exit)))
        (cond ((symbol? suite-name)
               (editor-run-test-suite suite-name))
              ((and (pair? suite-name)
                    (symbol? (car suite-name)))
               (editor-run-test-suite (car suite-name)))
              (else
               (editor-failure "Can't identify a test suite name.")))))))

(define (extract-and-run-test-case suite-region)
  (call-with-current-continuation
    (lambda (exit)
      (let* ((case-end (forward-sexp (region-end suite-region) 1 'ERROR))
             (case-start (backward-sexp case-end 1)))
        (editor-run-test-case
         (extract-test-name "suite" suite-region exit)
         (extract-test-name "case" (make-region case-start case-end) exit))))))

(define (extract-test-name type region exit)
  (bind-condition-handler (list condition-type:error)
      (lambda (condition)
        condition                       ;ignore
        (editor-failure "Can't identify a test " type " name.")
        (exit unspecific))
    (lambda ()
      (car (read-expressions-from-region region
                                         (evaluation-environment
                                          (mark-buffer (region-start region))
                                          #f))))))

(define (editor-run-test-suite suite-name)
  (eval-test-expression (current-buffer)
                        `(RUN-TEST ,suite-name)
                        (evaluation-environment (current-buffer) #f)))

(define (editor-run-test-case suite-name case-name)
  (eval-test-expression
   (current-buffer)
   `(RUN-TEST (FIND-TEST ,suite-name ',case-name))
   (evaluation-environment (current-buffer) #f)))

(define (eval-test-expression buffer expression environment)
  (call-with-output-to-temporary-buffer "*test-output*"
      '(FLUSH-ON-SPACE SHRINK-WINDOW)
    (lambda (output-port)
      (let ((thunk (editor-eval buffer `(LAMBDA () ,expression) environment)))
        (with-notification-output-port output-port
          (lambda ()
            (with-output-to-port output-port
              thunk)))))))

;;; Edwin Variables:
;;; scheme-environment: '(edwin)
;;; End:
