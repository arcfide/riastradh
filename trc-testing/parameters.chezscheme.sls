;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <Description of File>
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

#!chezscheme

(library (riastradh trc-testing parameters)
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

    ;; Special Parameter for choosing output
    notification-output-port

    ;; Enable restarting
    restart)
  (import (scheme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported names, procedures, &c.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Basic Customization Parameters

(define test-verbosity (make-parameter 'normal))

(define with-test-verbosity
  (lambda (verbosity thunk)
    (parameterize ([test-verbosity verbosity])
      (thunk))))

(define set-test-verbosity!
  (lambda (verbosity)
    (test-verbosity verbosity)))

(define test-debug-errors? (make-parameter #f))

(define with-test-debug-errors?
  (lambda (flag thunk)
    (parameterize ([test-debug-errors? flag])
      (thunk))))

(define set-test-debug-errors?!
  (lambda (flag)
    (test-debug-errors? flag)))

;;; Different types of tests

(define niladic-test
  (lambda ()
    (warningf "Null test: ~" (test-name))))

(define monadic-test
  (lambda (thunk)
    (with-test-restarters #f
      (lambda ()
        (with-test-condition-handler #f thunk)))))

(define polyadic-test 
  (lambda (thunks)
    (with-test-restarters #f
      (do ([thunks thunks (cdr thunks)]
           [index 0 (+ index 1)])
          [(null? thunks)]
        (with-test-notification index #f
          (lambda ()
            (with-test-restarters index
              (lambda ()
                (with-test-condition-handler index (car thunks))))))))))

(define component-test
  (lambda (thunk)
    (thunk)))

;;; Test Failures

(define-condition-type &test-failure &error 
  make-test-failure test-failure?)

(define test-failure
  (lambda (message . irritants)
    (raise-continuable 
      (condition
        (make-test-failure)
        (make-message-condition message)
        (make-irritants-condition irritants)))))

(define test-failure:predicate-datum
  (lambda (predicate expression datum)
    (test-failure "expression's value failed to satisfy predicate"
      predicate expression datum)))

(define test-failure:compare-datum
  (lambda (comparator eexp edat aexp adat)
    (test-failure "expression's value failed comparison"
      `(expected ,eexp => ,edat)
      `(got ,aexp => ,adat))))

;;; Running tests

(define with-test-case-run 
  (lambda (name desc thunk)
    (with-test-notification name desc thunk)))

(define with-test-suite-run
  (lambda (name description thunk)
    (let-values
      ([(passages failures)
        (with-test-notification name description
          (lambda ()
            (parameterize ([testing-suite? #t]
                           [test-suite-passages 0]
                           [test-suite-failures '()])
              (thunk)
              (values (test-suite-passages) (test-suite-failures)))))])
      (if (testing-suite?)
          (begin (record-test-passages passages)
                 (record-test-failures failures)
                 (unless (eq? (test-verbosity) 'quiet)
                   (report-test-suite passages failures)))
          (report-test-suite passages failures)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxilary and Suppport procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Internal State

(define test-name (make-parameter #f))
(define test-description (make-parameter #f))
(define testing-suite? (make-parameter #f))
(define test-suite-passages (make-parameter #f))
(define test-suite-failures (make-parameter #f))

(define record-test-passages
  (lambda (count)
    (let ([val (test-suite-passages)])
      (if val
          (test-suite-passages (+ val count))
          (error #f "Not currenty testing suite")))))

(define record-test-failures
  (lambda (failures)
    (let ([val (test-suite-failures)])
      (if val
          (test-suite-failures (append-reverse failures val))
          (error #f "Not currenty testing suite.")))))

(define append-reverse
  (lambda (list tail)
    (if (pair? list)
        (append-reverse (cdr list) (cons (car list) tail))
        tail)))

;;; Test Reporting

(define report-test-suite
  (lambda (passages failures)
    (write-notification-line
      (lambda (port)
        (let* ([failures (length failures)]
               [total (+ passages failures)])
          (format port "~a tests, ~a passed (~a%), ~a failed (~a%)~%"
            total
            passages (round (* 100 (/ passages total)))
            failures (round (* 100 (/ passages total)))))))))

(define with-test-notification
  (lambda (name description thunk)
    (parameterize ([test-name name]
                   [test-description description])
      (if (eq? (test-verbosity) 'quiet)
          (thunk)
          (with-notification report-test thunk)))))

(define report-test 
  (lambda (port)
    (display "Test " port)
    (write (test-name) port)
    (if (and (test-description) (eq? (test-verbosity) 'verbose))
        (begin
          (display ": " port)
          (write-notification-line
            (lambda (port)
              (display (test-description) port)))))))

(define report-test-failure
  (lambda (index condition)
    (when (eq? (test-verbosity) 'quiet)
      (write-notification-line report-test))
    (display-condition condition (notification-output-port))))

(define restart-reporter
  (lambda (index verb)
    (format (notification-output-port)
      "~a ~:[testing~;test #~a~] ~a." 
      verb index index (test-name))))

;;; Test Conditions

(define restart-handler (make-parameter void))
(define restart (lambda () ((restart-handler))))

(define with-test-restarters
  (lambda (index thunk)
    (call-with-current-continuation
      (lambda (exit)
        (parameterize ([reset-handler
                        (lambda () 
                          (restart-reporter index "Abort")
                          (exit))])
          (let loop ()
            (call-with-current-continuation
              (lambda (restart)
                (parameterize 
                  ([restart-handler
                    (lambda () (restart-reporter index "Retry")
                      (restart))])
                  (thunk)
                  (exit))))
            (loop)))))))

(define with-test-condition-handler
  (lambda (index thunk)
    (with-exception-handler
      (lambda (condition)
        (when (testing-suite?) (record-test-failures (list condition)))
        (call/cc (lambda (k) (handle-test-failure index condition k))))
      (lambda ()
        (thunk)
        (when (testing-suite?) (record-test-passages 1))))))

(define handle-test-failure
  (lambda (index condition continue)
    (if (or (test-failure? condition)  
            (and (error? condition)
                 (not (test-debug-errors?))))
        (begin (report-test-failure index condition)
          (reset))
        (parameterize ([reset-handler void]) 
          (debug-message-and-continuation 
            (with-output-to-string (lambda () (display-condition condition)))
            continue)
          (display-condition condition)
          (printf "~%Type (debug) to enter debugger.~%")
          (reset)))))

;;; Handling notifications

(define notification-output-port (make-parameter (current-output-port)))
(define notification-level (make-parameter 0))

(define write-notification-line
  (lambda (message-writer)
    (let ([port (notification-output-port)])
      (start-notification-line port)
      (message-writer port)
      (flush-output-port port))))

(define with-notification
  (lambda (message-writer thunk)
    (let ([output-port (notification-output-port)])
      (define start-notification
        (lambda (suffix)
          (start-notification-line output-port)
          (message-writer output-port)
          (display suffix output-port)))
      (dynamic-wind
        (lambda ()
          (start-notification "...")
          (flush-output-port output-port))
        (lambda ()
          (parameterize ([notification-level (1+ (notification-level))])
            (thunk)))
        (lambda ()
          (start-notification " --")
          (display " done" output-port)
          (newline output-port)
          (flush-output-port output-port))))))

(define start-notification-line
  (lambda (output-port)
    (format output-port "~%;~v,1t" (* 2 (notification-level)))))

)