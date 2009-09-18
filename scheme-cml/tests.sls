(library (riastradh scheme-cml tests)
	(export run-tests channel-test-1 channel-test-2)
	(import 
		(except (rnrs base) error)
		(srfi :23)
		(rnrs sorting)
		(rnrs control)
		(only (chezscheme) iota)
		(riastradh scheme-cml rendezvous)
		(riastradh scheme-cml primitives)
		(riastradh scheme-cml channels)
		(riastradh scheme-cml threading)
		(riastradh scheme-cml misc))

(define (sort-list l p) (list-sort p l))

;;; Shamelessly cribbed from Scheme48's cml-tests.scm.  Optimized to
;;; use linearithmic rather than quadratic tests at each iteration,
;;; making it run immensely faster.

(define (channel-test-1)
  (let ((foo (iota 2000)))
    (do ((i 0 (+ 1 i)))
        ((= i 100))
      (let ((channel (make-channel)))
        (spawn
         (lambda ()
           (let loop ((i 0))
             (if (not (= i 1000))
                 (begin
                   (channel-send channel i)
                   (loop (+ 1 i))))))
         'producer-a)
        (spawn
         (lambda ()
           ;; (sleep 500)
           (let loop ((i 1000))
             (if (not (= i 2000))
                 (begin
                   (channel-send channel i)
                   (loop (+ 1 i))))))
         'producer-b)
    
        (let loop ((count 0)
                   (values '()))
          (if (= count 2000)
              (if (not (equal? (sort-list values <) foo))
                  (error "Lose!" values))
              (loop (+ 1 count)
                    (cons (channel-receive channel) values))))))))

(define (channel-test-2)
  (let ((foo (iota 2000)))
    (do ((i 0 (+ i 1)))
        ((= i 100))
      (let ((channel-1 (make-channel))
            (channel-2 (make-channel)))
        (spawn
         (lambda ()
           (let loop ((i 0))
             (if (not (= i 1000))
                 (begin
                   (channel-send channel-1 i)
                   ;;(sleep 1000)
                   (loop (+ 1 i))))))
         'channel-1)
        (spawn
         (lambda ()
           ;; (sleep 500)
           (let loop ((i 1000))
             (if (not (= i 2000))
                 (begin
                   (channel-send channel-2 i)
                   (loop (+ 1 i))))))
         'channel-2)
        (sleep 500)
        (let loop ((count 0)
                   (values '()))
          (if (= count 2000)
              (if (not (equal? (sort-list values <) foo))
                  (error "Lose!"))
              (loop (+ 1 count)
                    (cons (synchronize-chosen-rendezvous
                           (channel-receive-rendezvous channel-1)
                           (channel-receive-rendezvous channel-2))
                          values))))))))

(define (run-tests)
	(channel-test-1)
	(channel-test-2))

)