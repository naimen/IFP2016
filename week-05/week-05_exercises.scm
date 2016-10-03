;;; week-05_exercises.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 26 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-05_exercises.html

;;;;;;;;;;
;;; Predicates
(define is-leaf?
  number?)

(define is-node?
  pair?)

;;; Accessors
(define node-1
  (lambda (v)
    (car v)))
(define node-2
  (lambda (v)
    (cdr v)))

(define fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (if (null? ws)
                            nil-case
                            (cons-case (car ws)
                                       (visit (cdr ws)))))])
        (visit vs)))))

(define Dutch-flag
  (lambda (xs x)
    (letrec ([visit (lambda (xs)
                      (cond
                        ;; If xs is null, then there are no more elements to consider
                        [(null? xs)
                         (list '() 0 '())]
                        ;; If the current element is smaller than x, then add it to the first list, in front of what was returned by visit
                        [(< (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (cons (car xs) (list-ref res 0)) (list-ref res 1) (list-ref res 2)))]
                        ;; If the current element is equal to x, then add 1 to the middle element that was returned from visit
                        [(= (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (list-ref res 0) (+ 1 (list-ref res 1)) (list-ref res 2)))]
                        ;; If the current element is larger than x (which I don't actually need to check, since it is not smaller than or equal),
                        ;; then add it to the last list, in front of what was returned by visit
                        [(> (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (list-ref res 0) (list-ref res 1) (cons (car xs) (list-ref res 2))))]))])
      (visit xs))))

(define Dutch-flag_fold-right
  (lambda (xs_init x_init)
    ((fold-right_proper-list
      ;; If xs is null, then there are no more elements to consider
      (list '() 0 '())
      (lambda (x res)
        (cond
          ;; If the current element is smaller than x, then add it to the first list, in front of what was returned by visit
          [(< x x_init)
           (list (cons x (list-ref res 0)) (list-ref res 1) (list-ref res 2))]
          ;; If the current element is equal to x, then add 1 to the middle element that was returned from visit
          [(= x x_init)
           (list (list-ref res 0) (+ 1 (list-ref res 1)) (list-ref res 2))]
          ;; If the current element is larger than x (which I don't actually need to check, since it is not smaller than or equal),
          ;; then add it to the last list, in front of what was returned by visit
          [(> x x_init)
           (list (list-ref res 0) (list-ref res 1) (cons x (list-ref res 2)))])))
     xs_init)))

(define test-Dutch-flag
  (lambda (candidate)
    (and (equal? (candidate '() 10)
                 '(() 0 ()))
         (equal? (candidate '(1 2 3) 10)
                 '((1 2 3) 0 ()))
         (equal? (candidate '(10 10 10) 10)
                 '(() 3 ()))
         (equal? (candidate '(100 200 300) 10)
                 '(() 0 (100 200 300)))
         (equal? (candidate '(1 2 3 10 100 200 300) 10)
                 '((1 2 3) 1 (100 200 300)))
         (equal? (candidate '(10 1 300 3 10 2 100 10 200 1 10) 10)
                 '((1 3 2 1) 4 (300 100 200)))
		 ;; Tests for something other than 10:
         (equal? (candidate '(1 2 3 2 1 4 5 6 5 4) 4)
                 '((1 2 3 2 1) 2 (5 6 5)))
         ;;;
         )))

(unless (test-Dutch-flag Dutch-flag)
  (printf "Dutch-flag does not work"))

(unless (test-Dutch-flag Dutch-flag_fold-right)
  (printf "Dutch-flag_fold-right does not work"))

;;;;;;;;;;
(define test-lengths-of-all-distinct-paths
  (lambda (candidate)
    (and (equal? (candidate (cons 1 2))
                 '(0 1 1))
         (equal? (candidate (cons 3
                                  (cons 1 2)))
                 '(0 1 1 2 2))
         (equal? (candidate (cons (cons 1 2)
                                  (cons 3 4)))
                 '(0 1 2 2 1 2 2))
         (equal? (candidate (cons (cons 1 2)
                                  (cons 3
                                        (cons 4 5))))
                 '(0 1 2 2 1 2 2 3 3))
         (equal? (candidate (cons (cons (cons (cons 1 2) 3) 4) 5))
                 '(0 1 2 3 4 4 3 2 1))
         )))


;;;;;;;;;;

(define lengths-of-all-distinct-paths
  (lambda (v_init)
    (letrec ([visit (lambda (v n)
                      (cons n
                            (cond
                              [(is-leaf? v)
                               '()]
                              [(is-node? v)
                               (append (visit (node-1 v) (1+ n))
                                       (visit (node-2 v) (1+ n)))]
                              [else
                               (errorf 'lengths-of-all-distinct-paths
                                       "not a binary tree: ~s"
                                       v)])))])
      (visit v_init 0))))
(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths)
  (print "haha nice try"))

(define lengths-of-all-distinct-paths-alt
  (lambda (v_init)
    (letrec ([visit (trace-lambda visit (v n a)
                      (cons n
                            (cond
                              [(is-leaf? v)
                               '()]
                              [(is-node? v)
                               (visit (node-1 v) (1+ n)
                                      (visit (node-2 v) (1+ n) a))
                              ]
                              [else
                               (errorf 'lengths-of-all-distinct-paths
                                       "not a binary tree: ~s"
                                       v)])))])
      (visit v_init 0 '()))))
                              

(define lengths-of-all-distinct-paths_alt
  (trace-lambda entering (v_init)
	(letrec ([visit (trace-lambda visit (v n a)
					  (cond
						[(is-leaf? v)
						 (cons n a)]
						[(is-node? v)
						 (visit (node-2 v) (1+ n)
								(visit (node-1 v) (1+ n)
									   (cons n a)))]
						[else
						  (errorf 'lengths-of-all-distinct-paths_alt
								  "not a binary tree: ~s"
								  v)]))])
	  (reverse (visit v_init 0 '())))))
(unless (test-lengths-of-all-distinct-paths lengths-of-all-distinct-paths_alt)
  (print "lengths-of-all-distinct-paths_alt does not work"))
;;;;;;;;;;

(define test-run-length
  (lambda (candidate)
    (and (equal? (candidate '())
                 '())
         (equal? (candidate '(a))
                 '((a . 1)))
         (equal? (candidate '(a b b))
                 '((a . 1) (b . 2)))
         (equal? (candidate '(a b b c c c))
                 '((a . 1) (b . 2) (c . 3)))
         (equal? (candidate '(a b b c c c a a a a a a))
                 '((a . 1) (b . 2) (c . 3) (a . 6)))
         ;;; etc.
         )))

;;;;;;;;;;

;;; end of TFP-exercises-week-08.scm

"TFP-exercises-week-08.scm"
