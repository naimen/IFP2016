;;; week-05_exercises.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 26 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-05_exercises.html

;;;;;;;;;;

(define Dutch-flag
  (lambda (xs x)
	(letrec ([visit (lambda (xs x)
					  (cond
						[(null? xs)
						 (list '() 0 '())]
						[(< (car xs) x)
						 (let ([res (visit (cdr xs) x)])
						   (list (cons (car xs) (list-ref res 0)) (list-ref res 1) (list-ref res 2)))]
						[(= (car xs) x)
						 (let ([res (visit (cdr xs) x)])
						   (list (list-ref res 0) (+ 1 (list-ref res 1)) (list-ref res 2)))]
						[(> (car xs) x)
						 (let ([res (visit (cdr xs) x)])
						   (list (list-ref res 0) (list-ref res 1) (cons (car xs) (list-ref res 2))))]))])
	  (visit xs x))))


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
		 (equal? (candidate '(1 2 3 2 1 4 5 6 5 4) 4)
				 '((1 2 3 2 1) 2 (5 6 5)))
		 (equal? (candidate '(1 2 3 4 5 4 3 2 1 2 3 4 5 4 3 2 1) 3)
				 '((1 2 2 1 2 2 1) 4 (4 5 4 4 5 4)))
         ;;;
         )))

(unless (test-Dutch-flag Dutch-flag)
  (printf "Dutch-flag does not work"))

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
         (equal? (candidate '(a b c a b c))
                 '((a . 1) (b . 1) (c . 1) (a . 1) (b . 1) (c . 1)))
         ;;; This extra unit test has been added to make sure it works when more than one symbol is repeated a different places in the string.
         (equal? (candidate '(cat dog dog bee))
                 '((cat . 1) (dog . 2) (bee . 1)))
         ;;; This test is added to check if it works for symbols made up of more than one character.
         
         ;;; Apart  from this, we believe that these unit tests already cover the potential problems fairly well, and that it would be redundant to add any more.
         ;;; The only test that would make sense is one that checks to see that it raises an error if the input is not a proper list, or if it contains something other than symbols. 
         ;;; It doesn't seem like this is immediately possible in scheme though, so for now these tests will have to do.
         )))

;;;;;;;;;;

;;; end of TFP-exercises-week-08.scm

"TFP-exercises-week-08.scm"
