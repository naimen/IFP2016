;;; IFP-exercises-week-06.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus Shuo <danvy@cs.au.dk>
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
  (lambda (nil-case cons-case . rest)
    (let ([else-case (cond
                       [(null? rest)
                        (lambda (v)
                          (errorf 'fold-right_proper-list
                                  "not a proper list: ~s"
                                  v))]
                       [(null? (cdr rest))
                        (car rest)]
                       [else
                        (errorf 'fold-right_proper-list
                                "too many parameters: ~s"
                                rest)])])
      (lambda (vs)
        (letrec ([visit (lambda (ws)
                          (cond
                            [(null? ws)
                             nil-case]
                            [(pair? ws)
                             (cons-case (car ws)
                                        (visit (cdr ws)))]
                            [else
                             (else-case ws)]))])
          (visit vs))))))

(define Dutch-flag
  (lambda (xs x)
    (letrec ([visit (lambda (xs)
                      (cond
;; If xs is null, then there are no more elements to consider
                        [(null? xs)
                         (list '() 0 '())]
;; If the current element is smaller than x, then add it to the first list,
;;  in front of what was returned by visit
                        [(< (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (cons (car xs) (list-ref res 0))
                                 (list-ref res 1)
                                 (list-ref res 2)))]
;; If the current element is equal to x,
;;  then add 1 to the middle element that was returned from visit
                        [(= (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (list-ref res 0)
                                 (+ 1 (list-ref res 1))
                                 (list-ref res 2)))]
;; If the current element is larger than x
;; (which I don't actually need to check, since it is not smaller than or equal)
;;  then add it to the last list, in front of what was returned by visit
                        [(> (car xs) x)
                         (let ([res (visit (cdr xs))])
                           (list (list-ref res 0)
                                 (list-ref res 1)
                                 (cons (car xs) (list-ref res 2))))]))])
      (visit xs))))


(define Dutch-flag_multiple-values
  (lambda (xs x)
    (letrec ([visit (lambda (xs)
                      (cond
                        ;; If xs is null,
                        ;;  then there are no more elements to consider
                        [(null? xs)
                         (values '() 0 '())]
                        ;; If the current element is smaller than x,
                        ;;  then add it to the first list,
                        ;;  in front of what was returned by visit
                        [(< (car xs) x)
                         (let-values ([(res0 res1 res2) (visit (cdr xs))])
                           (values (cons (car xs) res0)
                                   res1
                                   res2))]
                        ;; If the current element is equal to x,
                        ;;  then add 1 to the middle element that was
                        ;;  returned from visit
                        [(= (car xs) x)
                         (let-values ([(res0 res1 res2) (visit (cdr xs))])
                           (values res0
                                   (+ 1 res1)
                                   res2))]
                        ;; If the current element is larger than x
                        ;;  (which I don't actually need to check,
                        ;;  since it is not smaller than or equal)
                        ;;  then add it to the last list,
                        ;;  in front of what was returned by visit
                        [(> (car xs) x)
                         (let-values ([(res0 res1 res2) (visit (cdr xs))])
                           (values res0
                                   res1
                                   (cons (car xs) res2)))]))])
      (let-values ([(res0 res1 res2) (visit xs)])
        (list res0 res1 res2)))))

(define Dutch-flag_fold-right
  (lambda (xs_init x_init)
    ((fold-right_proper-list
;; If xs is null, then there are no more elements to consider
      (list '() 0 '())
      (lambda (x res)
        (cond
;; If the current element is smaller than x,
;;  then add it to the first list, in front of what was returned by visit
          [(< x x_init)
           (list (cons x (list-ref res 0))
                 (list-ref res 1)
                 (list-ref res 2))]
;; If the current element is equal to x,
;;  then add 1 to the middle element that was returned from visit
          [(= x x_init)
           (list (list-ref res 0)
                 (+ 1 (list-ref res 1))
                 (list-ref res 2))]
;; If the current element is larger than x
;;(which I don't actually need to check, since it is not smaller than or equal),
;; then add it to the last list, in front of what was returned by visit
          [(> x x_init)
           (list (list-ref res 0)
                 (list-ref res 1)
                 (cons x (list-ref res 2)))])))
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

(unless (test-Dutch-flag Dutch-flag_multiple-values)
  (printf "Dutch-flag_multiple-values does not work"))

;;;;;;;;;;;

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
         (equal? (candidate '(a b c a b c))
                 '((a . 1) (b . 1) (c . 1) (a . 1) (b . 1) (c . 1)))
;;; This extra unit test has been added to make sure it works
;;  when more than one symbol is repeated a different places in the string.
         (equal? (candidate '(cat dog dog bee))
                 '((cat . 1) (dog . 2) (bee . 1)))
;;; This test is added to check if it works
;; for symbols made up of more than one character.
         )))
;;; Apart  from this, we believe that these unit tests already cover
;;  the potential problems fairly well,
;;  and that it would be redundant to add any more.
         

;;;
(define run-length
  (lambda (xs)
    (letrec ([visit (lambda (xs)
                      (cond 
                        [(null? xs)
                         '()]
                        [(and (pair? xs) (symbol? (car xs))) 
                         (let ([res (visit (cdr xs))]) 
                           (if (and (pair? res) 
                                    (pair? (car res))
                                    (equal? (caar res) (car xs)))
                               (cons (cons (car (car res))
                                           (1+ (cdr (car res)))) (cdr res))
                               (cons (cons (car xs) 1) res)))]
                        [else
                         (errorf 'run-length
                                 "not a proper list of symbols: ~s"
                                 xs)]))])
      (visit xs))))
(unless (test-run-length run-length)
  (printf "run-length does not work"))

(define run-length_acc
  (lambda (xs)
  ;(trace-lambda entering (xs)
	(letrec ([visit (lambda (xs a b)
    ;(letrec ([visit (trace-lambda visit (xs a)
                      (cond 
                        [(null? xs)
                         (if (not (null? a))
						    (cons (cons a b) '())
							'()) ]
                        [(and (pair? xs) (symbol? (car xs))) 
						 (if (and (not (null? a))
								  (equal? a (car xs)))
						   (visit (cdr xs) a (+ 1 b))
						   (if (not (null? a))
							 (cons (cons a b) (visit (cdr xs) (car xs) 1))
							 (visit (cdr xs) (car xs) 1))) ]
                        [else
                         (errorf 'run-length_acc
                                 "not a proper list of symbols: ~s"
                                 xs)]))])
      (visit xs '() '()))))
(unless (test-run-length run-length_acc)
  (printf "run-length_acc does not work"))

;;; This implementation is made with the idea that it always returns
;;  three values, the last seen symbol, the number of times it has
;;  been seen in a row and the resulting list.
;;  The problem with this implementation is that returning three values
;;  right from the beginning means that it will also add (() . 0) to
;;  the list.
;;  To solve this, we have added a conditional that ignores this entry.
;;  This though leads to the problem where it will not recognize the
;;  input being only the empty string.
;;  To solve this we have added another condition before the first call
;;  of the recursive function that will simply return '() if the input
;;; is the empty list.
(define run-length-multiple-values
  (lambda (xs)
    (letrec ([visit (lambda (xs)
                      (cond
                        [(null? xs)
                         (values '() 0 '())
                         ]
                        [(and (pair? xs) (symbol? (car xs)))
                         (let-values ([(current count res) (visit (cdr xs))])
                           (if (and (pair? res)
                                    (pair? (car res))
                                    (equal? current (car xs)))
                               (values current (+ 1 count) res)
                               (cond 
                                 [(equal? current '())
                                  (values (car xs) 1 res)] ;Special case
                                 [(equal? current (car xs))
                                  (values current (+ 1 count) res)] 
                                 [else 
                                  (values (car xs) 1 (cons (cons current
                                                                 count)
                                                           res))])))]
                        [else
                         (errorf 'run-length-multiple-values
                                 "not a proper list of symbols: ~s"
                                 xs)]))])
      (let-values ([(v1 v2 v3) (visit xs)])
        (if (null? xs)
            '() ;Special case if the input is an empty string.
            (cons (cons v1 v2) v3))))))

(unless (test-run-length run-length-multiple-values)
  (printf "run-length-multiple-values does not work"))

(define run-length_fold-right
  (fold-right_proper-list
   '()
   (lambda (x res)
     (if (symbol? x)
         (if (and (pair? res)
                  (pair? (car res))
                  (equal? (caar res) x))
             (cons (cons (car (car res))
                         (1+ (cdr (car res))))
                   (cdr res))
             (cons (cons x 1) res))
         (errorf 'run-length
                 "not a symbol: ~s"
                 x)))))

(unless (test-run-length run-length_fold-right)
  (printf "run-length_fold-right does not work"))
;;;;

;;;;;;;;;;

;;; end of IFP-exercises-week-06.scm

"IFP-exercises-week-06.scm"
