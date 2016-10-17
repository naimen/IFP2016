;;; week-06_evaluation-and-reification.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 04 Oct 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-06_evaluation-and-reification.html

;;;;;;;;;;;;;;;;;;;;

;;; The usual auxiliary procedures for the unit tests:

(define try-candidate
  (lambda (name candidate expected-output . input)
    (or (equal? expected-output
                (apply candidate input))
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

;;;;;;;;;;;;;;;;;;;;

;;; The simplest implementation of reification

(define reify-first-order-value
  (lambda (v)
    `',v))    ;;; i.e., (list 'quote v)

;;;;;;;;;;

;;; A structural approach to reification

(define reify-number-defensively
  (lambda (v)
    (if (number? v)
        v
        (errorf 'reify-number
                "not a number: ~s"
                v))))

(define reify-number
  (lambda (n)
    n))

(define reify-boolean-defensively
  (lambda (v)
    (if (boolean? v)
        v
        (errorf 'reify-boolean-defensively
                "not a boolean: ~s"
                v))))

(define reify-boolean
  (lambda (b)
    b))

(define reify-char-defensively
  (lambda (v)
    (if (char? v)
        v
        (errorf 'reify-char-defensively
                "not a char: ~s"
                v))))

(define reify-char
  (lambda (c)
    c))

(define reify-string-defensively
  (lambda (v)
    (if (string? v)
        v
        (errorf 'reify-string-defensively
                "not a string: ~s"
                v))))

(define reify-string
  (lambda (s)
    s))

(define reify-symbol-defensively
  (lambda (v)
    (if (symbol? v)
        `',v
        (errorf 'reify-symbol-defensively
                "not a symbol: ~s"
                v))))

(define reify-symbol
  (lambda (x)
    `',x))

(define reify-the-empty-list-defensively
  (lambda (v)
    (if (null? v)
        `',v
        (errorf 'reify-the-empty-list-defensively
                "not the empty list: ~s"
                v))))

(define reify-the-empty-list
  (lambda (nil)
    `',nil))

(define reify-pair-of-symbols-and-numbers-defensively
  (lambda (v)
    (if (pair? v)
        `(cons ,(reify-symbol-defensively (car v))
               ,(reify-number-defensively (cdr v)))
        (errorf 'reify-pair-of-symbols-and-numbers-defensively
                "not a pair: ~s"
                v))))

(define reify-pair-of-symbols-and-numbers
  (lambda (p)
    `(cons ,(reify-symbol (car p))
           ,(reify-number (cdr p)))))

;;;;;;;;;;;;;;;;;;;;

(define alist-mt
  '())

(define alist-extend
  (lambda (name denotable environment)
    (cons (cons name denotable)
          environment)))

(define alist-lookup
  (lambda (name environment found not-found)
    (letrec ([visit (lambda (e)
                      (if (null? e)
                          (not-found)
                          (let ([binding (car e)])
                            (if (equal? name (car binding))
                                (found (cdr binding))
                                (visit (cdr e))))))])
      (visit environment))))

(define test-reify-environment-binding-symbols-to-numbers
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z 30 alist-mt)
                            '((z . 30)))
             (try-candidate name
                            candidate
                            '(alist-extend 'y 20 (alist-extend 'z 30 alist-mt))
                            '((y . 20) (z . 30)))
             (try-candidate name
                            candidate
                            '(alist-extend 'x 10
                                           (alist-extend 'y 20
                                                         (alist-extend 'z 30
                                                                       alist-mt
                                                                       )))
                            '((x . 10) (y . 20) (z . 30)))
               ;;; etc.
             )))

(define test-reify-environment-binding-symbols-to-strings
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z "30" alist-mt)
                            '((z . "30")))
             (try-candidate name
                            candidate
                            '(alist-extend 'y "20" (alist-extend 'z "30" alist-mt))
                            '((y . "20") (z . "30")))
             (try-candidate name
                            candidate
                            '(alist-extend 'x "10" (alist-extend 'y "20" (alist-extend 'z "30" alist-mt)))
                            '((x . "10") (y . "20") (z . "30")))
               ;;; etc.
             )))

(define reify-environment
  (lambda (reify-name reify-denotation)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(null? v)
                           'alist-mt]
                          [(pair? v)
                           (let ([binding (car v)])
                             (if (pair? binding)
                                 `(alist-extend ,(reify-name (car binding))
                                                ,(reify-denotation (cdr binding))
                                                ,(visit (cdr v)))
                                 (errorf 'reify-environment
                                         "not a binding: ~s"
                                         binding)))]
                          [else
                           (errorf 'reify-environment
                                   "not an environment: ~s"
                                   v)]))])
        (visit v_init)))))

(define reify-environment-binding-symbols-to-numbers
  (reify-environment reify-symbol reify-number))

(unless (test-reify-environment-binding-symbols-to-numbers 'reify-environment-binding-symbols-to-numbers reify-environment-binding-symbols-to-numbers)
  (printf "(test-reify-environment-binding-symbols-to-numbers 'reify-environment-binding-symbols-to-numbers reify-environment-binding-symbols-to-numbers) failed~n"))

(define reify-environment-binding-symbols-to-strings
  (reify-environment reify-symbol reify-string))

(unless (test-reify-environment-binding-symbols-to-strings 'reify-environment-binding-symbols-to-strings reify-environment-binding-symbols-to-strings)
  (printf "(test-reify-environment-binding-symbols-to-strings 'reify-environment-binding-symbols-to-strings reify-environment-binding-symbols-to-strings) failed~n"))

;;;;;;;;;;

(define test-reify-environment-binding-symbols-to-symbols
  (lambda (name candidate)
    (and-all (try-candidate name
                            candidate
                            'alist-mt
                            '())
             (try-candidate name
                            candidate
                            '(alist-extend 'z 'baz alist-mt)
                            '((z . baz)))
             (try-candidate name
                            candidate
                            '(alist-extend 'y 'bar (alist-extend 'z 'baz alist-mt))
                            '((y . bar) (z . baz)))
             (try-candidate name
                            candidate
                            '(alist-extend 'x 'foo (alist-extend 'y 'bar (alist-extend 'z 'baz alist-mt)))
                            '((x . foo) (y . bar) (z . baz)))
               ;;; etc.
             )))

(define reify-environment-binding-symbols-to-symbols
    (reify-environment reify-symbol reify-symbol))

(unless (test-reify-environment-binding-symbols-to-symbols 'reify-environment-binding-symbols-to-symbols reify-environment-binding-symbols-to-symbols)
  (printf "(test-reify-environment-binding-symbols-to-symbols 'reify-environment-binding-symbols-to-symbols reify-environment-binding-symbols-to-symbols) failed~n"))

(define test-reify-first-path
  (lambda (candidate)
    (and (equal? (candidate 'x '10 '(10 20 30))
                 '(lambda (x) (car x)))
         (equal? (candidate 'y '20 '(10 20 30))
                 '(lambda (y) (car (cdr y))))
         (equal? (candidate 'z '30 '(10 20 30))
                 '(lambda (z) (car (cdr (cdr z)))))
         (equal? (candidate 'a '40 '(10 20 30))
                 #f)
         (equal? (candidate 'x '10 '((10) (20) (30)))
                 '(lambda (x) (car (car x))))
         (equal? (candidate 'y '20 '((10) (20) 30))
                 '(lambda (y) (car (car (cdr y)))))
         (equal? (candidate 'z '10 '(((10)) 10))
                 '(lambda (z) (car (car (car z)))))
         )))

;;; BNF for Binary-tree-of-properlist:
;;; <Binary-tree-of-properlist> := <Binary-tree-of-properlist> <Binary-tree-of-properlist_1>
;;;                             := <number>
;;; <Binary-tree-of-properlist_1> := <Binary-tree-of-properlist> <Binary-tree-of-properlist_1>
;;;                               := <null>

;; !!ACUMULATOR MASTER RACE!!
(define reify-first-path 
  (lambda (p v ls)
    (letrec ([visit (lambda (vs a)
                      (cond
                        [(null? vs)
                         #f]
                        [(number? vs)
                         (if (equal? v vs)
                            `(lambda (,p) ,a)
                             #f)]
                        [(pair? vs)
                         (or (visit (car vs) `(car ,a))
                             (visit (cdr vs) `(cdr ,a)))
                         ]
                        [else
                         (errorf 'reify-first-path
                                 "error: ~s"
                                 vs)
                         ]))])
      (visit ls p)
      )))

(unless (test-reify-first-path reify-first-path)
  (printf "you suck"))
;;;;;

(define test-reify-last-path
  (lambda (candidate)
    (and (equal? (candidate 'x '10 '(((10)) 10))
                 '(lambda (x) (car (cdr x))))
         (equal? (candidate 'x '20 '(((10)) 10))
                 #f)
         (equal? (candidate 'x '10 '(((10)) 10 10))
                 '(lambda (x) (car (cdr (cdr x)))))
         (equal? (candidate 'x '10 '(((10)) (10 10)))
                 '(lambda (x) (car (cdr (car (cdr x))))))
         )))
;;;


(define reify-last-path 
  (lambda (p v ls)
    (letrec ([visit (lambda(vs a)
                      (cond
                        [(null? vs)
                         #f]
                        [(number? vs)
                         (if (equal? v vs)
                             `(lambda (,p) ,a)
                             #f)]
                        [(pair? vs)
                         (or (visit (cdr vs) `(cdr ,a))
                             (visit (car vs) `(car ,a)))
                         ]
                        [else
                         (errorf 'reify-first-path
                                 "error: ~s"
                                 vs)
                         ]))])
      (visit ls p)
      )))


(unless (test-reify-last-path reify-last-path)
  (printf "git gud"))
;;;;
(define test-reify-nth-path
  (lambda (candidate)
    (and (equal? (candidate 'x '10 '(((10)) 10 ) 1)
                 '(lambda (x) (car (car (car x)))))
         (equal? (candidate 'x '10 '(((10)) 10 ) 2)
                 '(lambda (x) (car (cdr x))))
         (equal? (candidate 'x '10 '(((10)) 10 ) 3)
                 #f)
         (equal? (candidate 'x '30 '(((10)) 20  30 ) 1)
                 '(lambda (x) (car (cdr (cdr x)))))
         (equal? (candidate 'x '10 '(((10)) 20  10 ) 2)
                 '(lambda (x) (car (cdr (cdr x)))))
         (equal? (candidate 'x '10 '(((10)) 20  10 ) 3)
                 #f)
         )))
;;;
(define reify-nth-path 
  (lambda (p v ls n_init)
    (letrec ([visit (trace-lambda visit (n vs a)
                      (cond
                        [(null? vs)
                         (values #f n)]
                        [(number? vs)
                         (if (and (equal? v vs)
                                  (equal? n 1))
                             (values `(lambda (,p) ,a) n)
                             (if (equal? v vs)
                                 (values #f (- n 1))
                                 (values #f n)))]
                        [(pair? vs)
                         (let-values([(leftres leftn)
                                      (visit n (car vs) `(car ,a))])
                           (let-values([(rightres rightn)
                                        (visit  leftn (cdr vs) `(cdr ,a))])
                             (values (or leftres rightres) rightn)))
                         ]
                        [else
                         (errorf 'reify-first-path
                                 "error: ~s"
                                 vs)
                         ]))])
      (let-values ([(rec _d) (visit n_init ls p)])
        rec)
      )))


(unless (test-reify-nth-path reify-nth-path)
  (printf "rekt"))

;How should the accumulator be incorprated?
(define fold-right_binary-tree-from-proper-lists
  (lambda (nod1 lea nod2 nul err)
    (lambda (v_init)
      (letrec ([visit1 (lambda (v)
                        (cond
                          [(number? v)
                           (lea v)]
                          [(pair? v)
                           (nod1 (visit1 (car v))
                                 (visit2 (cdr v)))]
                          [else
                           (err v)]))]
			   [visit2 (lambda (v)
						 (cond
						   [(null? v)
							(nul v)]
						   [(pair? v)
							(nod2 (visit1 (car v))
								  (visit2 (cdr v)))]
						   [else
							 (err v)]))])
        (visit1 v_init)))))


(define reify-first-path_fold-right
  ;(lambda (p v ls)
  (trace-lambda entering (p v ls)
	(letrec ([rec ((fold-right_binary-tree-from-proper-lists
				  ;(lambda (vl vr)
				  (trace-lambda nod1 (vl vr)
					(if vl
					  (cons 'car vl)
					  (if vr
						(cons 'cdr vr) 
						#f))
					)
				  ;(lambda (vs)
				  (trace-lambda lea (vs)
					(if (equal? v vs)
					  '()
					  #f))
				  ;(lambda (vl vr)
				  (trace-lambda nod2 (vl vr)
					(if vl
					  (cons 'car vl)
					  (if vr
						(cons 'cdr vr) 
						#f))
					)
				  ;(lambda (vs)
				  (trace-lambda nil (vs)
					#f)
				  ;(lambda (vs)
				  (trace-lambda err (vs)
					(errorf 'reify-first-path_fold-right
							"error: ~s"
							vs)))
				ls)]
		  [rebuild (lambda (vs)
					 (cond
					   [(null? vs)
						p]
					   [(pair? vs)
						(list (car vs) (rebuild (cdr vs)))]))])
	  (if rec
		`(lambda (,p) ,(rebuild (reverse rec)))
		#f))))

(unless (test-reify-first-path reify-first-path_fold-right)
  (printf "I suck"))

(define reify-last-path_fold-right
  ;(lambda (p v ls)
  (trace-lambda entering (p v ls)
	(letrec ([rec ((fold-right_binary-tree-from-proper-lists
				  ;(lambda (vl vr)
				  (trace-lambda nod1 (vl vr)
					(if vr
					  (cons 'cdr vr)
					  (if vl
						(cons 'car vl) 
						#f))
					)
				  ;(lambda (vs)
				  (trace-lambda lea (vs)
					(if (equal? v vs)
					  '()
					  #f))
				  ;(lambda (vl vr)
				  (trace-lambda nod2 (vl vr)
					(if vr
					  (cons 'cdr vr)
					  (if vl
						(cons 'car vl) 
						#f))
					)
				  ;(lambda (vs)
				  (trace-lambda nil (vs)
					#f)
				  ;(lambda (vs)
				  (trace-lambda err (vs)
					(errorf 'reify-first-path_fold-right
							"error: ~s"
							vs)))
				ls)]
		  [rebuild (lambda (vs)
					 (cond
					   [(null? vs)
						p]
					   [(pair? vs)
						(list (car vs) (rebuild (cdr vs)))]))])
	  (if rec
		`(lambda (,p) ,(rebuild (reverse rec)))
		#f))))

(unless (test-reify-last-path reify-last-path_fold-right)
  (printf "I suck"))


;;;;;;;;;;;;;;;;;;;;

;;; end of week-06_evaluation-and-reification.scm

"week-06_evaluation-and-reification.scm"
