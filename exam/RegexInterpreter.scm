;;; RegexInterpreter.scm
;;; IFP 2016-2016, Q1
;;; Markus, Rasmus, Shuo


;;;;;;;;;;


;;BNF for Regex

;; <regexp> ::= (empty)
;;            | (atom <atom>)
;;            | (any)
;;            | (seq <regexp> <regexp>)
;;            | (disj <regexp> <regexp>)
;;            | (star <regexp>)
;;            | (plus <regexp>)
;;            | (var <name>)

;; <atom>   ::= ...any Scheme integer...

;; <name>   ::= ...any Scheme identifier...

(load "RegexSamples.scm")

;;;;;;;;;;
;; Constructors
(define make-empty
  (lambda ()
    (list 'empty)))

(define make-atom
  (lambda (a)
    (list 'atom a)))

(define make-any
  (lambda ()
    (list 'any)))

(define make-seq
  (lambda (re1 re2)
    (list 'seq re1 re2)))

(define make-disj
  (lambda (re1 re2)
    (list 'disj re1 re2)))

(define make-star
  (lambda (re)
    (list 'star re)))

(define make-plus
  (lambda (re)
    (list 'plus re)))

(define make-var
  (lambda (name)
    (list 'var name)))
;;;;;;;

;; Predicates
(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

(define is-empty?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'empty)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-atom?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'atom)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-any?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'any)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-seq?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'seq)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-disj?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'disj)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-star?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'star)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-plus?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'plus)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-var?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'var)
         (proper-list-of-given-length? (cdr v) 1))))
;;;;;;;

;; Accessor
(define atom-1
  (lambda (v)
    (list-ref v 1)))

(define seq-1
  (lambda (v)
    (list-ref v 1)))

(define seq-2
  (lambda (v)
    (list-ref v 2)))

(define disj-1
  (lambda (v)
    (list-ref v 1)))

(define disj-2
  (lambda (v)
    (list-ref v 2)))

(define star-1
  (lambda (v)
    (list-ref v 1)))

(define plus-1
  (lambda (v)
    (list-ref v 1)))

(define var-1
  (lambda (v)
    (list-ref v 1)))
;;;;;;;

;; Syntax-checker
(define check-regular-expression
  (lambda (v)
    (cond
      [(is-empty? v)
       #t]
      [(is-atom? v)
       (number? (atom-1 v))]
      [(is-any? v)
       #t]
      [(is-seq? v)
       (and (check-regular-expression (seq-1 v))
            (check-regular-expression (seq-2 v)))]
      [(is-disj? v)
       (and (check-regular-expression (disj-1 v))
            (check-regular-expression (disj-2 v)))]
      [(is-star? v)
       (check-regular-expression (star-1 v))]
      [(is-plus? v)
       (check-regular-expression (plus-1 v))]
      [(is-var? v)
       (symbol? (var-1 v))]
      [else
       #f])))
;;;;;;;;;;

;; Testing the interpreter

(define test-interpret-regular-expression-left-most-result
  (lambda (candidate)
	(and (andmap (lambda (re) (not (equal? (candidate (car re) (cdr re)) #f))) sample-of-regular-expressions))
	(and (andmap (lambda (re) (equal? (candidate (car re) (cdr re)) #f)) sample-of-negative-regular-expressions))))

;; Interpreter
(define interpret-regular-expression-left-most-result
  (lambda (r vs)
	(letrec ([visit (lambda (r vs)
					  (cond
						[(is-empty? r)
						 ;; If the current expression is empty, then we want to make sure that the current list is empty.
						 (if (equal? vs '())
						   '()
						   #f) ]
						[(is-atom? r)
						 ;; If the current expression is an atom, then we want to make sure that the current list is the same integer.
						 (if (proper-list-of-given-length? vs 1)
						   (if (number? (car vs))
							 (if (equal? (car vs) (atom-1 r))
							   '()
							   #f)
							 (errorf 'interpret-regular-expression-left-most-result
									 "Element is not an integer: ~s"
									 vs))
						   #f) ]
						[(is-any? r)
						 ;; If the current expression is any, then we want to make sure that there is 1 element in the list.
						 (if (proper-list-of-given-length? vs 1)
						   (if (number? (car vs))
							 '()
							 (errorf 'interpret-regular-expression-left-most-result
									 "Element is not an integer: ~s"
									 vs))
						   #f) ]
						[(is-seq? r)
						 ;; If the current expression is a sequence, then we want to try all possible configurations, starting with the configuration, where the left-most sequence has none of the remaining list, and the right-most sequence has all of the remaining list.
						 (letrec ([splitAt (lambda (a n lst)
											 (if (or (= n 0) (null? lst))
											   (cons (reverse a) lst)
											   (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
								  [sequensize (lambda (n)
												(let ([split (splitAt '() n vs)]
													  [res1 (visit (seq-1 r) (car split))]
													  [res2 (visit (seq-2 r) (cdr split))])
												  (if (and res1 res2)
													(list res1 res2)
													(if (proper-list-of-given-length? vs n)
													  #f
													  (sequensize (+ n 1))))))])
						   (sequensize 0)) ]
						[(is-disj? r)
						 ;; If the current expression is a disjunktion, then we want to try both sub-expressions on the list.
						 (or (visit (disj-1 r) vs)
							 (visit (disj-2 r) vs)) ]
						[(is-star? r)
						 ;; If the current expression is a star, then we want to try the sub-expression with an increasing amount of the list, starting with none of the list, and calling visit with the current expression and the remaining list.
						 (letrec ([splitAt (lambda (a n lst)
											 (if (or (= n 0) (null? lst))
											   (cons (reverse a) lst)
											   (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
								  [starize (lambda (n)
											 (let ([split (splitAt '() n vs)]
												   [res1 (visit (star-1 r) (car split))]
												   [res2 (visit r (cdr split))])
											   (if (and res1 res2)
												 (list res1 res2)
												 (if (proper-list-of-given-length? vs n)
												   #f
												   (starize (+ n 1))))))])
						   (if (equal? '() vs)
							 '()
							 (starize 0))) ]
						[(is-plus? r)
						 ;; If the current expression is a plus, then we want to try the sub-expression with an increasing amount of the list, starting with one element, and calling visit with a star, containing the sub-expression, and the remaining list.
						 (letrec ([splitAt (lambda (a n lst)
											 (if (or (= n 0) (null? lst))
											   (cons (reverse a) lst)
											   (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
								  [plusize (lambda (n)
											 (let ([split (splitAt '() n vs)]
												   [res1 (visit (plus-1 r) (car split))]
												   [res2 (visit (make-star (plus-1 r)) (cdr split))])
											   (if (and res1 res2)
												 (list res1 res2)
												 (if (proper-list-of-given-lengh? vs n)
												   #f
												   (plusize (+ n 1))))))])
						   (starize 0)) ]
						[(is-var? r)
						 ;; If the current expression is a var, then we want to make sure, that the current list is one element, and return the environment containing the var and the element.
						 (if (proper-list-of-given-length? vs 1)
						   (if (number? (car vs))
							 (list (var-1 r) (car vs))
							 (errorf 'interpret-regular-expression-left-most-result
									 "Element is not an integer: ~s"
									 vs))
						   #f) ]
						[else
						  ;; If the current expression is illegal, then we want to raise an error.
						  (errorf 'interpret-regular-expression-left-most-result
								  "Not a proper regular expression: ~s"
								  r) ]
						))])
	  (visit (r vs)))))

;;; end of RegexInterpreter.scm

"RegexInterpreter.scm"

