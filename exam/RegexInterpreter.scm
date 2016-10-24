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

(define test-interpret-regular-expression-generic
  (lambda (candidate)
    (and (andmap (lambda (re) (not (equal? (candidate (car re) (cdr re)) #f))) sample-of-regular-expressions)
         (andmap (lambda (re) (equal? (candidate (car re) (cdr re)) #f)) sample-of-negative-regular-expressions))))

(define test-interpret-regular-expression-leftmost
  (lambda (candidate)
    (andmap (lambda (re)
              (equal? (candidate (caar re)
                                 (cdar re))
                      (cdr re)))
            sample-of-regular-expressions-leftmost)))

(define test-interpret-regular-expression-rightmost
  (lambda (candidate)
    (andmap (lambda (re)
              (equal? (candidate (caar re)
                                 (cdar re))
                      (cdr re)))
            sample-of-regular-expressions-rightmost)))


(define test-interpret-regular-expression-number
  (lambda (candidate)
    (andmap (lambda (re)
              (equal? (candidate (caar re)
                                 (cdar re))
                      (cdr re)))
            sample-of-regular-expressions-with-number)
    ))



;; Interpreter


(define interpret-regular-expression-left-most-result
  (trace-lambda leftmost (reg vs)
    (letrec ([visit (trace-lambda visit (r vs env k)
                      (cond
;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (if (equal? vs '())
                             (k '() env)
                             (k vs env))]
;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (if (and (number? (car vs))
                                     (equal? (car vs) (atom-1 r)))
                                (k (cdr vs) env)
                                (k #f env))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (if (number? (car vs))
                                (k (cdr vs) env)
                                (k #f env))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
;If r is seq, and vs is a pair, we should travers the left side of vs, and the right side of vs. 
                        [(is-seq? r) ;seems pretty robust now
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (seq-1 r) vs env
                                   (lambda (res env)
                                     (if res
                                         (visit (seq-2 r) res  env
                                                (lambda (res2 env)
                                                  (if (and res res2)
                                                      (k res2 env)
                                                      (k #f env))))
                                         (k res env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (disj-2 r) vs env
                                   (lambda (x env2)
                                     (visit (disj-1 r) vs env
                                            (lambda (y env3)
                                              (if (and x (k x env2))
                                                  (k x env2)
                                                  (if (and y (k y env3))
                                                      (k y env3)
                                                      (k #f env)))))))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
; comment
                        [(is-star? r)
                         (cond
                           [(null? vs)
                            (k '() env)]
                           [(pair? vs)
                            (let ([try1 (visit (star-1 r) vs env
                                               (lambda (x env)
                                                 (if x
                                                     (if (null? (k x env))
                                                         (k x env)
                                                         (visit r x env k))
                                                     (k x env))))])
                              (if try1
                                  try1
                                  (k vs env)))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
;comment
                        [(is-plus? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (plus-1 r) vs env
                                   (lambda (x env)
                                     (if x
                                         (if (null? (k x env))
                                             (k x env)
                                             (visit (make-star (plus-1 r))
                                                    x env k))
                                         (k x env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
;comment
                        [(is-var? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (letrec ([is-in-env?
                                      (lambda (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (caar env) x)
                                               #t
                                               (is-in-env? x (cdr env)))]
                                          [else
                                           (errorf 'is-in-env
                                                   "Not a proper environment: ~s"
                                                   env)]))]
                                     [get-from-env
                                      (lambda (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (car (car env)) x)
                                               (cdr (car env))
                                               (get-from-env x (cdr env)))]
                                          [else
                                           (errorf 'get-from-env
                                                   "Not a proper environment: ~s"
                                                   env)]))])
                              (if (is-in-env? (var-1 r) env)
                                  (if (equal?
                                       (get-from-env (var-1 r) env)
                                       (car vs))
                                      (k (cdr vs) env)
                                      (k #f env))
                                  (k (cdr vs)
                                     (cons (cons (var-1 r)
                                                 (car vs))
                                           env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result
                             "Not a proper list: ~s"
                             vs)])]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result
                          "Not a recognized expression, please consult BNF:  ~s"
                          vs)]))])
      (visit reg vs '()
             (lambda (x env)
               (if (null? x)
                   env
                   #f))))))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-left-most-result)
  (printf "Regex mismatch in left-most."))

(unless (test-interpret-regular-expression-leftmost interpret-regular-expression-left-most-result)
  (printf "Result of left-most interpreter does not match the expected value"))

;;;;;;;;;;;

(define interpret-regular-expression-right-most-result
  (lambda (reg vs)
    (letrec ([visit (lambda (r vs env k)
                      (cond
;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (if (equal? vs '())
                             (k '() env)
                             (k vs env))]
;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (if (and (number? (car vs))
                                     (equal? (car vs) (atom-1 r)))
                                (k (cdr vs) env)
                                (k #f env))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (if (number? (car vs))
                                (k (cdr vs) env)
                                (k #f env))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
;If r is seq, and vs is a pair, we should travers the left side of vs, and the right side of vs. 
                        [(is-seq? r) ;seems pretty robust now
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (seq-1 r) vs env
                                   (lambda (res env)
                                     (if res
                                         (visit (seq-2 r) res  env
                                                (lambda (res2 env)
                                                  (if (and res res2)
                                                      (k res2 env)
                                                      (k #f env))))
                                         (k res env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (disj-2 r) vs env
                                   (lambda (x env2)
                                     (visit (disj-1 r) vs env
                                            (lambda (y env3)
                                              (if (and y (k y env3))
                                                  (k y env3)
                                                  (if (and x (k x env2))
                                                      (k x env2)
                                                      (k #f env)))))))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
; comment
                        [(is-star? r)
                         (cond
                           [(null? vs)
                            (k '() env)]
                           [(pair? vs)
                            (let ([try1 (visit (star-1 r) vs env
                                               (lambda (x env)
                                                 (if x
                                                     (if (k x env)
                                                         (k x env)
                                                         (visit r x env k))
                                                     (k x env))))])
                              (if (k vs env)
                                  (k vs env)
                                  try1))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
;comment
                        [(is-plus? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (visit (plus-1 r) vs env
                                   (lambda (x env)
                                     (if x
                                         (if (null? (k x env))
                                             (k x env)
                                             (visit (make-star (plus-1 r))
                                                    x env k))
                                         (k x env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "Not a proper list. ~s"
                             vs)])]
;comment
                        [(is-var? r)
                         (cond
                           [(null? vs)
                            (k #f env)]
                           [(pair? vs)
                            (letrec ([is-in-env?
                                      (lambda (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (caar env) x)
                                               #t
                                               (is-in-env? x (cdr env)))]
                                          [else
                                           (errorf 'is-in-env
                                                   "Not a proper environment: ~s"
                                                   env)]))]
                                     [get-from-env
                                      (lambda (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (car (car env)) x)
                                               (cdr (car env))
                                               (get-from-env x (cdr env)))]
                                          [else
                                           (errorf 'get-from-env
                                                   "Not a proper environment: ~s"
                                                   env)]))])
                              (if (is-in-env? (var-1 r) env)
                                  (if (equal?
                                       (get-from-env (var-1 r) env)
                                       (car vs))
                                      (k (cdr vs) env)
                                      (k #f env))
                                  (k (cdr vs)
                                     (cons (cons (var-1 r)
                                                 (car vs))
                                           env))))]
                           [else
                            (errorf
                             'interpret-regular-expression-right-most-result
                             "ERROR ~s"
                             vs)])]
                        [else
                         (errorf
                          'interpret-regular-expression-right-most-result
                          "ERROR ~s"
                          vs)]))])
      (visit reg vs '()
             (lambda (x env)
               (if (null? x)
                   env
                   #f))))))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-right-most-result)
  (printf "Regex mismatch in right most"))

(unless (test-interpret-regular-expression-rightmost interpret-regular-expression-right-most-result)
  (printf "Result of right-most interpreter does not match the expected value"))



;;;;;;;;;;;
 (define interpret-regular-expression-number-results
   (trace-lambda entering (reg vs)
     (letrec ([visit (trace-lambda visit (r vs env k)
                       (cond
;If r is empty, and vs is empty too, we should call k with the empty list, the current environment and a count of 1.
; Otherwise we should call k with the resulting list, the current environment and a count of 0.
                        [(is-empty? r)
                         (if (equal? vs '())
                             (k '() env 1)
                             (k vs env 0))]
                        [(is-atom? r) 
                         (cond
						   ;If we get the empty list, call k with #f.
                           [(null? vs)
                            (k #f env 0)]
						   ; If we get a pair, check that the first element is a number equal to the atom.
						   ; If it is, call k with the rest of the list and 1, else call k with #f.
                           [(pair? vs)
                            (if (and ;(proper-list-of-given-length? vs 1)
                                 (number? (car vs))
                                 (equal? (car vs) (atom-1 r)))
                                (k (cdr vs) env 1)
                                (k #f env 0))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result_1
                             "Not a proper list. ~s"
                             vs)])]
                        [(is-any? r)
                         (cond
						   ; If we get the empty list, call k with #f.
                           [(null? vs)
                            (k #f env 0)]
						   ; If we get a pair, and the first element is a number, call k with the resulting list and 1,
						   ; otherwise call k with #f.
                           [(pair? vs)
                            (if (number? (car vs))
                                (k (cdr vs) env 1)
                                (k #f env 0))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result_1
                             "Not a proper list. ~s"
                             vs)])]
                        [(is-seq? r)
                         ;(cond
                           ;[(null? vs)
                            ;(k #f env 0)]
                           ;[(pair? vs)
						   ; We visit the first sub-expression with the whole list
						   ; When that "returns" (that is, calls k) we get the unused part of the input list.
						   ; We then visit the second sub-expression with the unused part of the input list.
						   ; When that calls k we call our k on the part of the list, that the second sub-expression did not use.
						   ; We also pass along the highest count from the two sub-expressions.
                            (visit (seq-1 r) vs env
                                   (lambda (res env c1)
                                     (if res
                                         (visit (seq-2 r) res  env
                                                (lambda (res2 env c2)
                                                  (if (and res res2)
                                                      (k res2 env (max c1 c2))
                                                      (k #f env 0))))
                                         (k #f env 0))))
							]
                           ;[else
                            ;(errorf
                             ;'interpret-regular-expression-left-most-result_1
                             ;"Not a proper list. ~s"
                             ;vs)])]
;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         ;(cond
                           ;[(null? vs)
                            ;(k #f env 0)]
                           ;[(pair? vs)
						   ; We first visit the first sub-expression with the whole list.
						   ; When that calls k we get the remaining input and the current count.
						   ; We then call the second sub-expression with the whole list.
						   ; When that calls k we get the remaining input and the current count.
						   ; If both parts returned something, that our k will give a complete result on,
						   ; then return the sum of the two returning counts and the empty list.
						   ; If only one part returned something, that our k will give a complete result on,
						   ; then return what our k returns.
                            (visit (disj-2 r) vs env
                                   ;(trace-lambda x (x env2 c1)
                                   (lambda (x env2 c1)
                                     (visit (disj-1 r) vs env
                                            ;(trace-lambda y (y env3 c2)
                                            (lambda (y env3 c2)
														  (cond
															[(and 
															   (and y (null? (cdr (k y env3 c2))))
															   (and x (null? (cdr (k x env2 c1)))))
															 (cons (+ (car (k y env3 c2)) (car (k x env2 c1))) '()) ]
															[(and y (null? (cdr (k y env3 c2))))
															 (k y env3 c2)]
															[(and x (null? (cdr (k x env2 c1))))
															 (k x env2 c1)]
															[else
															  (k #f env 0)])
                                              ;(if (and y (k y env3))
                                                  ;(k y env3)
                                                  ;(if (and x (k x env2))
                                                      ;(k x env2)
                                                      ;(k #f env)))
											  ))))
							]
                           ;[else
                            ;(errorf
                             ;'interpret-regular-expression-left-most-result_1
                             ;"Not a proper list. ~s"
                             ;vs)])]
                        [(is-star? r)
						   ; We first visit the sub-expression with the whole list.
						   ; When this calls k we get the remaining list and the count.
						   ; If x is #f, then call k with '() and 0.
						   ; If k of x gives a complete result (null),
						   ; then return the sum of visiting the expression with the remaining list
						   ; and calling k of x with the current count.
						   ; Otherwise visit the whole expression again with x.
                            (visit (star-1 r) vs env
											   (trace-lambda star (x env count)
                                                 (if x
												   (if (null? (cdr (k x env (+ 0 count))))
													 (cons (+ (car (visit r x env k)) (car (k x env count))) '())
													 (visit r x env k))
												   (k '() env 0))))]
                        [(is-plus? r)
                         (cond
                           [(null? vs)
                            (k #f env 0)]
                           [(pair? vs)
                            (visit (plus-1 r) vs env
                                   (trace-lambda plus (x env count)
                                     (if x
                                         (if (null? (cdr (k x env (+ 0 count))))
                                             (k x env (+ 0 count))
                                             (visit (make-star (plus-1 r))
                                                    x env k))
                                         (k #f env 0))))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result_1
                             "Not a proper list. ~s"
                             vs)])]
;comment
                        [(is-var? r)
                         (cond
                           [(null? vs)
                            (k #f env 0)]
                           [(pair? vs)
                            (letrec ([is-in-env?
                                      (trace-lambda is (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (caar env) x)
                                               #t
                                               (is-in-env? x (cdr env)))]
                                          [else
                                           (errorf 'is-in-env
                                                   "Not a proper environment: ~s"
                                                   env)]))]
                                     [get-from-env
                                      (lambda (x env)
                                        (cond
                                          [(null? env)
                                           #f]
                                          [(and (pair? env)
                                                (pair? (car env)))
                                           (if (equal? (car (car env)) x)
                                               (cdr (car env))
                                               (get-from-env x (cdr env)))]
                                          [else
                                           (errorf 'get-from-env
                                                   "Not a proper environment: ~s"
                                                   env)]))])
                              (if (is-in-env? (var-1 r) env)
                                  (if (equal?
                                       (get-from-env (var-1 r) env)
                                       (car vs))
                                      (k (cdr vs) env 1)
                                      (k #f env 0))
                                  (k (cdr vs)
                                     (cons (cons (var-1 r)
                                                 (car vs))
                                           env) 1)))]
                           [else
                            (errorf
                             'interpret-regular-expression-left-most-result_1
                             "ERROR ~s"
                             vs)])]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result_1
                          "ERROR ~s"
                          vs)]))])
      (car (visit reg vs '()
			 (trace-lambda ident (x env count)
             ;(lambda (x env count)
               (if (null? x)
                   (cons count x)
                   (cons #f #f))))))))




(unless (test-interpret-regular-expression-number interpret-regular-expression-number-results)
  (printf "I Suck Numbers"))

;;; end of RegexInterpreter.scm

"RegexInterpreter.scm"


