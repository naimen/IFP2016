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

;; Interpreter
(define interpret-regular-expression-left-most-result
                                        ;(lambda (r vs)
  (trace-lambda entering (r vs)
                                        ;(letrec ([visit (lambda (r vs)
    (letrec ([visit (trace-lambda visit (r vs env)
                      (cond
                        [(is-empty? r)
;; If the current expression is empty, then we want to make sure that the current list is empty.
                         (if (equal? vs '())
                             env
                             #f) ]
                        [(is-atom? r)
;; If the current expression is an atom, then we want to make sure that the current list is the same integer.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 (if (equal? (car vs) (atom-1 r))
                                     env
                                     #f)
                                 (errorf 'interpret-regular-expression-left-most-result
                                         "Element is not an integer: ~s"
                                         vs))
                             #f) ]
                        [(is-any? r)
;; If the current expression is any, then we want to make sure that there is 1 element in the list.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 env
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
                                                (let* ([split (splitAt '() n vs)]
                                                       [res1 (visit (seq-1 r) (car split) env)])
                                                  (if res1
                                                      (let ([res2 (visit (seq-2 r) (cdr split) res1)]) 
                                                        (if (and res1 res2)
                                                            (cond [(and (equal? '() res1) (equal? '() res2))
                                                                   env]
                                                                  [(equal? '() res1)
                                                                   res2]
                                                                  [(equal? '() res2)
                                                                   res1]
                                                                  [(and (not (equal? '() res1)) (not (equal? '() res2)))
                                        ;(cons res1 res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                   res2]
                                                                  [else
                                                                   (errorf 'interpret-regular-expression-left-most-result
                                                                           "This should never happen")]
                                                                  )
                                                            (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                                #f
                                                                (sequensize (+ n 1)))))
                                                      (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                          #f
                                                          (sequensize (+ n 1))))))])
                           (sequensize 0)) ]
                        [(is-disj? r)
;; If the current expression is a disjunktion, then we want to try both sub-expressions on the list.
                         (or (visit (disj-2 r) vs env)
                             (visit (disj-1 r) vs env)) ] ;TODO, DISJUNKTION BEHAVES AS RIGHTMOST
                        [(is-star? r)
;; If the current expression is a star, then we want to try the sub-expression with an increasing amount of the list, starting with none of the list, and calling visit with the current expression and the remaining list.
                                        ;(letrec ([splitAt (trace-lambda splitAt (a n lst)
                         (letrec ([splitAt (lambda (a n lst)
                                             (if (or (= n 0) (null? lst))
                                                 (cons (reverse a) lst)
                                                 (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
                                  [starize (lambda (n)
                                        ;[starize (trace-lambda starize (n)
                                             (let* ([split (splitAt '() n vs)]
                                                    [res1 (visit (star-1 r) (car split) env)])
                                               (if res1
                                                   (let ([res2 (visit r (cdr split) res1)])
                                                     (if (and res1 res2)
                                                         (cond [(and (null? res1) (null? res2))
                                                                env]
                                                               [(null? res1)
                                                                res2]
                                                               [(null? res2)
                                                                res1]
                                                               [else
                                        ;(cons (car res1) res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                res2]
                                                               )
                                        ;(cons res1 res2)
                                                         (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                             #f
                                                             (starize (+ n 1)))))
                                                   (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                       #f
                                                       (starize (+ n 1)))
                                                   )))])
                           (if (equal? '() vs)
                               env
                               (starize 1))) ]
                        [(is-plus? r)
;; If the current expression is a plus, then we want to try the sub-expression with an increasing amount of the list, starting with one element, and calling visit with a star, containing the sub-expression, and the remaining list.
                         (letrec ([splitAt (lambda (a n lst)
                                             (if (or (= n 0) (null? lst))
                                                 (cons (reverse a) lst)
                                                 (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
                                  [plusize (lambda (n)
                                             (let* ([split (splitAt '() n vs)]
                                                    [res1 (visit (plus-1 r) (car split) env)])
                                               (if res1
                                                   (let ([res2 (visit (make-star (plus-1 r)) (cdr split) res1)])
                                                     (if (and res1 res2)
                                                         (cond [(and (null? res1) (null? res2))
                                                                env]
                                                               [(null? res1)
                                                                res2]
                                                               [(null? res2)
                                                                res1]
                                                               [else
                                        ;(cons (car res1) res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                res2]
                                                               )
                                        ;(cons res1 res2)
                                                         (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                             #f
                                                             (plusize (+ n 1)))))
                                                   (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                       #f
                                                       (plusize (+ n 1)))
                                                   )))])
                           (plusize 1)) ]
                        [(is-var? r)
;; If the current expression is a var, then we want to make sure, that the current list is one element, and return the environment containing the var and the element.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 (letrec ([is-in-env (lambda (vs)
                                                       (if (pair? vs)
                                                           (if (pair? (car vs))
                                                               (if (equal? (caar vs) (var-1 r))
                                                                   #t
                                                                   (is-in-env (cdr vs)))
                                                               (errorf 'is-in-env
                                                                       "Not a proper environment 1"))
                                                           (if (null? vs)
                                                               #f
                                                               (errorf 'is-in-env
                                                                       "Not a proper environment"))))])
                                   (if (not (is-in-env env))
                                       (cons (cons (var-1 r) (car vs)) env)
                                       env
                                       ))
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
      (visit r vs '()))))

;(unless (test-interpret-regular-expression-generic interpret-regular-expression-left-most-result)
 ; (printf "I Suck Left"))


(define interpret-regular-expression-left-most-result_1
  (lambda (reg vs)
    (letrec ([visit (trace-lambda visit (r vs k)
                      (cond
                        ;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (if (equal? vs '())
                             (k '())
                             (k #f))]
                        ;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (if (and ;(proper-list-of-given-length? vs 1)
                                  (number? (car vs))
                                  (equal? (car vs) (atom-1 r)))
                             (k (cdr vs))
                             (k #f))]
                        ;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                         (if (and ;(proper-list-of-given-length? vs 1)
                                  (number? (car vs)))
                             (k (cdr vs))
                             (k #f))]
                        ;If r is seq, and vs is a pair, we should travers the left side of vs, and the right side of vs. 
                        [(is-seq? r) ;seems pretty robust now
                         (cond
                           [(null? vs)
                            #f]
                           [(pair? vs)
                            (visit (seq-1 r) vs
                                   (lambda (res)
                                     (visit (seq-2 r) res k
                                        ;(lambda (maybe?)
                                        ;  (k (and res maybe?)))
                                            )))]
                           [else
                            (errorf 'interpret-regular-expression-left-most-result_1
                                    "Not a proper list. ~s"
                                    vs)])]
                        ;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (cond
                           [(null? vs)
                            #f]
                           [(pair? vs)
                            (or (visit (disj-2 r) vs k)
                                (visit (disj-1 r) vs k))]
                           [else
                            (errorf 'interpret-regular-expression-left-most-result_1
                                    "Not a proper list. ~s"
                                    vs)])]
                        [(is-star? r)
                         (visit (star-1 r) vs (lambda (x)
                                                (visit (star-1 r) x k)))
                         ]
                        [(is-plus? r)
                         ]
                        [(is-var? r)
                         ]
                        [else
                         (errorf 'interpret-regular-expression-left-most-result_1
                                 "ERROR ~s"
                                 vs)]))])
      (visit reg vs (lambda (x) x)))))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-left-most-result_1)
  (printf "I Suck Left2"))

;;just leftmost fliped
(define interpret-regular-expression-right-most-result
                                        ;(lambda (r vs)
  (trace-lambda entering (r vs)
                                        ;(letrec ([visit (lambda (r vs)
    (letrec ([visit (trace-lambda visit (r vs env)
                      (cond
                        [(is-empty? r)
;; If the current expression is empty, then we want to make sure that the current list is empty.
                         (if (equal? vs '())
                             env
                             #f) ]
                        [(is-atom? r)
;; If the current expression is an atom, then we want to make sure that the current list is the same integer.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 (if (equal? (car vs) (atom-1 r))
                                     env
                                     #f)
                                 (errorf 'interpret-regular-expression-left-most-result
                                         "Element is not an integer: ~s"
                                         vs))
                             #f) ]
                        [(is-any? r)
;; If the current expression is any, then we want to make sure that there is 1 element in the list.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 env
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
                                                (let* ([split (splitAt '() n vs)]
                                                       [res1 (visit (seq-2 r) (cdr split) env)])
                                                  (if res1
                                                      (let ([res2 (visit (seq-1 r) (car split) res1)]) 
                                                        (if (and res1 res2)
                                                            (cond [(and (equal? '() res1) (equal? '() res2))
                                                                   env]
                                                                  [(equal? '() res1)
                                                                   res2]
                                                                  [(equal? '() res2)
                                                                   res1]
                                                                  [(and (not (equal? '() res1)) (not (equal? '() res2)))
                                        ;(cons res1 res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                   res2]
                                                                  [else
                                                                   (errorf 'interpret-regular-expression-left-most-result
                                                                           "This should never happen")]
                                                                  )
                                                            (if (or (equal? n 0) (null? vs))
                                                                #f
                                                                (sequensize (- n 1)))))
                                                      (if (or (equal? n 0) (null? vs))
                                                          #f
                                                          (sequensize (- n 1))))))])
                           (sequensize (length vs))) ]
                        [(is-disj? r)
;; If the current expression is a disjunktion, then we want to try both sub-expressions on the list.
                         (or (visit (disj-1 r) vs env)
                             (visit (disj-2 r) vs env)) ] ;TODO, BEHAVES LIKE LEFTMOST
                        [(is-star? r)
;; If the current expression is a star, then we want to try the sub-expression with an increasing amount of the list, starting with none of the list, and calling visit with the current expression and the remaining list.
                                        ;(letrec ([splitAt (trace-lambda splitAt (a n lst)
                         (letrec ([splitAt (lambda (a n lst)
                                             (if (or (= n 0) (null? lst))
                                                 (cons (reverse a) lst)
                                                 (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
                                  [starize (lambda (n)
                                        ;[starize (trace-lambda starize (n)
                                             (let* ([split (splitAt '() n vs)]
                                                    [res1 (visit (star-1 r) (car split) env)])
                                               (if res1
                                                   (let ([res2 (visit r (cdr split) res1)])
                                                     (if (and res1 res2)
                                                         (cond [(and (null? res1) (null? res2))
                                                                env]
                                                               [(null? res1)
                                                                res2]
                                                               [(null? res2)
                                                                res1]
                                                               [else
                                        ;(cons (car res1) res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                res2]
                                                               )
                                        ;(cons res1 res2)
                                                         (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                             #f
                                                             (starize (+ n 1)))))
                                                   (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                       #f
                                                       (starize (+ n 1)))
                                                   )))])
                           (if (equal? '() vs)
                               env
                               (starize 1))) ]
                        [(is-plus? r)
;; If the current expression is a plus, then we want to try the sub-expression with an increasing amount of the list, starting with one element, and calling visit with a star, containing the sub-expression, and the remaining list.
                         (letrec ([splitAt (lambda (a n lst)
                                             (if (or (= n 0) (null? lst))
                                                 (cons (reverse a) lst)
                                                 (splitAt (cons (car lst) a) (- n 1) (cdr lst))))]
                                  [plusize (lambda (n)
                                             (let* ([split (splitAt '() n vs)]
                                                    [res1 (visit (plus-1 r) (car split) env)])
                                               (if res1
                                                   (let ([res2 (visit (make-star (plus-1 r)) (cdr split) res1)])
                                                     (if (and res1 res2)
                                                         (cond [(and (null? res1) (null? res2))
                                                                env]
                                                               [(null? res1)
                                                                res2]
                                                               [(null? res2)
                                                                res1]
                                                               [else
                                        ;(cons (car res1) res2)] ;; DOES NOT GIVE CORRECT OUTPUT. TODO
                                                                res2]
                                                               )
                                        ;(cons res1 res2)
                                                         (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                             #f
                                                             (plusize (+ n 1)))))
                                                   (if (or (proper-list-of-given-length? vs n) (null? vs))
                                                       #f
                                                       (plusize (+ n 1)))
                                                   )))])
                           (plusize 1)) ]
                        [(is-var? r)
;; If the current expression is a var, then we want to make sure, that the current list is one element, and return the environment containing the var and the element.
                         (if (proper-list-of-given-length? vs 1)
                             (if (number? (car vs))
                                 (letrec ([is-in-env (lambda (vs)
                                                       (if (pair? vs)
                                                           (if (pair? (car vs))
                                                               (if (equal? (caar vs) (var-1 r))
                                                                   #t
                                                                   (is-in-env (cdr vs)))
                                                               (errorf 'is-in-env
                                                                       "Not a proper environment 1"))
                                                           (if (null? vs)
                                                               #f
                                                               (errorf 'is-in-env
                                                                       "Not a proper environment"))))])
                                   (if (not (is-in-env env))
                                       (cons (cons (var-1 r) (car vs)) env)
                                       env
                                       ))
                                 (errorf 'interpret-regular-expression-left-most-result
                                         "Element is not an integer: ~s"
                                         vs))
                             #f) ]
                        [else
                         ;; If the current expression is illegal, then we want to raise an error.
                         (errorf 'interpret-regular-expression-right-most-result
                                 "Not a proper regular expression: ~s"
                                 r) ]
                        ))])
      (visit r vs '()))))

;(unless (test-interpret-regular-expression-generic interpret-regular-expression-right-most-result)
 ; (printf "I Suck Right"))


;;; end of RegexInterpreter.scm

"RegexInterpreter.scm"


