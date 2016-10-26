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
  (lambda (candidate fail)
    (and (andmap (lambda (re)
                   (not (equal? (candidate (car re)
                                           (cdr re))
                                fail)))
                 sample-of-regular-expressions)
         (andmap (lambda (re)
                   (equal? (candidate (car re)
                                      (cdr re))
                           fail))
                 sample-of-negative-regular-expressions))))


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

(define test-interpret-regular-expression-solutions
  (lambda (candidate)
    (andmap (lambda (re)
              (equal? (candidate (caar re)
                                 (cdar re))
                      (cdr re)))
            sample-of-regular-expressions-with-solutions)
    ))

(define test-interpret-regular-expression_Magritte_generic
  (lambda (candidate)
    (and (andmap (lambda (re) (not (equal? ((eval (candidate (car re))) (cdr re)) #f))) sample-of-regular-expressions-Magritte)
         (andmap (lambda (re) (equal? ((eval (candidate (car re))) (cdr re)) #f)) sample-of-negative-regular-expressions-Magritte))))

;; Interpreter


(define interpret-regular-expression-left-most-result
  (lambda (reg vs)
    (letrec ([visit (lambda (r vs env k)
                      (cond
                                        ;If r is empty we run our continuation on vs and our environment.
                        [(is-empty? r)
                         (k vs env)]
                                        ;If r is an atom, we check if it matches the prefix of vs and continue with the suffix.
                        [(is-atom? r) 
                         (if (and (pair? vs)
                                  (= (car vs) (atom-1 r)))
                             (k (cdr vs) env)
                                        ;If r is any, and the prefix of vs is a number, we should continue on the corresponding suffix of vs.
                             #f)]
                        [(is-any? r)
                         (if (pair? vs)
                             (k (cdr vs) env)
                             #f)]
                                        ;If r is seq, and vs is a pair, we should traverse the left side of vs, and the right side of vs. 
                        [(is-seq? r)
                         (visit (seq-1 r) vs env
                                (lambda (vs1 env1)
                                  (visit (seq-2 r) vs1 env1 k)))]
                                        ;If r is disj, for left-most we visit the left part of the disjunction first, and should that not match, the right part.
                        [(is-disj? r)
                         (or (visit (disj-1 r) vs env k)
                             (visit (disj-2 r) vs env k))]
                                        ;If r is star, we try to match it to as short a match as possible. This is done by either matching the rest of the regular expression to vs, or if that fails, match r with the prefix of vs and continue with the suffix. This is then looped, until the shortest match is found.
                        [(is-star? r)
                         (letrec ([loop (lambda (vs env)
                                          (or (k vs env)
                                              (visit (star-1 r) vs env
                                                     (lambda (vs1 env1)
                                                       (and (not(equal? vs vs1))
                                                            (loop vs1 env1)
                                                            )))))])
                           (loop vs env))]
                                        ;If r is plus, we almost follow the same procedure as star. The difference here is that we immediately match r to the prefix of vs and then continue with the suffix. Should this fail, we loop. Additionally we check whether vs has changed when it loops, to avoid infinite looping.
                        [(is-plus? r)
                         (letrec ([loop (lambda (vs env)
                                          (visit (plus-1 r) vs env
                                                 (lambda (vs1 env1)
                                                   (and (not (equal? vs vs1))
                                                        (or (k vs1 env1)
                                                            (loop vs1 env1)
                                                            )))))])
                           (loop vs env))]
                                        ;If r is var, we check whether it matches any elements already in our environment, and if not, it extends the environment.
                                        ;It might be better to merge is-in-env? and get-from-env, since they both go through the same list and find the same element.
                        [(is-var? r)
                         (letrec ([get-from-env
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
                                        #f]))])
                           (and (pair? vs)
                                (or (and (not (get-from-env (var-1 r) env))
                                         (k (cdr vs)
                                            (cons (cons (var-1 r)
                                                        (car vs))
                                                  env)))
                                    (and (equal?  (get-from-env (var-1 r) env)
                                                  (car vs))
                                         (k (cdr vs) env)))))]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result
                          "Not a recognized expression, please consult BNF:  ~s"
                          r)]))])
      (visit reg vs '()
             (lambda (x env)
               (if (null? x)
                   env
                   #f))))))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-left-most-result #f)
  (printf "Regex mismatch in left-most."))

(unless (test-interpret-regular-expression-leftmost interpret-regular-expression-left-most-result)
  (printf "Result of left-most interpreter does not match the expected value"))


(define interpret-regular-expression-right-most-result
  (lambda (reg vs)
    (letrec ([visit (lambda (r vs env k)
                      (cond
;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (k vs env)]
;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (if (and (pair? vs)
                                  (= (car vs) (atom-1 r)))
                             (k (cdr vs) env)
                             #f)]
;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                         (if (pair? vs)
                             (k (cdr vs) env)
;If r is seq, and vs is a pair, we should traverse the left side of vs, and the right side of vs. 
                             #f)]
                        [(is-seq? r) 
                         (visit (seq-1 r) vs env
                                (lambda (vs1 env1)
                                  (visit (seq-2 r) vs1 env1 k)))]
;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (or (visit (disj-2 r) vs env k)
                             (visit (disj-1 r) vs env k))]
                        [(is-star? r)
                         (letrec ([loop (lambda (vs env)
                                          (or (visit (star-1 r) vs env
                                                     (lambda (vs1 env1)
                                                       (and (not (equal? vs vs1))
                                                            (loop vs1 env1)
                                                            )))
                                              (k vs env)))])
                           (loop vs env))]
                        [(is-plus? r)
                         (letrec ([loop (lambda (vs env)
                                          (visit (plus-1 r) vs env
                                                 (lambda (vs1 env1)
                                                   (and (not (equal? vs vs1))
                                                        (or (loop vs1 env1)
                                                            (k vs1 env1))))))])
                           (loop vs env))]
                        [(is-var? r)
                         (letrec ( [get-from-env
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
                                         #f]))])
                           (and (pair? vs)
                                (or (and (not (get-from-env (var-1 r) env))
                                         (k (cdr vs)
                                            (cons (cons (var-1 r)
                                                        (car vs))
                                                  env)))
                                    (and (equal?  (get-from-env (var-1 r) env)
                                                  (car vs))
                                         (k (cdr vs) env)))))]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result
                          "Not a recognized expression, please consult BNF:  ~s"
                          r)]))])
      (visit reg vs '()
             (lambda (x env)
               (if (null? x)
                   env
                   #f))))))
;;;;;;;;
(unless (test-interpret-regular-expression-generic interpret-regular-expression-right-most-result #f)
  (printf "Regex mismatch in right most"))

(unless (test-interpret-regular-expression-rightmost interpret-regular-expression-right-most-result)
  (printf "Result of right-most interpreter does not match the expected value"))

;;;;;;;;;;


(define interpret-regular-expression-numbers
  (lambda (reg vs)
    (letrec ([visit (lambda (r vs env k)
                      (cond
                                        ;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (k vs env 1)]
                                        ;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (if (and (pair? vs)
                                  (= (car vs) (atom-1 r)))
                             (k (cdr vs) env 1)
                             0)]
                                        ;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                         (if (pair? vs)
                             (k (cdr vs) env 1)
                                        ;If r is seq, and vs is a pair, we should traverse the left side of vs, and the right side of vs. 
                             0)]
                        [(is-seq? r) 
                         (visit (seq-1 r) vs env
                                (lambda (vs1 env1 c1)
                                  (visit (seq-2 r) vs1 env1 
                                         (lambda (vs2 env2 c2)
                                           (k vs2 env2 (max c1 c2))))))]
                                        ;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (let* ([v1 (visit (disj-2 r) vs env k)]
                                [v2 (visit (disj-1 r) vs env k)])
                           (+ v1 v2))]
                        [(is-star? r) 
                         (letrec ([loop (lambda (vs env)
                                          (let* ([v1 (visit (star-1 r) vs env
                                                            (lambda (vs1 env1 c1)
                                                              (and (not (equal?
                                                                         vs vs1))
                                                                   (loop vs1
                                                                         env1)
                                                                   )))]
                                                 [v2 (k vs env 1)])
                                            (+ v1 v2 )))])
                           (loop vs env))]
                        [(is-plus? r) 
                         (letrec ([loop (lambda (vs env)
                                          (visit (plus-1 r) vs env
                                                 (lambda (vs1 env1 c1)
                                                   (and (not (equal? vs vs1))
                                                        (let* ([v1 (loop vs1
                                                                         env1)]
                                                               [v2 (k vs1
                                                                      env1 c1)])
                                                          (+ v1 v2)
                                                          )))))])
                           (loop vs env))]
                        [(is-var? r)
                         (letrec ([get-from-env
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
                                        #f]))])
                           (or (and (pair? vs)
                                    (or (and (not (get-from-env (var-1 r) env))
                                             (k (cdr vs)
                                                (cons (cons (var-1 r)
                                                            (car vs))
                                                      env) 1))
                                        (and (equal?  (get-from-env (var-1 r) env)
                                                      (car vs))
                                             (k (cdr vs) env 1))))
                               0))]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result
                          "Not a recognized expression, please consult BNF:  ~s"
                          r)]))])
      (visit reg vs '()
             (lambda (x env c)
               (if (null? x)
                   c
                   0))))))



;;;;;;;;;;;

(unless (test-interpret-regular-expression-generic interpret-regular-expression-numbers 0)
  (printf "Regex mismatch in numbers."))

(unless (test-interpret-regular-expression-number interpret-regular-expression-numbers)
  (printf "Result of numbers interpreter does not match the expected value"))

(define interpret-regular-expression-all-results
  (lambda (reg vs)
    (letrec ([visit (lambda (r vs env k)
                      (cond
                                        ;If r is empty, and vs is empty too, we should return an empty list
                        [(is-empty? r)
                         (k vs env)]
                                        ;If r is atom, and the prefix of vs matches, we should return the rest of vs
                        [(is-atom? r) 
                         (if (and (pair? vs)
                                  (= (car vs) (atom-1 r)))
                             (k (cdr vs) env)
                             '())]
                                        ;If r is any, and the prefix of vs is a number, we should return the rest of vs
                        [(is-any? r)
                                        ;If r is seq, and vs is a pair, we should traverse the left side of vs, and the right side of vs. 
                         (if (pair? vs)
                             (k (cdr vs) env)
                             '())]
                        [(is-seq? r) ;seems pretty robust now
                         (visit (seq-1 r) vs env
                                (lambda (vs1 env1)
                                  (visit (seq-2 r) vs1 env1 k)))]
                                        ;If r is disj, in left most, we should first match on the right side of disj, if that fails, match on the left side of disj
                        [(is-disj? r)
                         (let* ([v1 (visit (disj-2 r) vs env k)]
                                [v2 (visit (disj-1 r) vs env k)])
                           (and (or v1
                                    v2)
                                (append (or v1 '())
                                        (or v2 '()))))]
                        [(is-star? r) 
                         (letrec ([loop (lambda (vs env)
                                          (let* ([v1 (visit
                                                      (star-1 r) vs env
                                                      (lambda (vs1 env1)
                                                        (and (not (equal? vs
                                                                          vs1))
                                                             (loop vs1 env1)
                                                             )))]
                                                 [v2 (k vs env)])
                                            (and (or v1
                                                     v2)
                                                 (append (or v1 '())
                                                         (or v2 '())))))])
                           (loop vs env))]
                        [(is-plus? r)
                         (letrec ([loop (lambda (vs env)
                                          (visit (plus-1 r) vs env
                                                 (lambda (vs1 env1)
                                                   (and (not (equal? vs vs1))
                                                        (let* ([v1 (loop vs1
                                                                         env1)]
                                                               [v2 (k vs1 env1)])
                                                          (and (or v1
                                                                   v2)
                                                               (append (or v1
                                                                           '())
                                                                       (or v2
                                                                           '()))
                                                               ))))))])
                           (loop vs env))]
                        [(is-var? r)
                         (letrec ([get-from-env
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
                                        #f]))])
                           (and (pair? vs)
                                (or (and (not (get-from-env (var-1 r) (car env)))
                                         (k (cdr vs)
                                            (cons (cons (cons (var-1 r)
                                                              (car vs))
                                                        (car env))
                                                  (cdr env))))
                                    (and (equal?  (get-from-env (var-1 r)
                                                                (car env))
                                                  (car vs))
                                         (k (cdr vs) env)))))]
                        [else
                         (errorf
                          'interpret-regular-expression-left-most-result
                          "Not a recognized expression, please consult BNF:  ~s"
                          r)]))])
      (visit reg vs '(())
             (lambda (x env)
               (if (null? x)
                   env
                   '()))))))



;;;;;;;;;;;

(unless (test-interpret-regular-expression-generic interpret-regular-expression-all-results '())
  (printf "Regex mismatch in all-results."))

(unless (test-interpret-regular-expression-solutions interpret-regular-expression-all-results)
  (printf "Result of all-results interpreter does not match the expected value"))

(define make-variable
  (lambda (stub)
    (cond
      [(symbol? stub)
       stub]
      [(string? stub)
       (let ([x (gensym)])
         (begin
           (parameterize ([gensym-prefix stub])
                         (symbol->string x))
           x))]
      [else
       (errorf 'make-variable
               "neither a string, nor a symbol: ~s"
               stub)])))

;;; Petite Chez Scheme paraphernalia:

(print-gensym #f)

(define reset-gensym-count
  (lambda ()
    (gensym-count 0)))

;;;;;;;;;;;;;;;;;;;;

(define interpret-regular-expression-left-most-result_Magritte
  (lambda (reg)
    (begin
      (reset-gensym-count)
      (letrec ([visit (lambda (r vs k)
                        (cond
                                        ;If r is empty we run our continuation on vs and our environment.
                          [(is-empty? r)
                           (k vs)]
                                        ;If r is an atom, we check if it matches the prefix of vs and continue with the suffix.
                          [(is-atom? r) 
                           `(and (pair? ,vs)
                                 (= (car ,vs) ,(atom-1 r))
                                 ,(k `(cdr ,vs)))]
                                        ;If r is any, and the prefix of vs is a number, we should continue on the corresponding suffix of vs.
                          
                          [(is-any? r)
                           `(and (pair? ,vs)
                                 ,(k `(cdr ,vs)))]
                                        ;If r is seq, and vs is a pair, we should traverse the left side of vs, and the right side of vs. 
                          [(is-seq? r)
                           (visit (seq-1 r) vs
                                  (lambda (vs1)
                                    (visit (seq-2 r) vs1 k)))]
                                        ;If r is disj, for left-most we visit the left part of the disjunction first, and should that not match, the right part.
                          [(is-disj? r)
                           `(or ,(visit (disj-1 r) vs k)
                                ,(visit (disj-2 r) vs k))]
                                        ;If r is star, we try to match it to as short a match as possible. This is done by either matching the rest of the regular expression to vs, or if that fails, match r with the prefix of vs and continue with the suffix. This is then looped, until the shortest match is found.
                          [(is-star? r)
                           (let ([vs0 (make-variable "vs")]
                                 [loop (make-variable "loop")])
                             `(letrec ([,loop (lambda (,vs0)
                                                (or ,(k vs0)
                                                    ,(visit (star-1 r) vs0
                                                            (lambda (vs1)
                                                              `(and (not (equal?
                                                                          ,vs0
                                                                          ,vs1))
                                                                    (,loop ,vs1)
                                                                    )))))])
                                (,loop ,vs)))]
                                        ;If r is plus, we almost follow the same procedure as star. The difference here is that we immediately match r to the prefix of vs and then continue with the suffix. Should this fail, we loop. Additionally we check whether vs has changed when it loops, to avoid infinite looping.
                          [(is-plus? r)
                           (let ([vs0 (make-variable "vs")]
                                 [loop (make-variable "loop")])
                             `(letrec ([,loop (lambda (,vs0)
                                                ,(visit (plus-1 r) vs0
                                                        (lambda (vs1)
                                                          `(and (not (equal? ,vs0
                                                                             ,vs1))
                                                                (or ,(k vs1)
                                                                    (,loop ,vs1)
                                                                    )))))])
                                (,loop ,vs)))]
                                        ;If r is var, we check whether it matches any elements already in our environment, and if not, it extends the environment.
                                        ;It might be better to merge is-in-env? and get-from-env, since they both go through the same list and find the same element.
                          [else
                           (errorf
                            'interpret-regular-expression-left-most-result_Magritte
                            "Not a recognized expression, please consult BNF:  ~s"
                            r)]))])
        `(lambda (vs) 
           ,(visit reg `vs
                   (lambda (x)
                     `(null? ,x)
                     )))))))

(unless (test-interpret-regular-expression_Magritte_generic interpret-regular-expression-left-most-result_Magritte)
  (printf "Regex mismatch in left-most_Magritte."))

;;;;;;;;;;;
(define fold-right-regular-expression
  (lambda (case-empty case-atom case-any case-seq
                      case-disj case-star case-plus case-var case-else)
    (lambda (reg)
      (letrec ([visit (lambda (r)
                        (cond
                          [(is-empty? r)
                           (case-empty)
                           ]
                          [(is-atom? r)
                           (case-atom (atom-1 r))
                           ]
                          [(is-any? r)
                           (case-any)]
                          [(is-seq? r)
                           (case-seq  (visit (seq-1 r))
                                      (visit (seq-2 r)))]
                          [(is-disj? r)
                           (case-disj (visit (disj-1 r))
                                      (visit (disj-2 r))) 
                           ]
                          [(is-star? r)
                           (case-star (visit (star-1 r)))] 
                          [(is-plus? r)
                           (case-plus (visit (plus-1 r)))] 
                          [(is-var? r)
                           (case-var (var-1 r))] 
                          [else
                           (case-else r)
                           ]))])
        (visit reg)))))
;;;;;
(define interpret-regular-expression-left-most-result_fold-right
  (lambda (r vs)
     ( ((fold-right-regular-expression
        (lambda () ;empty
          (lambda (vs env k)
            (k vs env)))
        (lambda (r) ;atom
          (lambda (vs env k)
            (if (and (pair? vs)
                     (= (car vs) r))
                (k (cdr vs) env)
                #f)))
        (lambda () ;any
          (lambda (vs env k)
            (if (pair? vs)
                (k (cdr vs) env)
                #f)))
        (lambda (r1 r2) ;seq
          (lambda (vs env k)
            (r1 vs env
                (lambda (vs1 env1)
                  (r2 vs1 env1 k)))))
        (lambda (r1 r2) ;disj
          (lambda (vs env k)
            (or (r1 vs env k)
                (r2 vs env k))))
        (lambda (r) ;star
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (or (k vs env)
                                 (r vs env (lambda (vs1 env1)
                                             (and (not (equal? vs vs1))
                                                  (loop vs1 env1))))))])
              (loop vs env))))
        (lambda (r) ;plus
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (r vs env (lambda (vs1 env1)
                                         (and (not (equal? vs vs1))
                                              (or (k vs1 env1)
                                                  (loop vs1 env1))))))])
              (loop vs env))))
        (lambda (r) ;var
          (lambda (vs env k)
            (letrec ([get-from-env
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
                           #f]))])
              (and (pair? vs)
                   (or (and (not (get-from-env r env))
                            (k (cdr vs)
                               (cons (cons r
                                           (car vs))
                                     env)))
                       (and (equal? (get-from-env r env)
                                    (car vs))
                            (k (cdr vs) env)))))))
        (lambda (r) ;else
          (errorf
           'interpret-regular-expression-left-most-result_fold-right
           "Not a recognized expression, please consult BNF: ~s"
           r)
          )) r)
      vs '() (lambda (vs env)
               (if (null? vs)
                   env
                   #f))
      )))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-left-most-result_fold-right #f)
  (printf "Regex mismatch in left-most."))

(unless (test-interpret-regular-expression-leftmost interpret-regular-expression-left-most-result_fold-right)
  (printf "Result of left-most interpreter does not match the expected value"))

;;;;;;;;;;;;;
(define interpret-regular-expression-right-most-result_fold-right
  (lambda (r vs)
     ( ((fold-right-regular-expression
        (lambda () ;empty
          (lambda (vs env k)
            (k vs env)))
        (lambda (r) ;atom
          (lambda (vs env k)
            (if (and (pair? vs)
                     (= (car vs) r))
                (k (cdr vs) env)
                #f)))
        (lambda () ;any
          (lambda (vs env k)
            (if (pair? vs)
                (k (cdr vs) env)
                #f)))
        (lambda (r1 r2) ;seq
          (lambda (vs env k)
            (r1 vs env
                (lambda (vs1 env1)
                  (r2 vs1 env1 k)))))
        (lambda (r1 r2) ;disj
          (lambda (vs env k)
            (or (r2 vs env k)
                (r1 vs env k))))
        (lambda (r) ;star
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (or (r vs env (lambda (vs1 env1)
                                             (and (not (equal? vs vs1))
                                                  (loop vs1 env1))))
                                 (k vs env)))])
              (loop vs env))))
        (lambda (r) ;plus
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (r vs env (lambda (vs1 env1)
                                         (and (not (equal? vs vs1))
                                              (or (loop vs1 env1)
                                                  (k vs1 env1))))))])
              (loop vs env))))
        (lambda (r) ;var
          (lambda (vs env k)
            (letrec ([get-from-env
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
                           #f]))])
              (and (pair? vs)
                   (or (and (not (get-from-env r env))
                            (k (cdr vs)
                               (cons (cons r
                                           (car vs))
                                     env)))
                       (and (equal? (get-from-env r env)
                                    (car vs))
                            (k (cdr vs) env)))))))
        (lambda (r) ;else
          (errorf
           'interpret-regular-expression-right-most-result_fold-right
           "Not a recognized expression, please consult BNF: ~s"
           r)
          )) r)
      vs '() (lambda (vs env)
               (if (null? vs)
                   env
                   #f))
      )))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-right-most-result_fold-right #f)
  (printf "Regex mismatch in right-most_foldright."))

(unless (test-interpret-regular-expression-rightmost interpret-regular-expression-right-most-result_fold-right)
  (printf "Result of right-most_foldright interpreter does not match the expected value"))

;;;;;;;;;;;;;
(define interpret-regular-expression-numbers-result_fold-right
  (trace-lambda number_fold (r vs)
     ( ((fold-right-regular-expression
        (lambda () ;empty
          (lambda (vs env k)
            (k vs env 1)))
        (lambda (r) ;atom
          (lambda (vs env k)
            (if (and (pair? vs)
                     (= (car vs) r))
                (k (cdr vs) env 1)
                0)))
        (lambda () ;any
          (lambda (vs env k)
            (if (pair? vs)
                (k (cdr vs) env 1)
                0)))
        (lambda (r1 r2) ;seq
          (lambda (vs env k)
            (r1 vs env
                (lambda (vs1 env1 c1)
                  (r2 vs1 env1 (lambda (vs2 env2 c2)
                                 (k vs2 env2 (max c1 c2))))))))
        (lambda (r1 r2) ;disj
          (lambda (vs env k)
            (+ (r1 vs env k)
               (r2 vs env k))))
        (lambda (r) ;star
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (+ (r vs env (lambda (vs1 env1 c1)
                                            (and (not (equal? vs vs1))
                                                 (loop vs1 env1))))
                                (k vs env 1)))])
              (loop vs env))))
        (lambda (r) ;plus
          (lambda (vs env k)
            (letrec ([loop (lambda (vs env)
                             (r vs env (lambda (vs1 env1 c1)
                                         (and (not (equal? vs vs1))
                                              (+ (loop vs1 env1)
                                                 (k vs1 env1 c1))))))])
              (loop vs env))))
        (lambda (r) ;var
          (trace-lambda var (vs env k)
            (letrec ([get-from-env
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
                           #f]))])
              (or (and (pair? vs)
                   (or (and (not (get-from-env r env))
                            (k (cdr vs)
                               (cons (cons r
                                           (car vs))
                                     env) 1))
                       (and (equal? (get-from-env r env)
                                    (car vs))
                            (k (cdr vs) env 1)))))
              0)))
        (lambda (r) ;else
          (errorf
           'interpret-regular-expression-right-most-result_fold-right
           "Not a recognized expression, please consult BNF: ~s"
           r)))
        r)
       vs '() (lambda (vs env c)
                (if (null? vs)
                    c
                    0))
       )))

(unless (test-interpret-regular-expression-generic interpret-regular-expression-numbers-result_fold-right 0)
  (printf "Regex mismatch in numbers_foldright."))

(unless (test-interpret-regular-expression-number interpret-regular-expression-numbers-result_fold-right)
  (printf "Result of numbers_foldright interpreter does not match the expected value"))


;;;;;;;;;;;

;;; end of RegexInterpreter.scm

"RegexInterpreter.scm"


