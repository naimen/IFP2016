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
;;;;;;

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

(define interpret-regular-expression-right-most-result-2
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


