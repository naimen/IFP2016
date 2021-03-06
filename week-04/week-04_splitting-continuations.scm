;;; week-04_splitting-continuations.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo
;;; Version of 20 Sep 2016

;;;;;;;;;;
(load "week-03_the-40-optimizing-compilers.scm")
;;;;;;;;;;


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

;;;;;;;;;;

(define test-well-balanced
  (lambda (candidate)
    (and-all ;;; some positive tests:
             (try-candidate 'test-well-balanced
                            candidate
                            #t
                            1)
             (try-candidate 'test-well-balanced
                            candidate
                            #t
                            (cons 1
                                  1))
             (try-candidate 'test-well-balanced
                            candidate
                            #t
                            (cons (cons 1
                                        1)
                                  2))
             ;;; and some negative tests:
             (try-candidate 'test-well-balanced
                            candidate
                            #f
                            (cons (cons 1
                                        1)
                                  3))
             ;;; etc.
             )))             

;;;;;;;;;;

(define well-balanced?_v0
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (let ([v1 (visit (car v))])
                           (if v1
                               (let ([v2 (visit (cdr v))])
                                  (if v2
                                      (if (= v1 v2)
                                          (+ v1 v2)
                                          #f)
                                      #f))
                               #f))]
                        [else
                         (errorf 'well-balanced?_v0
                                 "improper input ~s"
                                 v)]))])
      (number? (visit v_init)))))

(unless (test-well-balanced well-balanced?_v0)
  (printf "fail: (test-well-balanced well-balanced?_v0)~n"))

;;;;;;;;;;

(define well-balanced?_v1
  (lambda (v_init)
    (letrec ([visit (lambda (v k)
                      (cond
                        [(number? v)
                         (k v)]
                        [(pair? v)
                         (visit (car v)
                                (lambda (v1)
                                  (if v1
                                      (visit (cdr v)
                                             (lambda (v2)
                                               (if v2
                                                   (if (= v1 v2)
                                                       (k (+ v1 v2))
                                                       (k #f))
                                                   (k #f))))
                                      (k #f))))]
                        [else
                         (errorf 'well-balanced?_v1
                                 "improper input ~s"
                                 v)]))])
      (visit v_init (lambda (v) (number? v))))))

(unless (test-well-balanced well-balanced?_v1)
  (printf "fail: (test-well-balanced well-balanced?_v1)~n"))

;;;;;;;;;;

(define well-balanced?_v2
  (lambda (v_init)
    (letrec ([visit (lambda (v kn kb)
                      (cond
                        [(number? v)
                         (kn v)]
                        [(pair? v)
                         (visit (car v)
                                (lambda (n1)
                                  (visit (cdr v)
                                         (lambda (n2)
                                           (if (= n1 n2)
                                               (kn (+ v1 v2))
                                               (kb)))
                                         (lambda ()
                                           (kb))))
                                (lambda ()
                                  (kb)))]
                        [else
                         (errorf 'well-balanced?_v2
                                 "improper input ~s"
                                 v)]))])
      (visit v_init (lambda (n) #t)))))

(unless (test-well-balanced well-balanced?_v1)
  (printf "fail: (test-well-balanced well-balanced?_v1)~n"))

;;;;;;;;;;
(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v0?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising_v0 ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v0?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v0?
            sample-of-arithmetic-expressions)))

;;;;;;;;
(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v1?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising_v1 ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v1?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v1?
            sample-of-arithmetic-expressions)))
;;;;;;;;;
(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v2?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising_v2 ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v2?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v2?
            sample-of-arithmetic-expressions)))

;;;;;;;;;
(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v3?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising_v3 ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v3?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v3?
            sample-of-arithmetic-expressions)))

(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v4?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising_v4 ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v4?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v4?
            sample-of-arithmetic-expressions)))


;;;;;;;;;;
(define interpret-arithmetic-expression_Magritte_surprising_v0
  (lambda (e_init)
    (letrec ([visit (lambda (e)
                      (cond
                        [(is-literal? e)
                         (make-literal (literal-1 e))]
                        [(is-plus? e)
                         (let ([e1 (visit (plus-1 e))])
                           (if (and (is-literal? e1)
                                    (= (literal-1 e1) 0))
                               (visit (plus-2 e))

                               (let ([e2 (visit (plus-2 e))])
                                 (if (and (is-literal? e2)
                                          (= (literal-1 e2) 0))
                                     e1
                                     (make-plus e1 e2)))))]

                        [(is-times? e)
                         (let ([e1 (visit (times-1 e))])
                           (cond
                             [(and (is-literal? e1)
                                   (= (literal-1 e1) 0))
                              (make-literal 0)]

                             [(and (is-literal? e1)
                                   (= (literal-1 e1) 1))
                              (visit (times-2 e))]

                             [else
                              (let ([e2 (visit (times-2 e))])
                                (cond
                                  [(and (is-literal? e2)
                                        (= (literal-1 e2) 0))
                                   (make-literal 0)]

                                  [(and (is-literal? e2)
                                        (= (literal-1 e2) 1))
                                   e1]
                                  [else
                                   (make-times e1 e2)]))]))]))])
      (visit e_init))))

(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v0?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v0?)~n"))

;;;;;;;;
(define interpret-arithmetic-expression_Magritte_surprising_v1
  (lambda (e_init)
    (letrec ([visit (lambda (e k)
                      (cond
                        [(is-literal? e)
                         (k (make-literal (literal-1 e)))]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (lambda (e1)
                                  (if (and (is-literal? e1)
                                           (= (literal-1 e1) 0))
                                      (visit (plus-2 e) k)
                                      (visit (plus-2 e)
                                             (lambda (e2)
                                               (if (and (is-literal? e2)
                                                        (= (literal-1 e2) 0))
                                                   (k e1)
                                                   (k (make-plus e1 e2))))))))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (lambda (e1)
                                  (cond
                                    [(and (is-literal? e1) (= (literal-1 e1) 0))
                                     (k (make-literal 0))]
                                    [(equal? e1 '(literal 1))
                                     (visit (times-2 e) k)]
                                    [else
                                     (visit (times-2 e)
                                            (lambda (e2)
                                              (cond
                                                [(equal? e2 '(literal 0))
                                                 (k (make-literal 0))]
                                                [(and (is-literal? e2) (= (literal-1 e2) 1))
                                                 (k e1)]
                                                [else
                                                 (k (make-times e1 e2))])))])))]))])
      (visit e_init (lambda (a) a)))))

(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v1?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v1?)~n"))

(define interpret-arithmetic-expression_Magritte_surprising_v2
  (trace-lambda entering (e_init)
    (letrec ([visit (lambda (e k0 k1 kn)
                      (cond
                        [(is-literal? e)
                         (kn (make-literal (literal-1 e)))]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (trace-lambda p0 ()
                                  (visit (plus-2 e) k0 k1 kn))
                                (trace-lambda p1 ()
                                  (visit (plus-2 e)
                                         (lambda ()
                                           (k1))
                                         (lambda ()
                                           (kn (make-literal 2)))
                                         (lambda (e2)
                                           (if (and (is-literal? e2)
                                                    (= (literal-1 e2) 0))
                                               (k0)
                                               (if (and (is-literal? e2)
                                                        (= (literal-1 e2) 1))
                                                   (k1)
                                                   (kn (make-plus
                                                        (make-literal 1)
                                                        e2)))))))
                                (lambda (e1)
                                  (if (and (is-literal? e1)
                                           (= (literal-1 e1) 0))
                                      (k0)
                                      (if (and (is-literal? e1)
                                               (= (literal-1 e1) 1))
                                          (k1)
                                          (visit (plus-2 e) 
                                                 (lambda ()
                                                   (kn (make-literal
                                                        (literal-1 e1))))
                                                 (lambda ()
                                                   (kn (make-plus
                                                        e1
                                                        (make-literal 1))))
                                                 (lambda (e2)
                                                   (if (and (is-literal? e2)
                                                            (= (literal-1 e2) 0))
                                                       (k0)
                                                       (if (and (is-literal? e2)
                                                                (= (literal-1 e2)
                                                                   1))
                                                           (k1)
                                                           (kn
                                                            (make-plus (e1 e2))
                                                            )))))))))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (lambda ()
                                  (k0))
                                (lambda ()
                                  (visit (times-2 e) k0 k1 kn))
                                (lambda (e1)
                                  (if (and (is-literal? e1)
                                           (= (literal-1 e1) 0))
                                      (k0)
                                      (if (and (is-literal? e1)
                                               (= (literal-1 e1) 1))
                                          (k1)
                                          (visit (times-2 e)
                                                 (lambda () 
                                                   (kn (make-literal 0)))
                                                 (lambda () 
                                                   (kn (make-literal
                                                        (literal-1 e1))))
                                                 (lambda (e2)
                                                   (if (and (is-literal? e2)
                                                            (= (literal-1 e2) 0))
                                                       (k0)
                                                       (if (and (is-literal? e2)
                                                                (= (literal-1 e2)
                                                                   1))
                                                           (k1)
                                                           (kn (make-times
                                                                (e1 e2)))
                                                           ))))))))]))])
      (visit e_init
             (lambda ()
               (make-literal 0))
             (lambda ()
               (make-literal 1))
             (lambda (e)
               e)))))
;;(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v2?)
  ;;(printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v2?)~n"))

(define interpret-arithmetic-expression_Magritte_surprising_v3
  (lambda (e_init)
    (letrec ([visit (lambda (e k0 k1 kn)
                      (cond
                        [(is-literal? e)
                         (kn (make-literal (literal-1 e)))]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                k0
                                k1
                                (lambda (e1)
                                  (if (equal? e1 '(literal 0))
                                      (visit (plus-2 e) k0 k1 kn)
                                      (visit (plus-2 e)
                                             k0
                                             k1
                                             (lambda (e2)
                                               (if (equal? e2 '(literal 0))
                                                   (kn e1)
                                                   (kn (make-plus e1 e2))))))
                                  ))]
                        [(is-times? e)
                         (visit (times-1 e)
                                k0
                                k1
                                (lambda (e1)
                                  (cond
                                    [(and (is-literal? e1) (= (literal-1 e1) 0))
                                     (kn (k0))]
                                    [(equal? e1 '(literal 1))
                                     (visit (times-2 e) k0 k1 kn)]
                                    [else
                                     (visit (times-2 e)
                                            k0
                                            k1
                                            (lambda (e2)
                                              (cond
                                                [(equal? e2 '(literal 0))
                                                 (kn (k0))]
                                                [(equal? e2 '(literal 1))
                                                 (kn (k1))]
                                                [else
                                                 (kn (make-times e1 e2))])))])))]
                        ))])
      (visit e_init
             (lambda ()
               (make-literal 0))
             (lambda ()
               (make-literal 1))
             (lambda (e)
               e)))))

(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v3?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v3?)~n"))

(define interpret-arithmetic-expression_Magritte_surprising_v4
  (lambda (e_init)
    (letrec ([visit (lambda (e k0 k1 kn)
                      (cond
                        [(is-literal? e)
                         (cond
                           [(equal? e '(literal 0))
                            (k0)]
                           [(equal? e '(literal 1))
                            (k1)]
                           [else
                            (kn (make-literal (literal-1 e)))])]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (lambda () ;; k0
                                  (visit (plus-2 e)
                                         (lambda () ;; k0
                                           (k0))
                                         (lambda () ;; k1
                                           (k1))
                                         (lambda (e2) ;; kn
                                           (kn e2))))
                                (lambda () ;; k1
                                  (visit (plus-2 e)
                                         (lambda () ;; k0
                                           (k1))
                                         (lambda () ;; k1
                                           (kn (make-plus (make-literal 1)
                                                          (make-literal 1))))
                                         (lambda (e2) ;; kn
                                           (kn (make-plus (make-literal 1)
                                                          e2)))))
                                (lambda (e1) ;; kn
                                  (visit (plus-2 e)
                                         (lambda () ;; k0
                                           (kn e1))
                                         (lambda () ;; k1
                                           (kn (make-plus e1
                                                          (make-literal 1))))
                                         (lambda (e2) ;; kn
                                           (kn (make-plus e1 e2))))
                                  ))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (lambda () ;; k0
                                  (k0))
                                (lambda () ;; k1
                                  (visit (plus-2 e)
                                         (lambda () ;; k0
                                           (k0))
                                         (lambda () ;; k1
                                           (k1))
                                         (lambda (e2) ;; kn
                                           (kn e2))))
                                (lambda (e1) ;; kn
                                  (visit (plus-2 e)
                                         (lambda () ;; k0
                                           (k0))
                                         (lambda () ;; k1
                                           (kn e1))
                                         (lambda (e2) ;; kn
                                           (kn (make-times e1 e2))))))]
                        [else
                         (errorf
                          'interpret-arithmetic-expression_Magritte_surprising_v4
                                 "Not a proper arithmetic expression ~s" e)]
                        ))])
      (visit e_init
             (lambda ()
               (make-literal 0))
             (lambda ()
               (make-literal 1))
             (lambda (e)
               e)))))

(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v4?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute_v4?)~n"))


;;; end of week-04_splitting-continuations.scm

"week-04_splitting-continuations.scm"
