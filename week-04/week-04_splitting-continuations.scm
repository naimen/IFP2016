;;; week-04_splitting-continuations.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo
;;; Version of 20 Sep 2016

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

;;; end of week-04_splitting-continuations.scm

"week-04_splitting-continuations.scm"
