;;; RegexSample.scm
;;; IFP 2016-2016, Q1
;;; Markus, Rasmus, Shuo


;;;;;;;;;;

(define re0
  (list '(empty)
        '()))
(define re0_n
  (list '(empty)
        '(1)))

(define re1
  (list '(any)
        '(10)))
(define re1_n
  (list '(any)
        '()))

(define re2
  (list '(atom 10)
        '(10)))
(define re2_n
  (list '(atom 10)
        '(20)))

(define re3
  (list '(seq (any) (atom 20))
        '(10 20)))
(define re3_n
  (list '(seq (any) (atom 20))
        '(10 30)))

(define re4
  (list '(disj (seq (atom 10) (any)) (atom 20))
        '(10 20)))
(define re4-1
  (list '(disj (seq (atom 10) (any)) (atom 20))
        '(20)))
(define re4_n
  (list '(disj (seq (atom 10) (any)) (atom 20))
        '(20 10)))

(define re5
  (list '(star (disj (seq (any) (any)) (atom 10)))
        '(10 10 10 10 10)))
(define re5-1
  (list '(star (disj (seq (any) (any)) (atom 10)))
        '(30 50)))
(define re5-2
  (list '(star (disj (seq (any) (any)) (atom 10)))
        '()))
(define re5_n
  (list '(star (disj (seq (any) (any)) (atom 10)))
        '(30)))

(define re6
  (list '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10)))
(define re6-1
  (list '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10 20)))
(define re6-2 
  (list '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10 10 10)))
(define re6_n
  (list '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(20)))

(define re7
  (list '(seq (plus (var x)) (atom 10))
        '(20 20 20 20 10)))
(define re7_n
  (list '(seq (plus (var x)) (atom 10))
        '(10 10)))

(define re8
  (list '(seq (atom 10) (seq (var x) (atom 30)))
        '(10 20 30)))
(define re8_n
  (list '(seq (atom 10) (seq (var x) (atom 30)))
        '(10 30 20)))

(define re9
  (list '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10 20)))
(define re9-1
  (list '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(20 10 20)))
(define re9-2
  (list '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10 20 30 40 50)))
(define re9_n
  (list '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10)))

(define re10
  (list '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(10 10 20)))
(define re10-1
  (list '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(50 10 10 20 60)))
  (define re10-2
    (list '(seq (star (any))
                (seq
                 (seq
                  (seq (var a)
                       (atom 10))
          (atom 20))
                 (star (any))))
          '(10 20 10 20 10)))

(define re10_n
  (list '(seq (star (any))
              (seq
               (seq
                (seq (var a)
               (atom 10))
                (atom 20))
               (star (any))))
        '(10 20 30 20 10)))

(define re10_n-1
  (list '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(10 20 10)))

;;;;;;;;;;

(define sample-of-regular-expressions
  (list re0
        re1
        re2
        re3
        re4
        re4-1
        re5
        re5-1
        re5-2
        re6
        re6-1
        re6-2
        re7
        re8
        re9
        re9-1
        re9-2
        re10
        re10-1
        re10-2))

(define sample-of-negative-regular-expressions
  (list re0_n
        re1_n
        re2_n
        re3_n
        re4_n
        re5_n
        re6_n
        re7_n
        re8_n
        re9_n
        re10_n
        re10_n-1))

;;;;;;;;;;

;;; end of RegexSample.scm

"RegexSample.scm"
