;;; RegexSample.scm
;;; IFP 2016-2016, Q1
;;; Markus, Rasmus, Shuo


;;;;;;;;;;

(define re0
  '(empty)
  '())
(define re0_n
  '(empty)
  '(1))

(define re1
  '(any)
  '(10))
(define re1_n
  '(any)
  '())

(define re2
  '(xatom 10)
  '(10))
(define re2_n
  '(atom 10)
  '(20))

(define re3
  '(seq (any) (atom 20))
  '(10 20))
(define re3_n
  '(seq (any) (atom 20))
  '(10 30))

(define re4
  '(disj (seq (atom 10) (any)) (atom 20))
  '(10 20))
(define re4-1
  '(disj (seq (atom 10) (any)) (atom 20))
  '(20))
(define re4_n
  '(disj (seq (atom 10) (any)) (atom 20))
  '(20 10))

(define re5
  '(star (disj (seq (any) (any)) (atom 10)))
  '(10 10 10 10 10))
(define re5-1
  '(star (disj (seq (any) (any)) (atom 10)))
  '(30 50))
(define re5-2
  '(star (disj (seq (any) (any)) (atom 10)))
  '())
(define re5_n
  '(star (disj (seq (any) (any)) (atom 10)))
  '(30))

(define re6
  '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
  '(10))
(define re6-1
  '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
  '(10 20))
(define re6-2 
  '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
  '(10 10 10))
(define re6_n
  '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
  '(20))

(define re7
  '(seq (plus (var x)) (atom 10))
  '(20 20 20 20 10))
(define re7_n
  '(seq (plus (var x)) (atom 10))
  '(10 10))

(define re8
  '(seq (atom 10) (seq (var x) (atom 30)))
  '(10 20 30))
(define re8_n
  '(seq (atom 10) (seq (var x) (atom 30)))
  '(10 30 20))

(define re9
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(10 20))
(define re9-1
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(20 10 20))
(define re9-2
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(10 20 30 40 50))
(define re9_n
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(10))

(define re10
  '(seq (star (any))
        (seq
         (seq
          (seq (var a)
               (atom 10))
          (atom 20))
         (star (any))))
  '(10 10 20))
(define re10-1
  '(seq (star (any))
        (seq
         (seq
          (seq (var a)
               (atom 10))
          (atom 20))
         (star (any))))
  '(50 10 10 20 60))
(define re10-2
  '(seq (star (any))
        (seq
         (seq
          (seq (var a)
               (atom 10))
          (atom 20))
         (star (any))))
  '(10 20 10 20 10))

(define re10_n
  '(seq (star (any))
        (seq
         (seq
          (seq (var a)
               (atom 10))
          (atom 20))
         (star (any))))
  '(10 20 30 20 10))

(define re10_n-1
  '(seq (star (any))
        (seq
         (seq
          (seq (var a)
               (atom 10))
          (atom 20))
         (star (any))))
  '(10 20 10))






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
