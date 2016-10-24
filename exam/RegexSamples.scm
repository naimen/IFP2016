;;; Relistample.scm
;;; IFP 2016-2016, Q1
;;; Markus, Rasmus, Shuo


;;;;;;;;;;

(define re0
  (cons '(empty)
        '()))
(define re0_n
  (cons '(empty)
        '(1)))

(define re1
  (cons '(any)
        '(10)))
(define re1_n
  (cons '(any)
        '()))

(define re2
  (cons '(atom 10)
        '(10)))
(define re2_n
  (cons '(atom 10)
        '(20)))

(define re3
  (cons '(seq (any) (atom 20))
        '(10 20)))
(define re3_n
  (cons '(seq (any) (atom 20))
        '(10 30)))

(define re4
  (cons '(disj (seq (atom 10) (any)) (atom 20))
        '(10 20)))
(define re4-1
  (cons '(disj (seq (atom 10) (any)) (atom 20))
        '(20)))
(define re4_n
  (cons '(disj (seq (atom 10) (any)) (atom 20))
        '(20 10)))

(define re5
  (cons '(star (disj (seq (any) (any)) (atom 10)))
        '(10 10 10 10 10)))
(define re5-1
  (cons '(star (disj (seq (any) (any)) (atom 10)))
        '(30 50)))
(define re5-2
  (cons '(star (disj (seq (any) (any)) (atom 10)))
        '()))
(define re5_n
  (cons '(star (disj (seq (any) (any)) (atom 10)))
        '(30)))

(define re6
  (cons '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10)))
(define re6-1
  (cons '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10 20)))
(define re6-2 
  (cons '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(10 10 10)))
(define re6_n
  (cons '(plus (disj (atom 10) (seq (atom 10) (star (any)))))
        '(20)))

(define re7
  (cons '(seq (plus (var x)) (atom 10))
        '(20 20 20 20 10)))
(define re7_n
  (cons '(seq (plus (var x)) (atom 10))
        '(10)))

(define re8
  (cons '(seq (atom 10) (seq (var x) (atom 30)))
        '(10 20 30)))
(define re8_n
  (cons '(seq (atom 10) (seq (var x) (atom 30)))
        '(10 30 20)))

(define re9
  (cons '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10 20)))
(define re9-1
  (cons '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x))))
  '(20 10 20)))
(define re9-2
  (cons '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10 20 30 40 50)))
(define re9_n
  (cons '(seq (disj (var x) (star (any)))
              (plus (seq (var z) (var x))))
        '(10)))

(define re10
  (cons '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(10 10 20)))
(define re10-1
  (cons '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(50 10 10 20 60)))
  (define re10-2
    (cons '(seq (star (any))
                (seq
                 (seq
                  (seq (var a)
                       (atom 10))
          (atom 20))
                 (star (any))))
          '(10 20 10 20 10)))

(define re10_n
  (cons '(seq (star (any))
              (seq
               (seq
                (seq (var a)
               (atom 10))
                (atom 20))
               (star (any))))
        '(10 20 30 20 10)))

(define re10_n-1
  (cons '(seq (star (any))
              (seq
               (seq
                (seq (var a)
                     (atom 10))
                (atom 20))
               (star (any))))
        '(10 20 10)))

(define re11
  (cons '(seq (star (any)) (star (any)))
        '(10 20)))

(define re12
  (cons '(disj (var x) (var y))
        '(10)))

(define re13
  (cons '(seq (seq (star (any)) (var x)) (star (any)))
        '(10 20 30)))

(define re14
  (cons '(seq (seq (atom 10) (any)) (atom 20)) 
        '(10 20 20)))
(define re14_n
  (cons '(seq (seq (atom 10) (any)) (atom 20)) 
        '(10 20 20 20)))

(define re15
  (cons '(seq (seq (atom 10) (any)) (seq (any) (atom 20)))
        '(10 10 20 20)))
(define re15-1
  (cons '(seq (seq (atom 10) (any)) (seq (any) (atom 20)))
        '(10 20 30 20)))
(define re15_n
  (cons '(seq (seq (atom 10) (any)) (seq (any) (atom 20)))
        '(10 10 20 30)))
(define re16
  (cons '(disj (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(10)))
(define re16-1
  (cons '(disj (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(20 30 40 50 10)))
(define re16-2
  (cons '(disj (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(10 20 30)))
(define re16_n
  (cons '(disj (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(20 10 30)))

(define re17
  (cons '(seq (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(20 10 10)))
(define re17-1
  (cons '(seq (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(20 10 10 20)))
(define re17_n
  (cons '(seq (seq (star (any)) (atom 10)) (seq (plus (atom 10)) (star (any))))
        '(20 10 20)))

(define re18
  (cons '(star (seq (disj (seq (atom 10) (any)) (seq (any) (atom 10))) (any)))
        '(10 20 10 20 10 20)))
(define re18-1
  (cons '(star (seq (disj (seq (atom 10) (any)) (seq (any) (atom 10))) (any)))
        '(10 10 10 10 10 10)))
(define re18_n
  (cons '(star (seq (disj (seq (atom 10) (any)) (seq (any) (atom 10))) (any)))
        '(30 20 10 20 10 20)))
(define re19
  (cons '(seq (star (any)) (any))
        '(10)))
(define re20
  (cons '(disj (seq (var x) (any)) (plus (seq (var y) (atom 10))))
        '(10 10)))
(define re20-1
  (cons '(disj (seq (var x) (any)) (plus (seq (var y) (atom 10))))
        '(10 10 10 10)))
(define re20_n
  (cons '(disj (seq (var x) (any)) (plus (seq (var y) (atom 10))))
        '(10 10 20 10)))

(define re21
  (cons '(star (disj (atom 10) (seq (atom 10) (atom 10))))
        '(10)))
(define re21-1
  (cons '(star (disj (atom 10) (seq (atom 10) (atom 10))))
        '(10 10)))
(define re21-2
  (cons '(star (disj (atom 10) (seq (atom 10) (atom 10))))
        '(10 10 10)))
(define re21-3
  (cons '(star (disj (atom 10) (seq (atom 10) (atom 10))))
        '(10 10 10 10)))

(define re22
  (cons '(star (disj (seq (var x) (any)) (seq (star (any)) (var y))))
        '(10 20)))




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
        re10-2
        re11
        re12
        re13
        re14
        re15
        re15-1
        re16
        re16-1
        re16-2
        re17
        re17-1
        re18
        re18-1
        re19
        re20
        re20-1
        re21
        re21-1
        re21-2
        re21-3
        re22
        ))

(define sample-of-regular-expressions-leftmost
  (list (cons re7 '((x . 20)))
        (cons re8 '((x . 20)))
        (cons re9 '((x . 20) (z . 10)))
        (cons re9-1 '((z . 10) (x . 20)))
        (cons re9-2 '((x . 50) (z . 40)))
        (cons re10 '((a . 10)))
        (cons re10-1 '((a . 10)))
        (cons re10-2 '((a . 20)))
        (cons re12 '((x . 10)))
        (cons re13 '((x . 10)))
        (cons re20 '((x . 10)))
        (cons re20-1 '((y . 10)))
        (cons re22 '((x . 10)))
        ))

(define sample-of-regular-expressions-rightmost
  (list (cons re7 '((x . 20)))
        (cons re8 '((x . 20)))
        (cons re9 '((x . 20) (z . 10)))
        (cons re9-1 '((x . 20) (z . 10)))
        (cons re9-2 '((x . 50) (z . 40)))
        (cons re10 '((a . 10)))
        (cons re10-1 '((a . 10)))
        (cons re10-2 '((a . 20)))
        (cons re12 '((y . 10)))
        (cons re13 '((x . 30)))
        (cons re20 '((y . 10)))
        (cons re20-1 '((y . 10)))
        (cons re22 '((y . 20)))
        ))



(define sample-of-regular-expressions-with-number
  (list (cons re0 1)
        (cons re1 1)
        (cons re2 1)
        (cons re3 1)
        (cons re4 1)
        (cons re4-1 1)
        (cons re5 8)
        (cons re5-1 1)
        (cons re5-2 1)
		(cons re6 2)
        (cons re6-1 1)
        (cons re6-2 13)
        (cons re7 1)
        (cons re8 1)
        (cons re9 1)
        (cons re9-1 2)
        (cons re9-2 1)
        (cons re10 1)
        (cons re10-1 1)
        (cons re10-2 1)
        (cons re11 3)
        (cons re12 2)
        (cons re13 3)
        (cons re14 1)
        (cons re15 1)
        (cons re15-1 1)
        (cons re16 2)
        (cons re16-1 1)
        (cons re16-2 1)
        (cons re17 1)
        (cons re17-1 1)
        (cons re18 1)
        (cons re18-1 4)
        (cons re19 1)
        (cons re20 2)
        (cons re20-1 1)
        (cons re21 1)
        (cons re21-1 2)
        (cons re21-2 3)
        (cons re21-3 5)
        (cons re22 3)
        ))

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
        re10_n-1
        re14_n
        re15_n
        re16_n
        re17_n
        re18_n
        re20_n
        ))

;;;;;;;;;;

;;; end of RegexSample.scm

"RegexSample.scm"
