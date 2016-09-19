;;; week-02_processing-binary-trees-generically.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/lecture-notes/week-02_processing-binary-trees-generically.html

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

;;; <binary-tree> ::= <number>
;;;                 | (<binary-tree> . <binary-tree>)
;;; 
;;; <number> ::= ...any Scheme number...

;;;;;;;;;;

(define fold-right_binary-tree
  (lambda (lea nod err)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(number? v)
                           (lea v)]
                          [(pair? v)
                           (nod (visit (car v))
                                (visit (cdr v)))]
                          [else
                           (err v)]))])
        (visit v_init)))))

;;;;;;;;;;

(define test-check_binary-tree
  (lambda (candidate)
    (and-all ;;; some positive tests:
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            0)
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            (cons 0 0))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #t
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; and some negative tests:
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            (cons (cons 10 20)
                                  (cons 30 "40")))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            (list 1 2 3))
             (try-candidate 'test-check_binary-tree
                            candidate
                            #f
                            'foo)
             ;;; an incorrect test:
             ;; (try-candidate 'test-check_binary-tree
             ;;                candidate
             ;;                #t
             ;;                'foo)
             ;;; etc.
             )))

(define check_binary-tree_alt
  (fold-right_binary-tree (lambda (n)
                            #t)
                          (lambda (b1 b2)
                            (and b1 b2))
                          (lambda (v)
                            #f)))

(unless (test-check_binary-tree check_binary-tree_alt)
  (printf "fail: (test-check_binary-tree check_binary-tree_alt)~n"))

;;;;;;;;;;

(define test-number-of-leaves
  (lambda (candidate)
    (and-all (try-candidate 'test-number-of-leaves
                            candidate
                            1
                            0)
             (try-candidate 'test-number-of-leaves
                            candidate
                            2
                            (cons 0 0))
             (try-candidate 'test-number-of-leaves
                            candidate
                            4
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; etc.
             )))

(define number-of-leaves_alt
  (fold-right_binary-tree (lambda (n)
                            1)
                          (lambda (n1 n2)
                            (+ n1 n2))
                          (lambda (v)
                            (errorf 'number-of-leaves_alt
                                    "not a binary tree: ~s"
                                    v))))

(unless (test-number-of-leaves number-of-leaves_alt)
  (printf "fail: (test-number-of-leaves number-of-leaves_alt)~n"))

;;;;;;;;;;

(define test-number-of-nodes
  (lambda (candidate)
    (and-all (try-candidate 'test-number-of-nodes
                            candidate
                            0
                            0)
             (try-candidate 'test-number-of-nodes
                            candidate
                            1
                            (cons 0 0))
             (try-candidate 'test-number-of-nodes
                            candidate
                            3
                            (cons (cons 10 20)
                                  (cons 30 40)))
             ;;; etc.
             )))

(define number-of-nodes_alt
  (fold-right_binary-tree (lambda (n)
                            0)
                          (lambda (n1 n2)
                            (1+ (+ n1 n2)))
                          (lambda (v)
                            (errorf 'number-of-nodes_alt
                                    "not a binary tree: ~s"
                                    v))))

(unless (test-number-of-nodes number-of-nodes_alt)
  (printf "fail: (test-number-of-nodes number-of-nodes_alt)~n"))

;;;;;;;;;;

(define test-smallest-leaf
  (lambda (candidate)
    (and-all (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            0)
             (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            (cons 0 0))
             (try-candidate 'test-smallest-leaf
                            candidate
                            10
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-smallest-leaf
                            candidate
                            -20
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-smallest-leaf
                            candidate
                            10
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define smallest-leaf_alt
  (fold-right_binary-tree (lambda (n)
                            (errorf 'smallest-leaf_alt
                                    "not implemented yet"))
                          (lambda (n1 n2)
                            (errorf 'smallest-leaf_alt
                                    "not implemented yet"))
                          (lambda (v)
                            (errorf 'smallest-leaf_alt
                                    "not implemented yet"))))

;; (unless (test-smallest-leaf smallest-leaf_alt)
;;   (printf "fail: (test-smallest-leaf smallest-leaf_alt)~n"))

;;;;;;;;;;

(define test-weight
  (lambda (candidate)
    (and-all (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            0)
             (try-candidate 'test-smallest-leaf
                            candidate
                            0
                            (cons 0 0))
             (try-candidate 'test-smallest-leaf
                            candidate
                            100
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-smallest-leaf
                            candidate
                            20
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-smallest-leaf
                            candidate
                            150
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define weight_alt
  (fold-right_binary-tree (lambda (n)
                            n)
                          (lambda (n1 n2)
                            (+ n1 n2))
                          (lambda (v)
                            (errorf 'weight_alt
                                    "not a binary tree: ~s"
                                    v))))

 (unless (test-weight weight_alt)
   (printf "fail: (test-weight weight_alt)~n"))

;;;;;;;;;;

(define test-height
  (lambda (candidate)
    (and-all (try-candidate 'test-height
                            candidate
                            0
                            0)
             (try-candidate 'test-height
                            candidate
                            1
                            (cons 1
                                  1))
             (try-candidate 'test-height
                            candidate
                            2
                            (cons (cons 2
                                        2)
                                  (cons 2
                                        2)))
             (try-candidate 'test-height
                            candidate
                            2
                            (cons (cons 2
                                        2)
                                  1))
             (try-candidate 'test-height
                            candidate
                            3
                            (cons (cons 2
                                        2)
                                  (cons (cons 3
                                              3)
                                        2)))
             (try-candidate 'test-height
                            candidate
                            4
                            (cons (cons 2
                                        (cons (cons 4
                                                    4)
                                              3))
                                  (cons (cons 3
                                              3)
                                        2)))
             ;;; etc.
             )))

(define height_alt
  (fold-right_binary-tree (lambda (n)
                            0)
                          (lambda (n1 n2)
                            (+ 1 (max n1 n2)))
                          (lambda (v)
                            (errorf 'height_alt
                                    "not a binary tree ~s"
                                    v))))

 (unless (test-height height_alt)
   (printf "fail: (test-height height_alt)~n"))

;;;;;;;;;;

(define test-width
  (lambda (candidate)
    (and-all (try-candidate 'test-width
                            candidate
                            1
                            1)
             (try-candidate 'test-width
                            candidate
                            2
                            (cons 1
                                  2))
             (try-candidate 'test-width
                            candidate
                            2
                            (cons (cons 11
                                        12)
                                  2))
             (try-candidate 'test-width
                            candidate
                            4
                            (cons (cons 1
                                        2)
                                  (cons 3
                                        4)))
             (try-candidate 'test-width
                            candidate
                            6
                            (cons (cons (cons 1
                                              2)
                                        0)
                                  (cons (cons 3
                                              4)
                                        (cons 5
                                              6))))
             (try-candidate 'test-width
                            candidate
                            6
                            (cons (cons (cons 1
                                              2)
                                        0)
                                  (cons (cons (cons 31
                                                    32)
                                              4)
                                        (cons 5
                                              (cons 61
                                                    62)))))
             ;;; etc.
             )))


(define zerolist
  (lambda (x)
    (make-list 
     (abs x)
     0)))

(define calc-result
  (lambda (x y)
    (cons 1
            (map (lambda (i j) (+ i j))
                 x y))))

(define width_alt
  (lambda (v_init)
    (apply max ((fold-right_binary-tree (lambda (n)
                                          '(1))
                                        (lambda (n1 n2)
                                          (let ([diff (- (length n1)
                                                         (length n2))])
                                            (if (< diff 0)
                                                (calc-result 
                                                 (append n1 (zerolist diff))
                                                 n2)
                                                (calc-result 
                                                 (append n2 (zerolist diff))
                                                 n1))))
                                        (lambda (v)
                                          (errorf 'width_alt
                                                  "not implemented yet")))
                v_init ))))

(unless (test-width width_alt)
  (printf "fail: (test-width width_alt)~n"))

;;;;;;;;;;

(define test-flatten
  (lambda (candidate)
    (and-all (try-candidate 'test-flatten
                            candidate
                            '(0)
                            0)
             (try-candidate 'test-flatten
                            candidate
                            '(0 1)
                            (cons 0 1))
             (try-candidate 'test-flatten
                            candidate
                            '(10 20 30 40)
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-flatten
                            candidate
                            '(10 -20 30)
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-flatten
                            candidate
                            '(10 20 30 40 50)
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define flatten_alt
  (fold-right_binary-tree (lambda (n)
                             (list n))
                          (lambda (n1 n2)
                            (append n1 n2))
                          (lambda (v)
                            (errorf 'flatten_alt
                                    "not a binary tree ~s"
                                    v))))
 (unless (test-flatten flatten_alt)
   (printf "fail (test-flatten flatten_alt)~n"))
;;;;;;;;;;
(define test-mobile
  (lambda (candidate)
    (and-all (try-candidate 'test-mobile
                            candidate
                            #t
                            0)
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons 0 0))
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons (cons 15 15)
                                  (cons 15 15)))
             (try-candidate 'test-mobile
                            candidate
                            #t
                            (cons (cons 15 15)
                                  30))
             (try-candidate 'test-mobile
                            candidate
                            #f
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

;;; the process outputs a pair, so a help function to exract the boolean value
(define well-balanced?_alt
  (lambda(x) (car (well-balanced?_process x))))

(define well-balanced?_process
  (fold-right_binary-tree (lambda (n)
                            (cons #t n))
                          (lambda (n1 n2)
                            (cons (and (equal? (cdr n1)
                                               (cdr n2))
                                       (and (car n1)
                                            (car n2)))
                                  (+(cdr n1)
                                    (cdr n2))))
                          (lambda (v)
                            (errorf 'well_balanced?_process
                                    "not a binary tree ~s"
                                    v)))))

(unless (test-mobile well-balanced?_alt)
   (printf "fail (test-mobile well-balanced?_alt)~n"))


;;;;;;;;;;

(define test-swap
  (lambda (candidate)
    (and-all (try-candidate 'test-swap
                            candidate
                            0
                            0)
             (try-candidate 'test-swap
                            candidate
                            (cons 0 0)
                            (cons 0 0))
             (try-candidate 'test-swap
                            candidate
                            (cons (cons 40 30)
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-swap
                            candidate
                            (cons 30
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  30))
             (try-candidate 'test-swap
                            candidate
                            (cons (cons 50
                                        (cons 40 30))
                                  (cons 20 10))
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define swap_alt
  (fold-right_binary-tree (lambda (n)
                            (errorf 'swap_alt
                                    "not implemented yet"))
                          (lambda (n1 n2)
                            (errorf 'swap_alt
                                    "not implemented yet"))
                          (lambda (v)
                            (errorf 'swap_alt
                                    "not implemented yet"))))

;; (unless (test-swap swap_alt)
;;   (printf "fail: (test-swap swap_alt)~n"))

;;;;;;;;;;

"week-02_processing-binary-trees-generically.scm"

;;; end of week-02_processing-binary-trees-generically.scm
