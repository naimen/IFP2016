;;; week-02_processing-binary-trees.scm
;;; IFP 2016-2017, Q1
;;; Makus, Rasmus, Shuo <201206051>
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/lecture-notes/week-02_processing-binary-trees.html

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
             ;;; other incorrect test:
             ;; (try-candidate 'test-check_binary-tree
             ;;               candidate
             ;;               #f
             ;;               (cons (cons 0 0) 1))
             )))

(define check_binary-tree
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         #t]
                        [(pair? v)
                         (and (visit (car v))
                              (visit (cdr v)))]
                        [else
                         #f]))])
      (visit v_init))))

(unless (test-check_binary-tree check_binary-tree)
  (printf "fail: (test-check_binary-tree check_binary-tree)~n"))

;;; Exercise 3
;;; ----------
;;; 
;;; * Edit the accompanying Scheme file (``C-x C-f
;;;   week-02_processing-binary-trees.scm``), and uncomment the incorrect
;;;   test in the definition of ``test-check_binary-tree``.  Then send the
;;;   content of the file to the Scheme process (``C-c C-b``), and verify
;;;   that the error message ``fail: (test-check_binary-tree
;;;   check_binary-tree)`` is displayed.
;;; 
;;; * In the definition of ``test-check_binary-tree``, the commented-out test
;;;   is incorrect in that the candidate syntax checker allegedly accepts an
;;;   ill-formed value.  Add another incorrect test where the candidate
;;;   syntax checker allegedly rejects a well-formed value, and verify that
;;;   the error message ``fail: (test-check_binary-tree check_binary-tree)``
;;;   is displayed when the content of the file is sent to the Scheme
;;;   process.
;;; 
;;; * Finally, to prevent future confusion, clean up your file by commenting
;;;   out the incorrect tests.

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

(define number-of-leaves
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         1]
                        [(pair? v)
                         (+ (visit (car v))
                            (visit (cdr v)))]
                        [else
                         (errorf 'number-of-leaves
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-number-of-leaves number-of-leaves)
  (printf "fail: (test-number-of-leaves number-of-leaves)~n"))

;;; Exercise 4
;;; ----------
;;; 
;;; Add another test in the definition of ``test-number-of-leaves`` and
;;; verify that ``number-of-leaves`` still passes the unit test.

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

(define number-of-nodes
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         0]
                        [(pair? v)
                         (1+ (+ (visit (car v))
                                (visit (cdr v))))]
                        [else
                         (errorf 'number-of-nodes
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-number-of-nodes number-of-nodes)
  (printf "fail: (test-number-of-nodes number-of-nodes)~n"))

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

(define smallest-leaf
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (min (visit (car v))
                              (visit (cdr v)))]
                        [else
                         (errorf 'smallest-leaf
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-smallest-leaf smallest-leaf)
  (printf "fail: (test-smallest-leaf smallest-leaf)~n"))

;;; Exercise 5
;;; ----------
;;; 
;;; Reminder: ``max`` denotes the predefined Scheme procedure that returns
;;; the largest of its arguments::
;;; 
;;;     > (max 2 3)
;;;     3
;;;     > (max 2 -3)
;;;     2
;;;     > 
;;; 
;;; What would happen if we were to replace ``min`` by ``max`` in the
;;; definition of ``smallest-leaf``?

;;;;;;;;;;

(define test-weight
  (lambda (candidate)
    (and-all (try-candidate 'test-weight
                            candidate
                            0
                            0)
             (try-candidate 'test-weight
                            candidate
                            0
                            (cons 0 0))
             (try-candidate 'test-weight
                            candidate
                            100
                            (cons (cons 10 20)
                                  (cons 30 40)))
             (try-candidate 'test-weight
                            candidate
                            20
                            (cons (cons 10 -20)
                                  30))
             (try-candidate 'test-weight
                            candidate
                            150
                            (cons (cons 10 20)
                                  (cons (cons 30 40)
                                        50)))
             ;;; etc.
             )))

(define weight
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (+ (visit (car v))
                            (visit (cdr v)))]
                        [else
                         (errorf 'weight
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-weight weight)
  (printf "fail: (test-weight weight)~n"))

;;; Exercise 6
;;; ----------
;;; 
;;; Describe how to compute the weight of a binary tree in English.

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

;;; Exercise 7
;;; ----------
;;; 
;;; Describe how to compute the height of a binary tree:
;;; 
;;; * in English:
;;; The height of leaf is 0.
;;; Given a well-formed tree t1 with height h1
;;;  and a well-formed tree t2 with height h2,
;;;  the height of the node  whose left subtree is t1
;;;  and whose right subtree is t2,
;;;  is the result of add 1 to the max between h1 and h2. 
;;; 
;;; * functionally.
;;; For all numbers n, height n=0
;;; For all well-formed binary trees t1 and t2,
;;;    height(cons t1 t2)= 1+ max(height t1, height t2), where max is
;;;    a binary function returning the biggest of its two arguments. 
;;; 
;;; Then define a Scheme procedure ``height`` that given a binary tree,
;;; computes its height.  If it is given another value than a well-formed
;;; binary tree, your procedure should raise an error.  Verify that it passes
;;; the unit test.

(define height
  (lambda (v_init)
     (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         0]
                        [(pair? v)
                         (+ 1 (max  (visit (car v))
                                    (visit (cdr v))))]
                        [else
                         (errorf 'height
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))
(unless (test-height height)
  (printf "fail: (test-height height)~n"))

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
			 (try-candidate 'test-width
							candidate
							6
							(cons (cons 10
										(cons (cons 1
													2)
											  (cons 3
													4)))
								  (cons (cons 6
											  7)
										(cons 8
											  9))))
             ;;; etc.
             )))

;;; Exercise 8
;;; ----------
;;; 
;;; Describe how to compute the width of a binary tree:
;;; 
;;; * in English:
;;; The width of a leaf is 1.
;;; If a node have 2 leaves as it's children, width is sum of leafs.
;;; If a node have 1 node and 1 leaf as it's children, width is the width
;;;   of the child node.
;;; If a node have 2 nodes as it's children, width is the sum of width
;;;   of the children nodes. 
;;; 
;;; 
;;; * functionally.
;;; For all number n return 1
;;; For all wellformed trees t1 and t2 with width v1 and v2,
;;;   if t1 and t2 are both leaves or nodes, return v1+v2.
;;;   if t1 is a leave and t2 is a node, return v2
;;;   if t1 is a node and t2 is a leave, return v1
;;;
;;; Then define a Scheme procedure ``width`` that given a binary tree,
;;; computes its width.  If it is given another value than a well-formed
;;; binary tree, your procedure should raise an error.  Verify that it passes
;;; the unit test.

;;; the code below is a simplification of abovementioned statement.

(define width
  (lambda (v_init)
  (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         1]
                        [(pair? v)
                         (let([x (+ (visit (car v))
                                    (visit (cdr v)))])
                           (- x (modulo x 2)))]
                        [else
                         (errorf 'width
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(define width-with-list
  (lambda (v_init)
	(letrec ([visit (lambda (v)
					  (cond
						[(number? v)
						 (list 1)]
						[(pair? v)
						 (letrec ([xs (visit (car v))]
								  [ys (visit (cdr v))]
								  [addLists (lambda (xss yss)
											  (cond
												[(and (pair? xss) (pair? yss))
												 (cons (+ (car xss) (car yss)) (addLists (cdr xss) (cdr yss)))]
												[(and (pair? xss) (number? yss))
												 (cons (+ (car xss) yss) (addLists (cdr xss) '()))]
												[(and (number? xss) (pair? yss))
												 (cons (+ xss (car yss)) (addLists '() (cdr yss)))]
												[(and (number? xss) (number? yss))
												 (list (+ xss yss))]
												[(and (null? xss) (null? yss))
												 '()]
												[(and (null? xss) (pair? yss))
												 (cons (car yss) (addLists '() (cdr yss)))]
												[(and (pair? xss) (null? yss))
												 (cons (car xss) (addLists (cdr xss) '()))]
												[(and (null? xss) (number? yss))
												 (list yss)]
												[(and (number? xss) (null? yss))
												 (list xss)]
												))])
						   (cons 1 (addLists xs ys)))]
						[else
						  (errorf 'width
								  "not a binary tree: ~s"
								  v)]))])
	  (apply max (visit v_init)))))

;(unless (test-width width)
  ;(printf "fail: (test-width width)~n"))

(unless (test-width width-with-list)
  (printf "fail: (test-width width-with-list)~n"))



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

;;; Exercise 9
;;; ----------
;;; 
;;; Describe how to flatten a binary tree using ``list`` and ``append``:
;;; 
;;; * in English:
;;; If it's a leaf, then the flattened tree is
;;;   a singleton list containing this leaf.
;;; If it's a node, then the flattened tree is the concatenation of
;;;   the result of flattening the left subtree and
;;;   the result of flattening the right subtree.
;;; 
;;;
;;; * functionally.
;;; For all numbers n, flatten-tree it's a list of n
;;; For all well-formed binary trees t1 and t2, flatten-tree(cons t1 t2) =
;;;   (append number-of-nodes t1, number-of-nodes t2), where append is a function
;;;   that appends element to a list
;;;
;;; Then define a Scheme procedure ``flatten`` that given a binary tree,
;;; flattens it into its list of leaves.  If it is given another value than a
;;; well-formed binary tree, your procedure should raise an error.  Verify
;;; that it passes the unit test.

(define flatten
  (lambda (v_init)
   (letrec ([visit (lambda(v)
                      (cond
                        [(number? v)
                         (list v)]
                        [(pair? v)
                        (append  (visit (car v))
                               (visit (cdr v)))]
                        [else
                         (errorf 'flatten
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))
(unless (test-flatten flatten)
   (printf "fail (test-flatten flatten)~n"))

;;;;;;;;;;

;;; Exercise 10
;;; -----------
;;; 
;;; a. Write a unit-test procedure for mobiles.  (NB. The leaf of a mobile
;;;    should be represented by a non-negative integer.)
;;; 
;;; b. Describe how to check whether a binary tree represents a well-balanced
;;;    mobile
;;; 
;;;    * in English
;;;    Given leaf, return true
;;;    Given a well formed tree t1 with weight n1 and a well formed tree t2 with;;;    with weight n2, then check if n1 and n2 is equal.
;;; 
;;;
;;;    * functionally.
;;;    For all numbers n, return #t
;;;    For all well formed binary tree t1 and t2, well-balanced(cons t1 t2)
;;;      = weight of t1 == weight of t2
;;; 
;;; c. Define a Scheme procedure ``well-balanced?`` that given a binary tree,
;;;    determines whether this tree represents a well-balanced mobile.  If it
;;;    is given another value than a well-formed binary tree, your procedure
;;;    should raise an error.  Verify that it passes the unit test.

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

(define well-balanced-2pass?
  (lambda (v_init)
   (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         #t]
                        [(pair? v)
                         (and (equal? (weight (car v))
                                      (weight (cdr v)))
                              (and (visit (car v))
                                   (visit (cdr v))))]
                        [else
                         (errorf 'well-balanced?
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(define well-balanced?
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         (cons #t v)]
                        [(pair? v)
                         (let ([n1 (visit (car v))]
                               [n2 (visit (cdr v))])
                           (cons (and (equal? (cdr n1) (cdr n2))
                                      (and (car n1) (car n2)))
                                 (+ (cdr n1) (cdr n2))))]
                        [else
                         (errorf 'well-balanced?
                                 "not a binary tree: ~s"
                                 v)]))])
      (car (visit v_init)))))

(unless (test-mobile well-balanced?)
   (printf "fail (test-mobile well-balanced)~n"))

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

;;; Exercise 11
;;; -----------
;;; 
;;; Describe how to swap each of the nodes of a binary tree:
;;; 
;;; * in English,
;;; 
;;; * logically, and
;;; 
;;; * functionally.
;;; 
;;; Then define a Scheme procedure that given a binary tree, swaps each of
;;; its nodes.  If it is given another value than a well-formed binary tree,
;;; your procedure should raise an error.  Verify that it passes the unit
;;; test.

(define snoc
  (lambda (a d)
    (cons d a)))

(define swap
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(number? v)
                         v]
                        [(pair? v)
                         (snoc (visit (car v))
                               (visit (cdr v)))]
                        [else
                         (errorf 'swap
                                 "not a binary tree: ~s"
                                 v)]))])
      (visit v_init))))

(unless (test-swap swap)
  (printf "fail: (test-swap swap)~n"))

;;;;;;;;;;

"week-02_processing-binary-trees.scm"

;;; end of week-02_processing-binary-trees.scm
