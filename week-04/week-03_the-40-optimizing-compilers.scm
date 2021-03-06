;;; week-03_the-40-optimizing-compilers.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo <danvy@cs.au.dk>
;;; Version of 13 Sep 2016

;;; Accompanying material for the lecture note at
;;;   https://users-cs.au.dk/danvy/IFP16/Lecture-notes/the-40-optimizing-compilers.html

;;;;;;;;;;

(load "week-03_arithmetic-expressions-sample.scm")

;;;;;;;;;;

;;; utilities:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (1- n))))))

;;;;;;;;;;

;;; implementation of the BNF for arithmetic expressions:

;;; <arithmetic-expression> ::= (literal <number>)
;;;                           | (plus <arithmetic-expression> <arithmetic-expression>)
;;;                           | (times <arithmetic-expression> <arithmetic-expression>)

;;;;;

;;; the constructors:

(define make-literal
  (lambda (n)
    (list 'literal n)))

(define make-plus
  (lambda (e1 e2)
    (list 'plus e1 e2)))

(define make-times
  (lambda (e1 e2)
    (list 'times e1 e2)))

;;; the predicates:

(define is-literal?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'literal)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-plus?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'plus)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-times?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'times)
         (proper-list-of-given-length? (cdr v) 2))))

;;; the accessors:

(define literal-1
  (lambda (v)
    (list-ref v 1)))

(define plus-1
  (lambda (v)
    (list-ref v 1)))

(define plus-2
  (lambda (v)
    (list-ref v 2)))

(define times-1
  (lambda (v)
    (list-ref v 1)))

(define times-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

(define fold-right_arithmetic-expression
  (lambda (case-literal case-plus case-times case-else)
    (lambda (v_init)
      (letrec ([visit (lambda (v)
                        (cond
                          [(is-literal? v)
                           (case-literal (literal-1 v))]
                          [(is-plus? v)
                           (case-plus (visit (plus-1 v))
                                      (visit (plus-2 v)))]
                          [(is-times? v)
                           (case-times (visit (times-1 v))
                                       (visit (times-2 v)))]
                          [else
                           (case-else v)]))])
        (visit v_init)))))

;;;;;;;;;;

;;; unparser:

(define unparse-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (list '+
             (unparse-arithmetic-expression (plus-1 e))
             (unparse-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (list '*
             (unparse-arithmetic-expression (times-1 e))
             (unparse-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'unparse-arithmetic-expression
               "unrecognized abstract syntax: ~s"
               e)])))

;;; parser:

(define parse-arithmetic-expression
  (lambda (v)
    (cond
      [(number? v)
       (make-literal v)]
      [(proper-list-of-given-length? v 3)
       (case (list-ref v 0)
         [(+)
          (make-plus (parse-arithmetic-expression (list-ref v 1))
                     (parse-arithmetic-expression (list-ref v 2)))]
         [(*)
          (make-times (parse-arithmetic-expression (list-ref v 1))
                      (parse-arithmetic-expression (list-ref v 2)))]
         [else
          (errorf 'parse-arithmetic-expression
                  "unrecognized operator: ~s"
                  v)])]
      [else
       (errorf 'parse-arithmetic-expression
               "unrecognized concrete syntax: ~s"
               v)])))

;;;;;;;;;;

;;; unit tests:

(define test-interpret-arithmetic-expression
  (lambda (interpret)
    (and (= (interpret (parse-arithmetic-expression source-ae0)) 42)
         (= (interpret (parse-arithmetic-expression source-ae1)) 11)
         (= (interpret (parse-arithmetic-expression source-ae2)) 1111)
         (= (interpret (parse-arithmetic-expression source-ae3)) 120)
         (= (interpret (parse-arithmetic-expression source-ae4)) 6)
         (= (interpret (parse-arithmetic-expression source-ae5)) 27)
         (= (interpret (parse-arithmetic-expression source-ae15)) 12100)
         ;;; add more tests here
         )))

(define test-compile-and-run-arithmetic-expression
  (lambda (compile run)
    (and (= (run (compile (parse-arithmetic-expression source-ae0))) 42)
         (= (run (compile (parse-arithmetic-expression source-ae1))) 11)
         (= (run (compile (parse-arithmetic-expression source-ae2))) 1111)
         (= (run (compile (parse-arithmetic-expression source-ae3))) 120)
         (= (run (compile (parse-arithmetic-expression source-ae4))) 6)
         (= (run (compile (parse-arithmetic-expression source-ae5))) 27)
         (= (run (compile (parse-arithmetic-expression source-ae15))) 12100)
         ;;; add more tests here
         )))

(define test-interpret-arithmetic-expression_Magritte
  (lambda (interpret_Magritte)
    (and (let ([ae0 (parse-arithmetic-expression source-ae0)])
           (equal? (interpret_Magritte ae0) ae0))
         (let ([ae1 (parse-arithmetic-expression source-ae1)])
           (equal? (interpret_Magritte ae1) ae1))
         (let ([ae2 (parse-arithmetic-expression source-ae2)])
           (equal? (interpret_Magritte ae2) ae2))
         (let ([ae3 (parse-arithmetic-expression source-ae3)])
           (equal? (interpret_Magritte ae3) ae3))
         (let ([ae4 (parse-arithmetic-expression source-ae4)])
           (equal? (interpret_Magritte ae4) ae4))
         (let ([ae5 (parse-arithmetic-expression source-ae5)])
           (equal? (interpret_Magritte ae5) ae5))
         (let ([ae15 (parse-arithmetic-expression source-ae15)])
           (equal? (interpret_Magritte ae15) ae15))
          ;;; add more tests here
         )))

;;;;;;;;;;

;;; interpreter:

(define interpret-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (+ (interpret-arithmetic-expression (plus-1 e))
          (interpret-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (* (interpret-arithmetic-expression (times-1 e))
          (interpret-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression interpret-arithmetic-expression)
  (printf "fail: (test-interpret-arithmetic-expression interpret-arithmetic-expression)~n"))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code instructions:

;;;;;

;;; the constructors:

(define make-PUSH
  (lambda (n)
    (list 'PUSH n)))

(define make-ADD
  (lambda ()
    (list 'ADD)))

(define make-MUL
  (lambda ()
    (list 'MUL)))

;;; the predicates:

(define is-PUSH?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'PUSH)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-ADD?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'ADD)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-MUL?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'MUL)
         (proper-list-of-given-length? (cdr v) 0))))

;;; the accessors:

(define PUSH-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code programs:

(define make-byte-code-program
  (lambda (is)
    (list 'byte-code-program is)))

(define is-byte-code-program?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'byte-code-program)
         (proper-list-of-given-length? (cdr v) 1))))

(define byte-code-program-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; virtual machine:

(define at-least-two?
  (lambda (vs)
    (and (pair? vs)
         (pair? (cdr vs)))))

(define run-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (if (null? is)
                             vs
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is
                                        (cons (PUSH-1 i) vs))]
                                 [(is-ADD? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (+ operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (* operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [else
                                  (errorf 'run-byte-code-program
                                          "unrecognized byte code: ~s"
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

(unless (equal? (run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) 42)
  (printf "(run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) didn't yield 42~n"))

;;;;;;;;;;

;;; compiler:

(define compile-arithmetic-expression
  (lambda (e)
    (letrec ([visit (lambda (e a)
                      (cond
                        [(is-literal? e)
                         (cons (make-PUSH (literal-1 e)) a)]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (visit (plus-2 e)
                                       (cons (make-ADD) a)))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (visit (times-2 e)
                                       (cons (make-MUL) a)))]
                        [else
                         (errorf 'compile-arithmetic-expression
                                 "unrecognized expression: ~s"
                                 e)]))])
      (make-byte-code-program (visit e '())))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program)~n"))

;;;;;;;;;;

;;; "just-in-time" compiler:

(define compile-and-run-arithmetic-expression
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression)~n"))

;;;;;;;;;;

;;; unit test:

(define does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression ae)))))

(define test_does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Magritte virtual machine:

(define run-byte-code-program_Magritte
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (if (null? is)
                             vs
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is
                                        (cons (make-literal (PUSH-1 i)) vs))]
                                 [(is-ADD? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (make-plus operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (make-times operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [else
                                  (errorf 'run-byte-code-program
                                          "unrecognized byte code: ~s"
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

;;; The corresponding "just-in-time" decompiler:

(define compile-and-run-arithmetic-expression_Magritte
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression e))))

(unless (test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte)
  (printf "fail: (test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte)~n"))

;;;;;;;;;;

;;; The Magritte interpreter:

(define interpret-arithmetic-expression_Magritte
  (lambda (e)
    (cond
      [(is-literal? e)
       (make-literal (literal-1 e))]
      [(is-plus? e)
       (make-plus (interpret-arithmetic-expression_Magritte (plus-1 e))
                  (interpret-arithmetic-expression_Magritte (plus-2 e)))]
      [(is-times? e)
       (make-times (interpret-arithmetic-expression_Magritte (times-1 e))
                   (interpret-arithmetic-expression_Magritte (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression_Magritte
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte)
  (printf "fail: (test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte)~n"))

;;;;;;;;;;

;;; unit test:

(define does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte ae)
              (compile-and-run-arithmetic-expression_Magritte ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?)~n"))

;;; util
(define decompile-to-expression
  (lambda(compile source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (run-byte-code-program_Magritte(compile ae))
      )))

(define testCompiler
  (lambda (compile source-ae)
	(let ([ae (parse-arithmetic-expression source-ae)])
	  (cons (list source-ae)
			(list (unparse-arithmetic-expression (run-byte-code-program_Magritte(compile ae))))))))



;;;;;;;;;;;;;;;;;;;;

;;; Bizarre exercise:

(define compile-arithmetic-expression_bizarre
  (lambda(e)
    (letrec ([visit
              (lambda(e)
                (cond
                  [(is-literal? e)
                   (list (make-PUSH (literal-1 e)))]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      (lambda (e2 prepend a2)
                                                        (prepend e2 a2))
                                                      a1)))
                               (list (make-ADD)))]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        (lambda (e2 prepend a2)
                                                          (prepend e2 a2))
                                                        a1)))
                                (list (make-MUL)))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-plus
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      k
                                                      a1)))
                               (cons (make-ADD) a))]
                  [(is-times? e)
                   (k (visit-times (times-1 e)
                                   (lambda (e1 prepend a1)
                                     (prepend e1
                                              (visit-times (times-2 e)
                                                           (lambda (e2 prepend a2)
                                                             (prepend e2 a2))
                                                           a1)))
                                   (list (make-MUL)))
                      append
                      a)]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-times
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (k (visit-plus (plus-1 e)
                                  (lambda (e1 prepend a1)
                                    (prepend e1
                                             (visit-plus (plus-2 e)
                                                         (lambda (e2 prepend a2)
                                                           (prepend e2 a2))
                                                         a1)))
                                  (list (make-ADD)))
                      append
                      a)]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        k
                                                        a1)))
                                (cons (make-MUL) a))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e)))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_bizarre
  (lambda (e)
    (run-byte-code-program (compile-arithmetic-expression_bizarre e))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre)~n"))

;;;;;;;;;;

;;; Does interpreting an expression give the same result as
;;; compiling this expression and running the compiled program?

(define does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_bizarre e))))

;;;;;;;;;;

;;; ***
;;; Write the BNF of the output of compile-and-run-arithmetic-expression_Magritte_bizarre
;;; <arithmetic-expression_bizarre>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_bizarre-plus> <arithmetic-expression_bizarre>)
;;;   | (times <arithmetic-expression_bizarre-times> <arithmetic-expression_bizarre>)
;;; 
;;; <arithmetic-expression_bizarre-plus>
;;; ::= (literal <number>)
;;;   | (times <arithmetic-expression_bizarre-times> <arithmetic-expression_bizarre>)
;;; 
;;; <arithmetic-expression_bizarre-times>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_bizarre-plus> <arithmetic-expression_bizarre>)
;;; and implement a syntax checker for it:

(define syntax-check-bizarre_old
  (lambda (e)
    (letrec ([visit (lambda (v)
                        (cond
                          [(is-literal? v)
                           #t]
                          [(is-plus? v)
                           (and (not (is-plus? (plus-1 v)))
                                (visit (plus-1 v))
                                (visit (plus-2 v)))]
                          [(is-times? v)
                           (and (not (is-times? (times-1 v)))
                                (visit (times-1 v))
                                (visit (times-2 v)))]
                          [else
                           #f]))])
      (visit e))))

(define syntax-check-bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   (and (visit-plus (plus-1 e))
                        (visit (plus-2 e)))]
                  [(is-times? e)
                   (and (visit-times (times-1 e))
                        (visit (times-2 e)))]
                  [else
                   #f]))]
             [visit-plus
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   #f]
                  [(is-times? e)
                   (and (visit-times (times-1 e))
                        (visit (times-2 e)))]
                  [else
                   #f]))]
             [visit-times
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   (and (visit-plus (plus-1 e))
                        (visit (plus-2 e)))]
                  [(is-times? e)
                   #f]
                  [else
                   #f]))])
      (visit e))))


(define test-bizarre-compiler
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-bizarre
               (compile-and-run-arithmetic-expression_Magritte_bizarre
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-bizarre-compiler)
   (printf "fail: (test-bizarre-compiler)~n"))

;;;;;;;;;;

;;; ***
;;; Write the corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_bizarre_old
  (lambda (e_init)
    (letrec([visit (lambda (e)
                     (cond
                       [(is-literal? e)
                        (make-literal (literal-1 e))]
                       [(is-plus? e)
                        (if (and (is-plus? (plus-1 e))
                                 (or (is-literal? (plus-2 e))
                                     (is-plus? (plus-2 e))
                                     (is-times? (plus-2 e))))
                            (let* ([p1 (visit (plus-1 e))]
                                   [p2 (visit (plus-2 e))])
                              (visit (make-plus (plus-1 p1)
                                                (make-plus (plus-2 p1) p2))))
                            (make-plus (visit (plus-1 e))
                                       (visit (plus-2 e)))
                            )]
                       [(is-times? e)
                        (if (and (is-times? (times-1 e))
                                 (or (is-literal? (times-2 e))
                                     (is-times? (times-2 e))
                                     (is-plus? (times-2 e))))
                            (let* ([p1 (visit (times-1 e))]
                                   [p2 (visit (times-2 e))])
                              (visit (make-times (times-1 p1)
                                                (make-times (times-2 p1) p2))))
                            (make-times (visit (times-1 e))
                                       (visit (times-2 e)))
                            )]
                       [else
                        (errorf 'interpret-arithmetic-expression_Magritte_bizarre
                                "unrecognized expression: ~s"
                                e)]))])
      (visit e_init ))))

(define interpret-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (make-literal (literal-1 e))]
                  [(is-plus? e)
                   (visit-plus (plus-1 e) (visit (plus-2 e)))
                   ]
                  [(is-times? e)
                   (visit-times (times-1 e) (visit (times-2 e)))]
                  [else
                   (errorf 'interpret-arithmetic-expression_Magritte_bizarre
                           "illegal input: ~s"
                           e)]))]
             [visit-plus
              (lambda (e a)
                (cond
                  [(is-plus? e)
                   (visit-plus(plus-1 e)
                              (visit-plus (plus-2 e) a))]
                  [else
                   (make-plus(visit e)a)]))]
             [visit-times
              (lambda (e a)
                (cond
                  [(is-times? e)
                   (visit-times(times-1 e)
                              (visit-times (times-2 e) a))]
                  [else
                   (make-times(visit e)a)]))]
             )
      (visit e))))


;;; ***
;;; Is your bizarre Magritte interpreter structurally recursive?
;;; yes, the old one is not as it's multipass
;;; Can you write it with fold-right_arithmetic-expression?
;;; No, as the recursion is not generalized to (visit e1) (visit e2),
;;;  due to the way our accumulator behaves
;;; ***
;;; In plain English, which bizarre program transformation is performed?
;;; Flattening and build with accumulator
;;;;;;;;;;

(define does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_bizarre ae)
              (compile-and-run-arithmetic-expression_Magritte_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
(unless (test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Verify that the bizarre syntax checker
;;; accepts the output of your bizarre Magritte interpreter:

(define test-bizarre-Magritte-interpreter
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-bizarre
               (interpret-arithmetic-expression_Magritte_bizarre
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-bizarre-Magritte-interpreter)
   (printf "(test-bizarre-Magritte-interpreter) failed~n"))

;;;;;;;;;;

(define is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_bizarre ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_bizarre ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?)
   (printf "fail: (test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?)~n"))

;;;;;;;;;;

;;; ***
;;; Which is more efficient:
;;; interpreting an expression with your bizarre Magritte interpreter, or
;;; compiling it with the bizarre Magritte compiler and decompiling the result?
;;; As the interpreter is a one pass, it's more effecient,
;;;  but for the old implementation compiling and decompiling is more effecient.

;;;;;;;;;;;;;;;;;;;;

;;; Strange exercise:

(define compile-arithmetic-expression_strange
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (list (make-PUSH (literal-1 e)))]
                  [(is-plus? e)
                   (visit-plus (plus-2 e)
                               (lambda (e2)
                                 (append (visit-plus (plus-1 e)
                                                     append)
                                         e2
                                         (list (make-ADD)))))]
                  [(is-times? e)
                   (visit-times (times-2 e)
                                (lambda (e2)
                                  (append (visit-times (times-1 e)
                                                       append)
                                          e2
                                          (list (make-MUL)))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-plus
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (visit-plus (plus-2 e)
                               (lambda (e2)
                                 (append (visit-plus (plus-1 e)
                                                     k)
                                         e2
                                         (list (make-ADD)))))]
                  [(is-times? e)
                   (k (visit-times (times-2 e)
                                   (lambda (e2)
                                     (append (visit-times (times-1 e)
                                                          append)
                                             e2
                                             (list (make-MUL))))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-times
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (k (visit-plus (plus-2 e)
                                  (lambda (e2)
                                    (append (visit-plus (plus-1 e)
                                                        append)
                                            e2
                                            (list (make-ADD))))))]
                  [(is-times? e)
                   (visit-times (times-2 e)
                                (lambda (e2)
                                  (append (visit-times (times-1 e)
                                                       k)
                                          e2
                                          (list (make-MUL)))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e)))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_strange run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_strange run-byte-code-program)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_strange
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression_strange ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_strange)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_strange)~n"))

;;;;;;;;;;

;;; Does interpreting an expression give the same result as
;;; compiling this expression and running the compiled program?

(define does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_strange ae)))))

(define test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?)
  (printf "fail(test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_strange
  (lambda (ae)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_strange ae))))

;;;;;;;;;;

;;; ***
;;; Write the BNF of the output of compile-and-run-arithmetic-expression_Magritte_strange
;;; and implement a syntax checker for it:

;;; <arithmetic-expression_strange>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_strange-plus> <arithmetic-expression_strange>)
;;;   | (times <arithmetic-expression_strange-times> <arithmetic-expression_strange>)
;;; 
;;; <arithmetic-expression_strange-plus>
;;; ::= (times <arithmetic-expression_strange-times> <arithmetic-expression_strange>)
;;;   | (literal <number>)
;;; <arithmetic-expression_strange-times>
;;; ::=(plus <arithmetic-expression_strange-plus> <arithmetic-expression_strange>)
;;;   | (literal <number>)


(define syntax-check-strange_old
  (lambda (e)
    (letrec ([visit (lambda (v)
                        (cond
                          [(is-literal? v)
                           #t]
                          [(is-plus? v)
                           (and (not (is-plus? (plus-2 v)))
                                (visit (plus-1 v))
                                (visit (plus-2 v)))]
                          [(is-times? v)
                           (and (not (is-times? (times-2 v)))
                                (visit (times-1 v))
                                (visit (times-2 v)))]
                          [else
                           #f]))])
      (visit e))))

(define syntax-check-strange
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   (and (visit (plus-1 e))
                        (visit-plus (plus-2 e)))]
                  [(is-times? e)
                   (and (visit (times-1 e))
                        (visit-times (times-2 e)))]
                  [else
                   #f]))]
             [visit-plus
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   #f]
                  [(is-times? e)
                   (and (visit (times-1 e))
                        (visit-times (times-2 e)))]
                  [else
                   #f]))]
             [visit-times
              (lambda (e)
                (cond
                  [(is-literal? e)
                   #t]
                  [(is-plus? e)
                   (and (visit (plus-1 e))
                        (visit-plus (plus-2 e)))]
                  [(is-times? e)
                   #f]
                  [else
                   #f]))])
      (visit e))))

(define test-strange-compiler
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-strange
               (compile-and-run-arithmetic-expression_Magritte_strange
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-strange-compiler)
   (printf "(test-strange-compiler) failed~n"))

;;;;;;;;;;

;;; ***
;;; Write the corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_strange
  (lambda (e_init)
    (letrec([visit (lambda (e)
                     (cond
                       [(is-literal? e)
                        (make-literal (literal-1 e))]
                       [(is-plus? e)
                        (visit-plus (visit (plus-1 e)) (plus-2 e))
                        ]
                       [(is-times? e)
                        (visit-times (visit (times-1 e)) (times-2 e))
                        ]
                       [else
                        (errorf 'interpret-arithmetic-expression_Magritte_strange
                                "unrecognized expression: ~s"
                                e)]))]
            [visit-plus
             (lambda (a e)
                (cond
                  [(is-plus? e)
                   (visit-plus (visit-plus a (plus-1 e) )
                               (plus-2 e))]
                  [else
                   (make-plus a (visit e))]))]
            [visit-times
             (lambda (a e)
               (cond
                  [(is-times? e)
                   (visit-times (visit-times a (times-1 e) )
                                (times-2 e))]
                  [else
                   (make-times a (visit e))]))]
            )
      (visit e_init ))))


;;; ***
;;; Is your strange Magritte interpreter structurally recursive?
;;; yes
;;; Can you write it with fold-right_arithmetic-expression?
;;; no, same reason as before, the recursion can not be generalized to (visit e1)
;;;  (visit e2), due to the way an accumulator is used
;;; ***
;;; In plain English, which wonderful program transformation is performed?
;;; Flatten the tree and build it with accumulator
;;;;;;;;;;

(define does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_strange ae)
              (compile-and-run-arithmetic-expression_Magritte_strange ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?)
   (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Verify that the strange syntax checker
;;; accepts the output of your strange Magritte interpreter:

(define test-strange-Magritte-interpreter
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-strange
               (interpret-arithmetic-expression_Magritte_strange
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-strange-Magritte-interpreter)
   (printf "(test-strange-Magritte-interpreter) failed~n"))

;;;;;;;;;;

(define is_interpret-arithmetic-expression_Magritte_strange_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_strange ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_strange ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_strange_idempotent?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
(unless (test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?)
   (printf "fail: (test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?)~n"))

;;;;;;;;;;

;;; ***
;;; Which is more efficient:
;;; interpreting an expression with your wonderful Magritte interpreter, or
;;; compiling it with the wonderful Magritte compiler and decompiling the result?
;;; The interpreter is more effecient as it's a one pass

;;;;;;;;;;;;;;;;;;;;

;;; Surprising exercise:

(define compile-arithmetic-expression_surprising
  (lambda (e)
    (letrec ([visit
              (lambda (e k0 k1 k)
                (cond
                  [(is-literal? e)
                   (case (literal-1 e)
                     [(0)
                      (k0)]
                     [(1)
                      (k1)]
                     [else
                      (k (list (make-PUSH (literal-1 e))))])]
                  [(is-plus? e)
                   (visit (plus-1 e)
                          (lambda ()
                            (visit (plus-2 e)
                                   k0
                                   k1
                                   k))
                          (lambda ()
                            (visit (plus-2 e)
                                   k1
                                   (lambda ()
                                     (k (list (make-PUSH 1) (make-PUSH 1) (make-ADD))))
                                   (lambda (bcs2)
                                     (hammer bcs2 (list (make-ADD)) (lambda (is)
                                                                      (k (cons (make-PUSH 1) is)))))))
                          (lambda (bcs1)
                            (visit (plus-2 e)
                                   (lambda ()
                                     (k bcs1))
                                   (lambda ()
                                     (hammer bcs1 (list (make-PUSH 1) (make-ADD)) k))
                                   (lambda (bcs2)
                                     (weld bcs1 bcs2 (list (make-ADD)) k)))))]
                  [(is-times? e)
                   (visit (times-1 e)
                          k0
                          (lambda ()
                            (visit (times-2 e)
                                   k0
                                   k1
                                   k))
                          (lambda (bcs1)
                            (visit (times-2 e)
                                   k0
                                   (lambda ()
                                     (k bcs1))
                                   (lambda (bcs2)
                                     (weld bcs1 bcs2 (list (make-MUL)) k)))))]
                  [else
                   (errorf 'compile-arithmetic-expression_surprising
                           "unrecognized expression: ~s"
                           e)]))]
             [hammer
              (lambda (xs ys k)
                (if (null? xs)
                    (k ys)
                    (hammer (cdr xs)
                            ys
                            (lambda (zs)
                              (k (cons (car xs) zs))))))]
             [weld
              (lambda (xs ys zs k)
                (hammer ys zs (lambda (ws)
                                (hammer xs ws k))))])
      (make-byte-code-program (visit e
                                     (lambda ()
                                       (list (make-PUSH 0)))
                                     (lambda ()
                                       (list (make-PUSH 1)))
                                     (lambda (bcs)
                                       bcs))))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_surprising run-byte-code-program)
  (printf "fail: (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_surprising run-byte-code-program)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_surprising
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression_surprising ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_surprising)
  (printf "fail: (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_surprising)~n"))

;;;;;;;;;;

;;; Does interpreting an expression give the same result as
;;; compiling this expression and running the compiled program?

(define does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_surprising ae)))))

(define test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?)
  (printf "fail: (test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?)~n"))

;;;;;;;;;;

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_surprising
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_surprising e))))

;;;;;;;;;;

;;; ***
;;; Write the BNF of the output of compile-and-run-arithmetic-expression_Magritte_surprising
;;; <arithmetic-expression_surprising>
;;; ::= (literal <number>)
;;;   | (plus <arithmetic-expression_surprising-plus> <arithmetic-expression_surprising-plus>)
;;;   | (times <arithmetic-expression_surprising-times> <arithmetic-expression_surprising-times>)
;;; 
;;; <arithmetic-expression_surprising-plus>
;;; ::= (plus <arithmetic-expression_surprising-plus> <arithmetic-expression_surprising-plus>)
;;;   | (times <arithmetic-expression_surprising-times> <arithmetic-expression_surprising-times>)
;;;   | (literal ^[1-9]\d*$)     ;;;all numbers expect 0
;;; <arithmetic-expression_strange-times>
;;; ::= (plus  <arithmetic-expression_surprising-plus> <arithmetic-expression_surprising-plus>)
;;;   | (times <arithmetic-expression_surprising-times> <arithmetic-expression_surprising-times>)
;;;   | (literal ^([2-9]|[2-9]\d|[1-9]\d{1,})$)  ;;;all numbers expect 0 and 1
;;; and implement a syntax checker for it:

(define syntax-check-surprising
  (lambda (e)
    (letrec ([visit (lambda (v)
                        (cond
                          [(is-literal? v)
                           #t]
                          [(is-plus? v)
                           (and (not (equal? (plus-1 v) '(literal 0)))
                                (not (equal? (plus-2 v) '(literal 0)))
                                (visit (plus-1 v))
                                (visit (plus-2 v)))
                           ]
                          [(is-times? v)
                           (and (not (equal? (times-1 v) '(literal 0)))
                                (not (equal? (times-2 v) '(literal 0)))
                                (not (equal? (times-1 v) '(literal 1)))
                                (not (equal? (times-2 v) '(literal 1)))
                                (visit (times-1 v))
                                (visit (times-2 v)))
                           ]
                          [else
                           #f]))])
      (visit e))))
(define test-surprising-compiler
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-surprising
               (compile-and-run-arithmetic-expression_Magritte_surprising
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-surprising-compiler)
   (printf "(test-surprising-compiler) failed~n"))

;;;;;;;;;;

;;; ***
;;; Write the corresponding optimizing Magritte interpreter:              
(define interpret-arithmetic-expression_Magritte_surprising
  (lambda (e_init)
    (letrec([visit (lambda (e)
                     (cond
                       [(is-literal? e)
                        (make-literal (literal-1 e))]
                       [(is-plus? e)
                        (let ([e1 (visit (plus-1 e))]
                              [e2 (visit (plus-2 e))])
                          (cond
                            [(equal? e1 '(literal 0))
                             e2]
                            [(equal? e2 '(literal 0))
                             e1]
                            [else
                             (make-plus e1 e2)]))]
                       [(is-times? e)
                        (let ([e1 (visit (times-1 e))]
                              [e2 (visit (times-2 e))])
                          (cond
                            [(equal? e1 '(literal 1))
                             e2]
                            [(equal? e2 '(literal 1))
                             e1]
                            [(or(equal? e1 '(literal 0))
                                (equal? e2 '(literal 0)))
                             '(literal 0)]
                            [else
                             (make-times e1
                                         e2)]))]
                       [else
                        (errorf 'interpret-arithmetic-expression_Magritte_surprising
                                "unrecognized expression: ~s"
                                e)]))])
      (visit e_init ))))

(define interpret-arithmetic-expression_Magritte_surprising_alt
  (fold-right_arithmetic-expression (lambda (e)
                                      (make-literal  e))
                                      (lambda (e1 e2)
                                        (cond
                                          [(equal? e1 '(literal 0))
                                           e2]
                                          [(equal? e2 '(literal 0))
                                           e1]
                                          [else
                                           (make-plus e1 e2)]))
                                      (lambda (e1 e2)
                                        (cond
                                          [(equal? e1 '(literal 1))
                                           e2]
                                          [(equal? e2 '(literal 1))
                                           e1]
                                          [(or(equal? e1 '(literal 0))
                                              (equal? e2 '(literal 0)))
                                           '(literal 0)]
                                          [else
                                           (make-times e1
                                                       e2)]))
                                      (lambda (e)
                                        (errorf 'interpret-arithmetic-expression_Magritte_surprising
                                                "unrecognized expression: ~s"
                                                e))))


;;; ***
;;; Is your surprising Magritte interpreter structurally recursive?
;;; Yes
;;; Can you write it with fold-right_arithmetic-expression?
;;; Yes, as the recursion are generalized to (visit e1) (visit e2)
;;; ***
;;; In plain English, which surprising program transformation is performed?
;;; Recursivly finds all the 1's and 0's in the expression.
;;;  If it's plus, then ignore the 0 by adding it. If it's times,
;;;  then ignore 1 by multiply it, and returns 0 when see a 0.   
;;;;;;;;;;

(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
(unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?)
   (printf "fail: (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?)~n"))

;;;;;;;;;;

;;; Verify that the surprising syntax checker
;;; accepts the output of your surprising Magritte interpreter:

(define test-surprising-Magritte-interpreter
  (lambda ()
    (andmap (lambda (ae)
              (syntax-check-surprising
               (interpret-arithmetic-expression_Magritte_surprising
                 (parse-arithmetic-expression
                  ae))))
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
 (unless (test-surprising-Magritte-interpreter)
   (printf "(test-surprising-Magritte-interpreter) failed~n"))

;;;;;;;;;;

(define is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_surprising ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_surprising ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
            sample-of-arithmetic-expressions)))

;;; ***
;;; Uncomment the following lines to test your implementation when loading this file:
(unless (test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?)
  (printf "fail: (test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?)~n"))

;;;;;;;;;;

;;; ***
;;; Which is more efficient:
;;; interpreting an expression with your surprising Magritte interpreter, or
;;; compiling it with the surprising Magritte compiler and decompiling the result?
;;; the interpreter as it's one pass that are structurly recursive.

;;;;;;;;;;;;;;;;;;;;

;;; end of week_03_the-40-optimizing-compilers.scm

"week_03_the-40-optimizing-compilers.scm"
