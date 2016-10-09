;;; week-06_unit-stress-tests.scm
;;; IFP 2016-2017, Q1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 04 Oct 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-06_unit-stress-tests.html

;;;;;;;;;;

(define stress-test-silently
  #t)

;;; need something to replace stress-test-silently, not sure which

(define raises-an-exception?
  (lambda (thunk)
    (guard (the-condition [(error? the-condition)
                           (begin
                             (unless stress-test-silently
                               (begin
                                 (printf "Error in ~s: "
                                         (condition-who the-condition))
                                 (apply printf
                                        (cons (condition-message the-condition)
                                              (condition-irritants the-condition)))
                                 (newline)))
                             #t)]
                          [else
                           (begin
                             (unless stress-test-silently
                               (printf "Another exception is being raised.~n"))
                             #t)])
           (begin
             (thunk)
             #f))))


;;;;;;;;;;
(define andmap1
  (lambda (p vs) 
    (letrec ([visit (lambda (w ws)
                      (cond
                        [(null? ws)
                         (p w)]
                        [(pair? ws)
                         (and (p w)
                              (visit (car ws)
                                     (cdr ws)))]
                        [else
                         (errorf 'andmap1
                                 "not a proper list ~s"
                                 ws)]
                        ))])
      (cond
        [(null? vs)
         #t]
        [(pair? vs)
         (visit (car vs) (cdr vs))]
        [else
         (errorf 'andmap1
                 "Illegal arguments ~s"
                 vs)]))))


;;;;;;;;;


(define stress-test-andmap1
  (lambda (candidate)
    (and (raises-an-exception? (lambda ()
                                 (candidate)))
         (raises-an-exception? (lambda ()
                                 (candidate number?)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 3) '(4 5 6))))
         (raises-an-exception? (lambda ()
                                 (candidate number? 'x)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 . x))))
         (raises-an-exception? (lambda ()
                                 (candidate number? "derp")))
       ; (raises-an-exception? (lambda ()
       ;                         (candidate (lambda (x) x) '(1 2 3))))

         ;;; etc.
         )))

(unless (stress-test-andmap1 andmap1)
  (printf "fail: (stress-test-andmap1 andmap1)~n"))


;;;;;;;;;
(define ormap1
  (lambda (p vs) 
    (letrec ([visit (lambda (w ws)
                      (cond
                        [(null? ws)
                         (p w)]
                        [(pair? ws)
                         (or (p w)
                              (visit (car ws)
                                     (cdr ws)))]
                        [else
                         (errorf 'ormap1
                                 "not a proper list ~s"
                                 ws)]
                        ))])
      (if (list? vs)
          (cond
            [(null? vs)
             #f]
            [(pair? vs)
             (visit (car vs) (cdr vs))]
            [else
             (errorf 'ormap1
                     "Illegal arguments ~s"
                     vs)])
          (errorf 'ormap1
                  "not a proper list ~s"
                  vs))
          )))

;;;;;;;;;
(define stress-test-ormap1
  (lambda (candidate)
    (and (raises-an-exception? (lambda ()
                                 (candidate)))
         (raises-an-exception? (lambda ()
                                 (candidate number?)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 3) '(4 5 6))))
         (raises-an-exception? (lambda ()
                                 (candidate number? 'x)))
         (raises-an-exception? (lambda ()
                                 (candidate number? '(1 2 . x))))
         (raises-an-exception? (lambda ()
                                 (candidate error? "derp")))
       ; (raises-an-exception? (lambda ()
       ;                         (candidate (lambda (x) x) '(1 2 3))))

         ;;; etc.
         )))

(unless (stress-test-ormap1 ormap1)
  (printf "fail: (stress-test-ormap1 ormap1)~n"))


;;;;;;;;;;;

;;; end of week-06_unit-stress-tests.scm

"week-06_unit-stress-tests.scm"
