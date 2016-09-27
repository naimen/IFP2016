;;; week-04_tail-calls.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo
;;; Version of 20 Sep 2016

;;;;;;;;;;

(define test-andmap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #t)  ;;; <------******------ why not #f?
                      ;;; because the and procedure returns when seen a #f
         (equal? (candidate number? '(1 2 3))
                 #t)
         (equal? (candidate number? '(1 "2" 3))
                 #f)
         (equal? (candidate (lambda (x) x) '(1 2 3))
                 3)
         (equal? (candidate (lambda (x) x) '(1 2 #f 4 5))
                 #f)
         ;;;
         )))

(define andmap1-not-properly-tail-recursive
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (and (p (car ws))
                               (visit (cdr ws)))))])
      (visit vs))))

(define andmap1
  (lambda (p vs) 
    (letrec ([visit (trace-lambda visit (w ws)
                      (cond
                        [(null? ws)
                         (p w)]
                        [(pair? ws)
                         (and (p w)
                              (visit (car ws)
                                     (cdr ws)))]
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
    

(define andmap2 ; boolean output
  (lambda (p vs) 
    (letrec ([visit (trace-lambda visit (xs)
                      (cond
                        [(null? xs)
                         #t]
                        [(p (car xs))
                         (visit (cdr xs))]
                        [else
                         #f]))])
      (visit vs))))

(define andmap3
  (lambda (p vs) 
    (letrec ([visit (trace-lambda visit (ws a)
                      (if (null? ws)
                          a
                          (visit (cdr ws)
                                 (and a
                                      (p (car ws))))
                          ))])
      (visit vs #t))))



;;;;;;;;;;

(define test-ormap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #f)  ;;; <------******------ why not #t?
                      ;;; becuase the or procedure returns when seen a #t
         (equal? (candidate number? '(1 2 3))
                 #t)
         (equal? (candidate number? '("1" "2" 3))
                 #t)
         (equal? (candidate number? '("1" "2" "3"))
                 #f)
         (equal? (candidate (lambda (x) x) '(1 2 3))
                 1)
         ;;;
         )))

(define ormap1
  (lambda (p vs) 
    (letrec ([visit (trace-lambda visit (w ws)
                      (cond
                        [(null? ws)
                         (p w)]
                        [(pair? ws)
                         (or (p w)
                              (visit (car ws)
                                     (cdr ws)))]
                        ))])
      (cond
        [(null? vs)
         #f]
        [(pair? vs)
         (visit (car vs) (cdr vs))]
        [else
         (errorf 'ormap1
                 "Illegal arguments ~s"
                 vs)]))))
    


(define ormap2 ;;; boolean output
  (lambda (p vs)
    (letrec ([visit (trace-lambda visit (xs)
                      (cond
                        [(null? xs)
                         #f]
                        [(p (car xs))
                         #t]
                        [else
                         (visit (cdr xs))]))])
      (visit vs))))

(define ormap3
  (lambda (p vs) 
    (letrec ([visit (trace-lambda visit (ws a)
                      (if (null? ws)
                          a
                          (visit (cdr ws)
                                 (or a
                                     (p (car ws))))
                          ))])
      (visit vs #f))))


;;;;;;;;;;

;;; end of week-04_tail-calls.scm

"week-04_tail-calls.scm"
