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
    (letrec ([visit (trace-lambda visit (f xs)
                      (cond
                        [(null? xs)
                         #t]
                        [(f (car xs))
                         (visit f (cdr xs))]
                        [else
                         #f]))])
      (visit p vs))))

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
    (letrec ([visit (trace-lambda visit (f xs)
                      (cond
                        [(null? xs)
                         #f]
                        [(f (car xs))
                         #t]
                        [else
                         (visit f (cdr xs))]))])
      (visit p vs))))

;;;;;;;;;;

;;; end of week-04_tail-calls.scm

"week-04_tail-calls.scm"
