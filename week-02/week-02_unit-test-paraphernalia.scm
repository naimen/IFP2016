;;; week-02_unit-test-paraphernalia.scm
;;; IFP 2016-2017, Q1
;;; Markus, Rasmus, Shuo <201206051>
;;; Version of 05 Sep 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/IFP16/Lecture-notes/week-02_unit-test-paraphernalia.html

;;;;;;;;;;

(define try-candidate
  (lambda (name candidate expected-output . input)
    (or (equal? expected-output
                (apply candidate input))
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

;;;;;;;;;;

(define test-and-all
  (lambda (name candidate)
    (and (try-candidate name
                        candidate
                        #t
                        #t #t #t)
         (try-candidate name
                        candidate
                        #f
                        #t #f #t)
         ;;; etc.
         )))

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

(unless (test-and-all 'and-all and-all)
  (printf "fail: (test-and-all 'and-all and-all)~n"))

;;;;;;;;;;

;;; Exercise 1
;;; ----------
;;; 
;;; Define your very own version of ``andmap``.
;;; 
;;; * Reminder: ``andmap`` was described `in the PL lecture notes
;;;   <http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-5_map.html#mapping-procedures-over-proper-lists-continued>`_.
;;; 
;;; * Hint: start by defining a unit-test procedure.

(define test-andmap
  (lambda (name candidate)
    (and (try-candidate name
                        candidate
                        #t
                        number?
                        '(0 1/2 3.14))
         (try-candidate name
                        candidate
                        #f
                        number?
                        '(0 "1" 3.14))
         (try-candidate name
                        candidate
                        #t
                        <
                        '(0 1 2 3)
                        '(1 2 3 4))
         (try-candidate name
                        candidate
                        #t
                        (lambda (x y z)
                          (and (<= x y)
                               (<= y z)))
                        '(0 1 2)
                        '(1 2 3)
                        '(2 3 4))
         ;;; etc.
         )))

(define andmap1
  (lambda (p arg)
    (letrec ([visit (lambda (ws)
                    (if (null? ws)
                        #t
                        (and (p (car ws))
                             (visit (cdr ws)))))])
      (visit arg))))

(define our-very-own-andmap
  (lambda (p arg . args)
    (letrec ([visit (lambda (w ws)
                      (cond
                        [(null? w)
                         (if (andmap1 null? ws)
                             #t
                             (errorf 'my-very-own-andmap
                                     "Some lists are longer than others: ~s"
                                     (cons w ws)))]
                        [(pair? w)
                         (if (andmap1 pair? ws)
                             (and (apply p (cons (car w) (map car ws)))
                                  (visit (cdr w) (map cdr ws)))
                             (errorf 'my-very-own-andmap
                                     "Some lists are longer than others: ~s"
                                     (cons w ws)))]
                        [else
                         (errorf 'my-very-own-andmap
                                 "Not a proper list: ~s"
                                 w)]))])
      (visit arg args))))


;; Uncomment the following two lines:
(unless (test-andmap 'our-very-own-andmap our-very-own-andmap)
   (printf "fail: (test-andmap 'our-very-own-andmap our-very-own-andmap)~n"))

;;;;;;;;;;

;;; Exercise 2
;;; ----------
;;; 
;;; Redefine ``and-all`` using ``andmap``.
;;; 
;;; * Hint: remember to use a unit-test procedure.

(define and-all_alt
  (lambda bs_init
    (andmap (lambda(x) x) bs_init)))

;; Uncomment the following two lines:
(unless (test-and-all 'and-all_alt and-all_alt)
   (printf "fail: (test-and-all 'and-all_alt and-all_alt)~n"))

;;;;;;;;;;

"week-02_unit-test-paraphernalia.scm"

;;; end of week-02_unit-test-paraphernalia.scm
