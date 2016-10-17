;;; RegexSample.scm
;;; IFP 2016-2016, Q1
;;; Markus, Rasmus, Shuo


;;;;;;;;;;


(define re0
  '(seq (atom 10) (seq (var x) (atom 30))))


(define re1
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x)))))
;;;;;;;;;;

(define sample-of-arithmetic-expressions
  (list re0
        re1))

;;;;;;;;;;

;;; end of RegexSample.scm

"RegexSample.scm"
