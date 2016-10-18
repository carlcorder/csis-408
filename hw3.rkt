#lang racket

#| 
Carl Corder
Dr. Hastings
CSIS 408
20.02.2011
|#

;1a. Define a procedure which compares the output of two procedures for a given value n.

(define (compare proc1 proc2 n)
  (cond ((equal? (proc1 n) (proc2 n)) #t)
        (#t #f)))

(define cube (lambda(x) (* x x x)))
(define square (lambda(x) (* x x)))

(display "(compare cube square 2)=")(compare cube square 2)

;1b. Define a procedure which returns another procedure called comparator, which is the same compare procedure from part a.

(define (make-comparator proc1 proc2)
  (define (comparator n)
    (cond ((equal? (proc1 n) (proc2 n)) #t)
          (#t #f)))
  comparator)

(display "((make-comparator cube square) 1)=")((make-comparator cube square) 1)

;Alternatively using a lambda function

(define (make-comparator* proc1 proc2)
  (lambda(n) (cond ((equal? (proc1 n) (proc2 n)) #t)
                   (#t #f))))

;1c. Define a procedure (test comparator from to) that accepts as parameters a comparator and two numbers from and to. It should return true iff the comparator returns true for every number between from and to, inclusive. Assume we have integers and from <= to.

(define (test comparator from to)
  (cond ((and (comparator from) (<= from to)) (test comparator (+ from 1) to))
        ((> from to) #t)
        (#t #f)))

(display "(test (make-comparator cube square) 1 3)=")(test (make-comparator cube square) 1 3)

;This works, but can I improve it? How about making this procedure iterative?

;1d. Define a procedure that takes two numbers, and returns a procedure that accepts a comparator as a parameter and test the comparator with
;the numbers as the procedure test above.

(define (make-test from to)
  (define (test* comparator)
    (cond ((and (comparator from) (<= from to)) (test comparator (+ from 1) to))
          ((> from to) #t)
          (#t #f)))
  test*)

(display "((make-test 1 3) (make-comparator cube square))=")((make-test 1 3) (make-comparator cube square))

#|

Some examples to try:

(compare square cube 2)
(compare square cube 1)
(define compare-sc (make-comparator square cube))
(compare-sc 1)
(test compare-sc 1 3)
(define test-1-3 (make-test 1 3))
(test-1-3 compare-sc)

|#

;2.a Create a procedure that accepts a one parameter procedure proc and returns a one-parameter procedure. Whenever applied to an input,
;the returned procedure should return a value that is the boolean negation of the value proc would return for the same input.

;How will I ensure that proc is a procedure which only accepts one parameter and has boolean output?

(define (complement proc)
  (define (~proc n)
    (if (proc n)
        #f
        #t))
  ~proc)

(define (even? n)
  (if (= (modulo n 2) 0)
      #t
      #f))

(display "((complement even?) 10)=")((complement even?) 10)
(display "((complement even?) 9)=")((complement even?) 9)

(define (odd? n)
  ((complement even?) n))

;2.b Write both a lambda and a let version of a procedure for evaluating the expression a*x^2 + b*x + c with the variable bindings a=1,b=2 and c=3.

(define (poly-eval x)
  ((lambda(a b c x) (+ (+ (* a (* x x)) (* b x)) c)) 
    1 2 3 x));seeding the lambda functions with initial coefficients


(define (poly-eval* x)
(let ((a 1) (b 2) (c 3))
  (+ (+ (* a (* x x)) (* b x)) c)))
  
;2.c This is an easy fix, let local variables are bound simultaneously. But let* is bounded sequentially.

(let* ((x 4)
       (y (+ x 1)))
  (+ x y))

;2.d Becuase we use let the procedures are bound simulatneously. Thus + and - are exchanged as well as the * and / operators.

(let ((+ -)
      (- +)
      (* /)
      (/ *))
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2))

;If we use let* the variables will be bound sequentially, so we will need to use temporary variables t1 & t2.

(let* ((t1 -)
       (- +)
       (+ t1)
       (t2 /)
       (/ *)
       (* t2))
  (* (/ (- (+ -6) (- 3 2 5)) 3) 2))

;2.e Is this an acceptable replacement for the special form if?

(define (iffy test consequent alternative)
  (if test
      (consequent)
      (alternative)))
 
;We call iffy with #t + and *
(display "(iffy #t + *)=")(iffy #t + *)
