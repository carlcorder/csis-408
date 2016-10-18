#lang racket

;Carl Corder
;Dr. Hastings
;CSIS 408 Homework 4
;01.03.2011

;1.a

(define (contains? list object)
  (cond ((null? list) #f)
        ((equal? (car list) object) #t)
        (#t (contains? (cdr list) object))))

(display "(contains? (list 1 2 3 4) 2)= ")
(contains? (list 1 2 3 4) 2)

;1.a Alternative

;We use the accumulate procedure for the second method.
(define (accumulate operator null list)
  (if (null? list)
      null
      (operator (car list)
                (accumulate operator null (cdr list)))))

;We also define flatmap here
(define (flatmap procedure list)
  (accumulate append null (map procedure list)))


(define (contains*? list object)
  (accumulate (lambda (x y) (or x y)) #f (map (lambda (x) (equal? x object)) list)))

(display "(contains*? (list 1 2 3 4) 0)= ")
(contains*? (list 1 2 3 4) 0)

;1.b The procedure (remove object list) returns the list with all occurences of the object ;removed. Implement two versions: one recursive, and one without recursion using accumulate. ;In the former, maximize structure sharing - the returned list should share as much ;structure with the original one as possible. (Hint: Use contains?) 

(define (remove object list)
  (define (remove-aux object list Rlist)
    (cond ((null? list) Rlist)
          ((not (equal? (car list) object)) (remove-aux object (cdr list) (cons (car list) Rlist)))
          (#t (remove-aux object (cdr list) Rlist))))
  (remove-aux object list '()))

(display "(remove 2 (list 1 2 3 2 1))= ")(remove 2 (list 1 2 3 2 1))

;1.b Alternative method using accumulate and contains?


(define (remove* object list)
  (accumulate (lambda (item list) (cons item list)) null
              (filter (lambda (x) (not (equal? object x))) list)))

(display "(remove* 2 (list 1 2 3 2 3 4 2))= ")(remove* 2 (list 1 2 3 2 3 4 2))

;2.a

(define (operator? object)
  (cond ((equal? object +) #t)
        ((equal? object -) #t)
        ((equal? object *) #t)
        ((equal? object /) #t)
        (#t #f)))

(display "(operator? *)= ")(operator? *)

;2.b

(define (id operator)
  (cond ((equal? operator +) 0)
        ((equal? operator -) 0)
        ((equal? operator *) 1)
        ((equal? operator /) 1)
        (#t null)))

(display "(id -)= ")(id -)

;2.c This needs to be changed because (list 1 + 2) is not a valid expression. It should be (list + 1 2). No unary operators allowed.
;Inspiration for this procedure goes to Dan Farlan

(define (expression? object)
  
  (define (expression-aux object)
    (cond ((null? object) #t)
          ((list? (car object)) (and (expression? (car object)) (expression-aux (cdr object))))
          ((operator? (car object)) #f)
          ((number? (car object)) (expression-aux (cdr object)))
          (null #f)))
  
  (cond ((and (list? object) (> (length object) 2) (operator? (car object))) (expression-aux (cdr object)))
        (#t #f)))

(define expr (list + 1 (list / 2 (list + (list * 3 5) 1)) (list - 0 4)))

(display "(expression? expr)= ")(expression? expr)
(display "\n")
(expression? (list + (list * 2 3) 'k))

;2.d

(define (count-operators expr)
  (define (count-operators-aux expr)
    (accumulate + 0 (map (lambda (y) (if (equal? y #t) 1 0)) (map (lambda (x) (operator? x)) (flatten expr)))))
  (if (expression? expr)
      (count-operators-aux expr)
      0))

(display "(count-operators expr)= ")(count-operators expr)

(define (count-primitive-operands expr)
  (define (count-operators-aux expr)
    (accumulate + 0 (map (lambda (y) (if (equal? y #t) 1 0)) (map (lambda (x) (number? x)) (flatten expr)))))
  (if (expression? expr)
      (count-operators-aux expr)
      0))

(display "(count-primitive-operands expr)= ")(count-primitive-operands expr)

;2.e Write a procedure (evaluate expr) that returns the arithmetic value of the expression by implementing the count procedures and evaluate in terms of a combination of accumulate and map.

;I will assume that the input is a valid expression.
;I seem to be having some side effects with this procedure.
(define (evaluate expr)
  
  (define (evaluate-aux expr eval-list)
  
  (define (evaluate-num expr eval-list)
    (append eval-list (list (car expr)))
    (evaluate-aux (cdr expr) eval-list))
  
  (cond((null? expr) (eval-list))
       ((operator? (car expr)) (accumulate (car expr) (id (car expr)) (evaluate-aux (cdr expr) eval-list)))
       ((number? (car expr)) (evaluate-num expr eval-list))
       ((list? (car expr)) (evaluate-aux (car expr) eval-list))
       ))
  (evaluate-aux expr null))



;Display calls
(display "\n")
(display "(operator? (car expr))= ")(operator? (car expr))
(display "(number? (cadr expr))= ")(number? (cadr expr))
(display "(expression? expr)= ")(expression? expr)
(display "(expression? (list 1 + 2))= ")(expression? (list 1 + 2));This should return false but does not
(display "(expression? (list + 1))= ")(expression? (list + 1))


#|
could use this to write expression/eval as well.
(define (expression? object)
  (and (operator? (car object))  (accumulate (lambda (x y) (and x y)) #t (map (lambda (x) (cond ((number? x) #t);put x here
                                                                    ((list? x) (expression? x));eval x
                                                                    (else #f)))
                                                  (cdr object)))))


(define (evaluate* expr)
  (and (operator? (car expr))  (accumulate (lambda (x y) (and x y)) #t (map (lambda (x) (cond ((number? x) x);put x here
                                                                    ((list? x) (evaluate* x));eval x
                                                                    (else #f)))
                                                  (cdr expr)))))
|#

