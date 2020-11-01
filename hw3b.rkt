#lang racket

(require rackunit)

#|
CS 270 Math Foundations of CS
Spring 2020
Instructor:  Profs. Bruce Char,  Mark Boady, and Jeremy Johnson

Submit in BBLearn as part of assignment 3.

Submit a single .rk file with no spaces in the filename.

Assignment 3
Recursive definitions and functions

In this assignment students will write several functions that
recursively process numbers.  In class we provided two recursive
definitions of numbers.  The recursive definitions provide a set
of constructors which capture the ways to construct a number.
Peano numbers have two constructors, one of which, succ, is recursive,
and Binary Numbers have three constructors, two of which, double and
double-plus1 are recursive.

1)  Arithmetic using the DP-DP1 representation.

2)  Arithmetic using the Peano representation

    In words:  A number is either zero, or, using a recursive represention:
    the successor of a number is a number.  

Part A.  Writing an  archival version of D-DP1 arithmetic.

Implement the following basic functions:


(getZero)  is an arity-zero function that retunrs the D-DP list representing of zero.
(dzero? L)  returns true if L is the D-DP1 representation of zero.
(dsuccessor L) returns the D-DP1 representation of one more than L's value.
(predecessor L) returns one less than L
(addD L1 L2)  adds together the number, recursively
(prodD L1 L2) multplies the numbers together, using the recursive idea of prod.
(subD L1 L2)  subtracts L2 from L1, assuming that L1 >= L2.
(remainderD L1 L2)  divides L2 into L1 and returns the remainder, which is always a number
between zero and L1-1.
(quotientD L1 L2)
(divides? L1 L2)  returns #t if the remainder of L2 divided by L1 is zero.
"divides" means "cleanly divides with no remainder".
|#

; returns the zero value in D-DP1 notation
; Output: the value zero in D-DP1 notation
(define (getZero) 
   'zero ;Implement Me
  )

(module+ test
  (check-equal? (getZero) 'zero "getZero implementation")
  )

; check if D-DP1 number is zero
; Input: D-DP1 number
; Output: boolean representing if L is zero
(define (dzero? L)
  (equal? 'zero L) ;Implement Me
  )

(module+ test
  (check-equal? (dzero? 'zero) #t "dzero? implementation")
  (check-equal? (dzero? '(D (DP1 (DP1 zero)))) #f "dzero? implementation")
  (check-equal? (dzero? '(DP1 (DP1 zero))) #f "dzero? implementation"))
;input-spec: D-DP1 number
;output-spec: returns the double of given non-zero D-DP1 number
(define (double N)
  (if (equal? N 'zero) 'zero (list 'D N)))
; the dsuccessor of a D-DP1 number
; Input: D-DP1 number L
; Output: the dsuccessor to L
(define (dsuccessor L)
 ( if (dzero? L) '(DP1 zero) (if (equal? (first L) 'D) (cons 'DP1 (rest L)) (double (dsuccessor (second L))))))

(module+ test
  (check-equal? (dsuccessor 'zero) '(DP1 zero) "dsuccessor implementation")
  (check-equal? (dsuccessor '(DP1 (D (D (DP1 zero))))) '(D (DP1 (D (DP1 zero)))) "dsuccessor implementation")
  (check-equal? (dsuccessor '(DP1 (DP1 (D (DP1 zero))))) '(D (D (DP1 (DP1 zero)))) "dsuccessor implementation")
  )

; the predecessor of a D-DP1 number
; Input: D-DP1 number L
; Output: the prececessor to L
(define (predecessor L)
  (cond [(equal? '(DP1 zero) L) (getZero)]
        [(equal? (first L) 'D) (dsuccessor (double (predecessor (second L))))]
        [(double (second L))]))

(module+ test
  (check-equal? (predecessor '(DP1 zero)) 'zero "predecessor implementation")
  (check-equal? (predecessor '(DP1 (DP1 (DP1 zero)))) '(D (DP1 (DP1 zero)))  "predecessor implementation")
  (check-equal? (predecessor '(DP1 (D (DP1 (D (DP1 zero)))))) '(D (D (DP1 (D (DP1 zero)))))  "predecessor implementation")
  )

; addition of two D-DP1 numbers
; Input: D-DP1 numbers L1, L2
; Output: the sum of L1 and L2
(define (addD L1 L2)
  (cond [(dzero? L1) L2]
        [(dzero? L2) L1]
        [(addD (predecessor L1) (dsuccessor L2))]))

(module+ test
  (check-equal? (addD '(D (DP1 zero)) '(DP1 zero)) '(DP1 (DP1 zero))  "addD implementation")
  (check-equal? (addD '(D (DP1 (DP1 (DP1 zero)))) '(D (DP1 (DP1 (DP1 zero))))) '(D (D (DP1 (DP1 (DP1 zero))))) "addD implementation")
  (check-equal? (addD '(DP1 (DP1 zero)) '(DP1 zero)) '(D (D (DP1 zero)))  "addD implementation")
  (check-equal? (addD '(D (DP1 (DP1 zero))) '(DP1 zero)) '(DP1 (DP1 (DP1 zero)))  "addD implementation")
  )

; subtraction of two D-DP1 numbers
; Input: D-DP1 numbers L1, L2
; Output: the difference of L1 and L2
(define (subD L1 L2)
  (cond [(dzero? L1) L2]
        [(dzero? L2) L1]
        [(subD (predecessor L1) (predecessor L2))]))

(module+ test
  (check-equal? (subD '(D (DP1 zero)) '(DP1 zero)) '(DP1 zero)  "subD implementation")
  (check-equal? (subD '(D (DP1 (DP1 (DP1 zero)))) '(D (DP1 (DP1 (DP1 zero))))) 'zero "subD implementation")
  (check-equal? (subD '(DP1 (DP1 zero)) '(DP1 zero)) '(D (DP1 zero))  "subD implementation"))

;Helper fucntion
(define (L1<L2? L1 L2) (if (equal? L2 'zero) #f
                           (if (equal? L1 'zero) #t
                               (if (equal? (second L1) (second L2)) (and (equal? (first L1) 'D) (equal? (first L2) 'DP1)) (L1<L2? (second L1) (second L2))))))

; remainder of division between two D-DP1 numbers
; Input: D-DP1 numbers L1, L2 and L1 must be greater than L2
; Output: the remainder of the division between L1 and L2
(define (remainderD L1 L2)
  (cond [(equal? L1 'zero) 'zero]
        [(equal? L2 'zero) 'zero]
  [(if (L1<L2? L1 L2) L1 (remainderD (subD L1 L2) L2))]))

(module+ test
  (check-equal? (remainderD '(D (D (DP1 zero))) '(D (DP1 (DP1 zero)))) '(D (D (DP1 zero))) "remainderD implementation")
  (check-equal? (remainderD '(D (DP1 (DP1 zero))) '(DP1 (DP1 zero))) 'zero "remainderD implementation")
  (check-equal? (remainderD '(DP1 (DP1 (DP1 zero))) '(D (DP1 zero))) '(DP1 zero) "remainderD implementation")
  )

; rounded-down division between two D-DP1 numbers
; Input: D-DP1 numbers L1, L2 and L1 must be greater than L2
; Output: the floor of the division between L1 and L2
(define (quotientD L1 L2)
  (if (L1<L2? L1 L2) 'zero (addD '(DP1 zero) (quotientD (subD L1 L2) L2)))
  )
(module+ test
  (check-equal? (quotientD '(D (DP1 (DP1 (DP1 zero)))) '(D (D (DP1 zero)))) '(DP1 (DP1 zero)) "quotientD implementation")
  (check-equal? (quotientD '(DP1 (DP1 (DP1 zero))) '(D (DP1 (DP1 zero)))) '(DP1 zero) "quotientD implementation")
  (check-equal? (quotientD '(DP1 (D (DP1 (D (DP1 zero))))) '(DP1 (D (DP1 zero)))) '(D (D (DP1 zero))) "quotientD implementation"))

; divisibility check between two D-DP1 numbers
; Input: D-DP1 numbers L1, L2 and L1 must be greater than L2
; Output: boolean representing whether or not L1 is evenly divisible by L2
(define (divides? L1 L2)
  (if (dzero? (remainderD L1 L2)) #t #f)
  )

(module+ test
  (check-equal? (divides? '(DP1 (D (D (DP1 zero)))) '(D (DP1 (DP1 zero)))) #f "divides? implementation")
  (check-equal? (divides? '(D (D (DP1 zero))) '(D (D (DP1 zero)))) #t "divides? implementation")
  (check-equal? (divides? '(D (D (D (DP1 zero)))) '(D (D (DP1 zero)))) #t "divides? implementation"))



#|
Part B.  Greatest common divisor

Recall that the greatest common divisor of two natural numbers a and b is the
largest number that divides both a and b.

This is the pay off for coding all those functions.

The natural number c is the greatest common divisor of the natural numbers a and b
if (divides? a c) and (divides? b c) [c is a common divisor of a and b] and it is the
largest such number.  One way to calculate the gcd of a and b is
to write a loop that searches for
common divisors and records the largest one that it finds (it has to stop when
                                                              it reaches min(a,b).
 However, there is a much more succinct way of writing it using recursion:

 a)  the base case is when b is zero.  Then since anything divides 0, gcd(a,0) is a.
b)  The following is true about gcds:   gcd(a,b) = gcd(b, remainder(a,b))
That is if you calculate the remainder of dividing a by b, then the gcd of b and that
remainder is equal to gcd(a,b).  This means that you can calculate the gcd(a,b)
recursively by calling gcd with b and remainder(a,b).  The sequence of
remainders generated by the recursive gcd calls is decreasing but every
remainder by definition is non-negative.  This means that eventually the
recursion will hit the base case and terminate.  (Trying a few calculations by
hand is a good way of figuring out what your unit tests should be.)

(25 points)

Write a recursive gcd function for natural numbers in the D-DP1 reresentation.
Provide unit tests, and code comments so that the grader can easily
understand what you are doing.  At least some of your unit tests should
show correct operation when the value of the first argument is less than the
second -- we expect gcd( 20, 35) to be the same as gcd( 35, 20).
|#

; greatest common divisor of two D-DP1 numbers
; Input: D-DP1 numbers L1, L2
; Output: GCD of the numbers L1 and L2
(define (gcdD m n)
  (cond [(dzero? n) m]
        [(gcdD n (remainderD m n))]))
; Write unit tests for gcdD:  
; Write unit tests for gcdD:  
(module+ test
  (check-equal? (gcdD  '(D (DP1 (DP1 (DP1 zero)))) '(D (D (D (DP1 zero))))) '(D (DP1 zero)) "gcdD implementation")
  (check-equal? (gcdD  '(D (D (DP1 (DP1 zero)))) '(DP1 (D (DP1 (DP1 zero))))) '(DP1 zero) "gcdD implementation"))

#|
Part C.  Peano arithmetic.

Question 3.  Implement all the unfinished functions for arithmetic on
the Peano representation of natural numbers below, filling in the empty
definitions where needed. We've given you a headstart to help get you going.
Exploit the reuse of code where you can; you might find that given the Peano-specific
implemention of zero? successor and predecessor below, that addition, multiplication,
etc. work unchanged.
|#

(define (zero? n)
  (eq? n 'zero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (successor n)
  (list ''succ n))

(define (pred n)
  (if (zero? n) 'zero (second n)))

(define zero 'zero)
(define one (successor zero))
(define two (successor one))
(define three (successor two))
(define four (successor three))
(define five (successor four))
(define six (successor five))
(define seven (successor six))
(define eight (successor seven))
(define nine (successor eight))
(define ten (successor nine))

; addition of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m+n
(define (plus m n)
  (if (zero? m) n (plus (pred m) (successor n))))

; multiplication of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m*n
(define (mult m n)
  (cond [(equal? one m) n]
        [(equal? one n) m]
        [(plus n (mult (pred m) n))]))
; comparison of Peano numbers
; Input: m, n Peano numbers
; Output: a boolean = #t if the value of m < value of n and #f otherwise
(define (ltnat? m n)
  (if (zero? n) #f (if (zero? m) #t (ltnat? (pred m) (pred n)))))

; subtraction of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value is m-n if m >= n.
;         It is undefined otherwise.
(define (sub m n)
 (cond [(zero? n) m]
       [(eq? m n) 'zero]
       [(ltnat? m n) (display "undefined")]
       [(sub (pred m) (pred n))]))

; Division of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value q is the quotient of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (div m n)
    (if (ltnat? m n) 'zero (plus one (div (sub m n) n))))


; Remainder of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number whose value r is the remainder of m divided by n.
;         m = q*n + r with 0 <= r < n.
(define (rem m n)
  (cond [(equal? m 'zero) 'zero]
        [(equal? n 'zero) 'zero]
        [(if (ltnat? m n) m (rem (sub m n) n))]))

; Greatest common divisor of Peano numbers
; Input: m, n Peano numbers
; Output: a Peano number equal to gcd(m,n).
; Note:  See algorithm for GCD near top of file
(define (gcd m n)
  (cond [(zero? n) m]
        [(gcd n (rem m n))]))

(module+ test
  (check-equal? (sub ten ten) zero "10 - 10 = 0")
  (check-equal? (sub ten two) eight "10 - 2 = 8")
  (check-equal? (sub nine nine) zero "9 - 9 = 0")
  (check-equal? (sub nine one) eight "9 - 1 = 8")
  (check-equal? (sub eight six) two "8 - 6 = 2")
  (check-equal? (sub eight five) three "8 - 5 = 3")
  (check-equal? (sub seven one) six "7 - 1 = 6")
  (check-equal? (sub seven five) two "7 - 5 = 2")
  (check-equal? (sub six six) zero "6 - 6 = 0")
  (check-equal? (sub six two) four "6 - 2 = 4")
  (check-equal? (sub five four) one "5 - 4 = 1")
  (check-equal? (sub four two) two "4 - 2 = 2")
  (check-equal? (sub eight zero) eight "8 - 0 = 8")
  (check-equal? (sub two one) one "2 - 1 = 1")
  (check-equal? (sub one zero) one "1 - 0 = 0")
)

(module+ test
  (check-equal? (div ten ten) one "10 / 10 = 1")
  (check-equal? (div ten two) five "10 / 2 = 5")
  (check-equal? (div nine three) three "9 / 3 = 3")
  (check-equal? (div nine one) nine "9 / 1 = 9")
  (check-equal? (div eight six) one "8 / 6 = 1")
  (check-equal? (div eight four) two "8 / 4 = 2")
  (check-equal? (div one seven) zero "1 / 7 = 0")
  (check-equal? (div seven five) one "7 / 5 = 1")
  (check-equal? (div six six) one "6 / 6 = 1")
  (check-equal? (div six two) three "6 / 2 = 3")
  (check-equal? (div five three) one "5 / 3 = 1")
  (check-equal? (div four two) two "4 / 2 = 2")
  (check-equal? (div three two) one "3 / 2 = 1")
  (check-equal? (div two one) two "2 / 1 = 2")
  (check-equal? (div one five) zero "1 / 5 = 0")
)

(module+ test
  (check-equal? (rem ten three) one "10 % 3 = 1")
  (check-equal? (rem ten two) zero "10 % 2 = 0")
  (check-equal? (rem nine three) zero "9 % 3 = 0")
  (check-equal? (rem nine one) zero "9 % 1 = 0")
  (check-equal? (rem eight five) three "8 % 5 = 3")
  (check-equal? (rem eight four) zero "8 % 4 = 0")
  (check-equal? (rem one seven) one "1 % 7 = 1")
  (check-equal? (rem seven five) two "7 % 5 = 2")
  (check-equal? (rem six six) zero "6 % 6 = 0")
  (check-equal? (rem six two) zero "6 % 2 = 0")
  (check-equal? (rem five three) two "5 % 3 = 2")
  (check-equal? (rem nine five) four "9 % 5 = 4")
  (check-equal? (rem three two) one "3 % 2 = 1")
  (check-equal? (rem two one) zero "2 % 1 = 0")
  (check-equal? (rem one five) one "1 % 5 = 1")
)

(module+ test
  (check-equal? (gcd nine ten) one "gcd(9, 10) = 1")
  (check-equal? (gcd three nine) three "gcd(3, 9) = 3")
  (check-equal? (gcd six seven) one "gcd(6, 7) = 1")
  (check-equal? (gcd eight two) two "gcd(8, 2) = 2")
  (check-equal? (gcd nine one) one "gcd(9, 1) = 1")
  (check-equal? (gcd ten three) one "gcd(10, 3) = 1")
  (check-equal? (gcd six seven) one "gcd(6, 7) = 1")
  (check-equal? (gcd five ten) five "gcd(5, 10) = 5")
  (check-equal? (gcd three zero) three "gcd(3, 0) = 3")
  (check-equal? (gcd six one) one "gcd(6, 1) = 1")
  (check-equal? (gcd eight four) four "gcd(8, 4) = 4")
  (check-equal? (gcd eight eight) eight "gcd(8, 8) = 8")
  (check-equal? (gcd three two) one "gcd(3, 2) = 1")
  (check-equal? (gcd three five) one "gcd(3, 5) = 1")
  (check-equal? (gcd three three) three "gcd(3, 3) = 3")
)
