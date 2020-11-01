#lang racket

(require rackunit)

; Complete the function definitions below:

; Question 3a: Simple Sequential Substitution

;input-spec L1 is a list (possibly nested list)
; substitution is a pair (A1 B1) where A1 and B1 are expressions or values
;output-spec  every item of L1 which is equal (matches
; to A1 has B1 instead in the result.  Racket doesn't change list
; it creates a new structure where values in it are either copied over from
; the input L1, or are substitute values.
(define (substitute L1 substitution) 
  (if (member (first substitution) L1)
      (if (equal? (first substitution) (first L1))
          (append (rest substitution) (substitute (rest L1) substitution)) (list* (first L1) (substitute (rest L1) substitution))) L1))

(module+ test
  (check-equal? (substitute '(1 2 3) '(2 7)) '(1 7 3)
     " one item substitution of a value")
  (check-not-equal? (substitute '() '(2 7)) '(1 7 3) 
    " one item substitution of a value")
  (check-equal? (substitute '(a b c) '(c (7 8 ))) '(a b (7 8))
     " one item substitution of a list")
  (check-equal? (substitute '(c a b c) '(c (7 8 ))) '((7 8) a b (7 8)) 
    " one item substitution of a list, more than once")
  (check-equal? (substitute '(1 4 5) '(4 8)) '(1 8 5)
     " one item substitution of a value")
  (check-equal? (substitute '(z h k z g) '(z (1 2 ))) '((1 2) h k (1 2) g) 
    " one item substitution of a list, more than once")
  (check-equal? (substitute '(i j h l) '(i (1 8 3))) '((1 8 3) j h l)
     " one item substitution of a list")
  )

; Question 3b: Second Sequential Substitution

;input-spec L1 is a list (possibly nested list)
; substitution1 and substitution2 are both pairs (A1 B1) and (A2 B2)
; where Ai and Bi are expressions or values.  
;output-spec  first substitutions are done of B1 for A1.  Then in that
; result substitutions are done of B2 for A2.  So if A2 is the same as B1
; then two changes might happen.
(define (sequentialSubstitute2 L1 substitution1 substitution2) (substitute (substitute L1 substitution1) substitution2))

(module+ test
  (check-equal? (sequentialSubstitute2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 17 17 17))
  (check-not-equal? (sequentialSubstitute2 '(1 3 (1 5) 3) '(3 (1 5)) '((1 5) 17)) '(1 (1 5)  17 (1 5)))
  (check-equal? (sequentialSubstitute2 '(9 8 (5 7) 1) '(8 (0 1)) '((5 7) 19)) '(9 (0 1) 19 1))
  (check-not-equal? (sequentialSubstitute2 '(1 8 (3 9) 0) '(2 (4 1)) '((2 3) 18)) '(1 (1 5)  17 (1 5)))
  )

; Question 3c: Several Sequential Substitutions

;input-spec L1 is a list (possibly nested list)
; substitutions is a list of pairs ( (A1 B1) (A2 B2) ... (An Bn)
; where Ai and Bi are expressions or values.  
;output-spec  Each specified substitution is applied to the result
; of the previous substitution.  So for each Aj  the same as some Bi for
;i<j, multiple changes might happen.
(define (sequentialSubstitute L1 substitutions) (if (null? substitutions) L1 (sequentialSubstitute (substitute L1 (first substitutions)) (rest substitutions))))

(module+ test
  (check-equal? (sequentialSubstitute '(1 2 3) '( ( 1 2) (2 3) (3 4))) '(4 4 4))
  (check-not-equal? (sequentialSubstitute '(1 2 3) '( ( 1 2) (2 3) (3 4))) '(2 3 4))
  (check-equal? (sequentialSubstitute '(8 9 1) '( ( 8 9) (9 2) (2 4))) '(4 4 1))
  (check-not-equal? (sequentialSubstitute '(4 3 1) '( ( 3 1) (1 2) (3 4))) '(2 3 4))
  )

;I took help from https://docs.racket-lang.org/reference/pairs.html