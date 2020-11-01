; <Hasham Tanveer>
#lang racket

; some quick unit tests are provided for you to check your code (eventually you should write your own!)
; note that passing these unit tests does not guarantee that your code is optimal nor always correct
; however, if you don't pass all of them, then you know you've done something wrong

; question #1B

; Input-spec: L is a list of symbols
; Output-spec: returns true if L has atleast one 'q in it, false otherwise
; Base-case: L is an empty list. if (null? L) #f
(define (AtLeastOneQ L)
  (if (null? L) false
      (if (equal? (first L) 'q)
          #t
          (AtLeastOneQ (rest L))))) ;<---  replace this line with your actual implementation

(if (AtLeastOneQ '(a q b)) "AtLeastOneQ passes first test" "AtLeastOneQ FAILED first test")
(if (AtLeastOneQ '(a b c)) "AtLeastOneQ FAILED second test" "AtLeastOneQ passes second test")
(if (AtLeastOneQ '(q q q)) "AtLeastOneQ passes third test" "AtLeastOneQ FAILED third test")
(display "\n")


; question #1C
; Input-spec: L is a list of symbols
; Output-spec: returns true if L has atleast one 'x in it, false otherwise
; Base-case: L is an empty list
(define (AtLeastOneX L)
  (if (null? L) false
      (if (equal? (first L) 'x)
          #t
          (AtLeastOneX (rest L))))) ; <---  replace this line with your actual implementation

(string-append "AtLeastOneX " (if (AtLeastOneX '(a x b)) "passes" "FAILED") " first test")
(string-append "AtLeastOneX " (if (AtLeastOneX '(a b c)) "FAILED" "passes") " second test")
(string-append "AtLeastOneX " (if (AtLeastOneX '(x x x)) "passes" "FAILED") " third test")
(display "\n")

; question #1D
; Input-spec: L is a list of symbols and z is a single symbol
; Output-spec: returns true if L has atleast one z, false otherwise
(define (AtLeastOne L z)
  (if (null? L) false
      (if (equal? (first L) z)
          #t
          (AtLeastOne (rest L) z)))) ;  <---  replace this line with your actual implementation

(string-append "AtLeastOne " (if (AtLeastOne '(a x b) 'x) "passes" "FAILED") " first test")
(string-append "AtLeastOne " (if (AtLeastOne '(a b c) 'x) "FAILED" "passes") " second test")
(string-append "AtLeastOne " (if (AtLeastOne '(x x x) 'x) "passes" "FAILED") " third test")
(display "\n")

; question #1E
; Input-spec: L is a list of symbols and z is a single symbol
; Output-spec: returns true if L has no more than one occurrence of z, false otherwise
(define (AtMostOne L z)
  (if (null? L) #t
      (if (equal? (first L) z) 
          (not (AtLeastOne (rest L) z))
          (AtMostOne (rest L) z)))) ; <---  replace this line with your actual implementation

(string-append "AtMostOne " (if (AtMostOne '(a x b) 'x) "passes" "FAILED") " first test")
(string-append "AtMostOne " (if (AtMostOne '(a b c) 'x) "passes" "FAILED") " second test")
(string-append "AtMostOne " (if (AtMostOne '(a x x) 'x) "FAILED" "passes") " third test")
(string-append "AtMostOne " (if (AtMostOne '(x x x) 'x) "FAILED" "passes") " fourth test")
(display "\n")

; question #1F
; Input-spec: L is a list of symbols and z is a single symbol
; Output-spec: returns true if L has exactly one occurrence of z, false otherwise
(define (ExactlyOne L z)
  (if (null? L) false
      (if (equal? (first L) z) 
          (not (AtLeastOne (rest L) z))
          (ExactlyOne (rest L) z)))) ; <---  replace this line with your actual implementation


(string-append "ExactlyOne " (if (ExactlyOne '(a x b) 'x) "passes" "FAILED") " first test")
(string-append "ExactlyOne " (if (ExactlyOne '(a b c) 'x) "FAILED" "passes") " second test")
(string-append "ExactlyOne " (if (ExactlyOne '(a x x) 'x) "FAILED" "passes") " third test")
(string-append "ExactlyOne " (if (ExactlyOne '(x x x) 'x) "FAILED" "passes") " fourth test")
(display "\n")

