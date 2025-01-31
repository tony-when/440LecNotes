#lang racket

(require racket/trace) ; for dynamic function call traces

#|-----------------------------------------------------------------------------
;; Recursion

- what is a recursive function?

- what are some common "rules" for writing recursive functions?
-----------------------------------------------------------------------------|#

;; Factorial: n! = n * (n - 1) * (n - 2) * ... * 1
(define (factorial n)
  (void))

; (trace factorial)

;; Integer summation: m + (m + 1) + (m + 2) + ... + n
(define (sum-from-to m n)
  (void))

; (trace sum-from-to)

#|-----------------------------------------------------------------------------
 The above functions demonstrate *structural recursion*, because they recurse
 over the structure of the input data.
 
 We can represent natural numbers as self-referential structures to more
 explicitly demonstrate structural recursion.
-----------------------------------------------------------------------------|#

(struct Z () #:transparent)     ; zero
(struct S (pred) #:transparent) ; "successor" of pred

(define one   (S (Z)))
(define two   (S one))
(define three (S two))
(define four  (S three))

;; What is the general form of a structurally recursive function over the
;; natural numbers?
(define (nats-rec-form n)
  (void))

;; Add two natural numbers
;; add(m, n) = n, if m = 0
;;           = 1 + add(m-1, n), otherwise
(define (add-nats m n)
  (void))

#|-----------------------------------------------------------------------------
;; Proofs of correctness

- how can we prove that a structurally recursive function is correct?

- can we apply this to `factorial` and the other functions above?
-----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------
;; Tail recursion and Accumulators

- what is tail recursion?

- what is an accumulator?

- why would we use these techniques?
-----------------------------------------------------------------------------|#

(define (factorial-tail n [acc 1])
  (void))

; (trace factorial-tail)

(define (sum-from-to-tail m n [acc 0])
  (void))

; (trace sum-from-to-tail)

(define (add-nats-tail m n [acc n])
  (void))

; (trace add-nats-tail)

#|-----------------------------------------------------------------------------
;; Structural recursion on lists
-----------------------------------------------------------------------------|#

;; length: the number of elements in a list
(define (length lst)
  (void))

;; concat: concatenate the elements of two lists
(define (concat l1 l2)
  (void))

;; count-elements: count the number of elements in a tree (a nested list)
(define (count-elements tree)
  (void)) ; recursive case

;; repeat: create a list of n copies of x
(define (repeat n x)
  (void))

;; reverse: reverse the elements of a list
(define (reverse lst)
  (void))

#|-----------------------------------------------------------------------------
;; Generative recursion

- what is generative recursion, and how does it differ from structural 
  recursion?
-----------------------------------------------------------------------------|#

(define (gcd m n)
  (cond
    [(= m 0) n]
    [(= n 0) m]
    [else (gcd n (remainder m n))]))
