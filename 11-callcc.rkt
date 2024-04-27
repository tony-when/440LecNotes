#lang racket

(require racket/trace
         macro-debugger/stepper)

#|-----------------------------------------------------------------------------
;; First-class continuations

In CPS, we manually create continuations as functions and pass/invoke them.
If we neglect to pass a continuation into a function, the latter has no way of
obtaining the current continuation.

In a language with support for *first-class continuations*, the current
continuation can be obtained (but not necessarily used) at any point. This
greatly simplifies and encourages the use of continuations, and enables many
different uses cases!

In Racket we have the function `call-with-current-continuation`, aka `call/cc`,
which takes as an argument another function `proc`. When `call/cc` is called,
it captures the current continuation k, which is passed to `proc`. If `k` is
called, its argument is passed (in tail position) to the continuation, else
the result of `proc` is the result of `call/cc`.
-----------------------------------------------------------------------------|#

;; evaluate & explain:

#; (* 10 (call/cc (lambda (_) 20)))

#; (* 10 (call/cc (lambda (k) (k 20))))

#; (* 10 (call/cc (lambda (k) (k (* 2 10)))))

#; (* 10 (call/cc (lambda (k) (* 2 (k 10)))))


;; we can save a continuation!
(define *k* void)

(define (arith x y z)
  (+ x (* y (/ z (call/cc (lambda (k) 
                            (set! *k* k)
                            (k 10)))))))

(define (save-cc! x)
  (call/cc (lambda (k)
             (set! *k* k)
             (k x))))


(define (sum-to n)
  (if (= n 0)
      (save-cc! 0)
      (+ n (sum-to (sub1 n)))))

(trace sum-to)


#|-----------------------------------------------------------------------------
;; Continuations as a functional "goto"
-----------------------------------------------------------------------------|#

;; what does this function do?
(define (foo)
  (let ([kk (call/cc (lambda (k) (k k)))])
    (kk kk))) ; acts like a goto

(define (current-continuation)
  (call/cc (lambda (k) (k k))))

(define (goto k)
  (k k))

(define (print-range n)
  (let* ([x 0]
         [loop (current-continuation)])
    (println x)
    (set! x (add1 x))
    (when (< x n)
      (goto loop))))


#|-----------------------------------------------------------------------------
;; ... as an escape hatch
-----------------------------------------------------------------------------|#

;; short-circuiting product
(define (prod lst)
  (call/cc
    (lambda (break) ; break represents the continuation
      (trace-let p ([lst lst])
        (cond [(empty? lst) 1]
              [(= 0 (first lst)) (break 0)]
              [else (* (first lst) (p (rest lst)))])))))
        


#|-----------------------------------------------------------------------------
;; ... for implementing exceptions and exception handling
-----------------------------------------------------------------------------|#

(define *estack* '())

(define-syntax (try stx)
  (syntax-case stx (catch)
    [(_ body ... catch id handler)
     #'(let ([exception (current-continuation)])
         (if (continuation? exception)
             (begin
               (set! *estack* (cons exception *estack*))
               body ... 
               (set! *estack* (cdr *estack*))) ; may not get here
             ((lambda (id) handler) exception)))]))

(define (throw e)
  (let ([k (car *estack*)])
    (set! *estack* (cdr *estack*)) ; not very elegant!
    (k e)))

#;
(try (println "L1")
     (println "L2")
     (println "L3")
 catch e
     (println (format "Exception: ~a" e)))

#; 
(try (println "L1")
     (println "L2")
     (throw 'BANG)
     (println "L3")
 catch e
     (println (format "Exception: ~a" e)))

#;
(try
     (try (println "L1")
          (println "L2")
          (throw 'BANG)
          (println "L3")      
      catch e
          (begin
            (println (format "Inner: ~a" e))
            (throw 'BOOM)))
 catch e
     (println (format "Outer: ~a" e)))

;; NB: for real implementations, better to use dynamic-wind or dynamic binding
