#lang racket

(require racket/trace)

#|-----------------------------------------------------------------------------
;; Review: user defined types with `struct`
-----------------------------------------------------------------------------|#

;; define a `widget` type
(struct widget          ; type name
  (name purpose price)  ; attributes
  #:transparent)        ; when printing a widget, show its attributes

;; we get the following functions for free:
;; - `widget`: constructor
;; - `widget?`: predicate that returns #t for widget values
;; - `widget-name`: retrieve `name` attribute
;; - `widget-purpose`: retrieve `purpose` attribute
;; - `widget-price`: retrieve `price` attribute

(define w1 (widget "wrench" "wrenching" 9.99))
(define w2 (widget "plier" "pliering" 12.99))


;; define a `doohickey` type that is a sub-type of `widget`
(struct doohickey widget (special-power) #:transparent)

(define d1 (doohickey "thingamajig" "thinging" 199.99 "time travel"))

;; we can also match against structs
(define (which-widget? w)
  (match w
    [(widget "wrench" _ _) "It's a wrench!"]
    [(doohickey "thingamajig" _ _ _) "It's a thingamajig!"]
    [(? widget?) "It's some sort of widget"]
    [_ "I don't know what this is"]))



#|-----------------------------------------------------------------------------
;; Our language

We're going to start with a very simple language and slowly add to it. Our
first iteration will support integer literals, the binary arithmetic operations
 +` and `*`, and `let`-bound variables. The syntax will mirror Racket's. 
-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 "(+ 1 2)")

(define p2 "(* 2 (+ 3 4))")

(define p3 "(+ x 1)") 

(define p4 "(* w (+ x y))")

(define p5 "(let ([x 10])
              (+ x 1))")

(define p6 "(let ([x 10]
                  [y (+ 1 2)])
              (+ x y))")

(define p7 "(let ([x 10])
                (+ x (let ([x 20] [y 30])
                       (* x y)))))")

;; for demonstrating strict/lazy evaluation
(define p8 "(let ([x (+ 1 2)])
              20)")

(define p9 "(let ([x (+ 1 2)])
              (* x (+ x x))))")


#|-----------------------------------------------------------------------------
;; Parser

Review: What is parsing?

- Input string (source language) => Syntax object

- A syntax object contains information about the structure of the code. Often,
  we use a *tree* as an underlying representation.

  - E.g., the code "(let ([x 10]) (+ x 1))"
          
          may be parsed to the syntax tree:

                    let
                   /   \
                  x     +
                 /     / \
                10    x   1

  - Racket sexps are a programmatic way of representing syntax trees!

- Since our syntax mirrors Racket's, we may rely on Racket's reader to parse
  our input to produce an initial syntax tree.
-----------------------------------------------------------------------------|#

;;; using Racket's reader
#; (read)

#; (read (open-input-string "(+ 1 2)"))

#; (read (open-input-string p1))

#; (read (open-input-file "demo.txt"))

(define (read-string str)
  (read (open-input-string str)))


#|-----------------------------------------------------------------------------
;; Parser (continued)

- Our syntax tree doesn't currently contain much information besides tokens
  pulled directly from the input string.

- Next, we will recursively descend through the syntax tree, "decorating" its
  nodes with information that can help us expand and evaluate it.
-----------------------------------------------------------------------------|#

;;; Some types for decorating our syntax tree

;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)


;; Parser (recursive-descent)
(define (parse sexp)
  (match sexp
    ;; integers
    [(? integer?) 
     (int-exp sexp)]

    ;; binary + operators
    [(list '+ lhs rhs)
     (arith-exp "PLUS" 
                (parse lhs)   ; "recursive-descent" parsing
                (parse rhs))] ; "recursive-descent" parsing

    ;; binary * operators
    [(list '* lhs rhs)
     (arith-exp "TIMES" 
                (parse lhs) 
                (parse rhs))]

    ;; variables
    [(? symbol?)
     (var-exp sexp)]

    ;; single variable let expressions
    ; e.g., (let ([x 10]) (+ x 1))
    ; e.g., (let ([x (+ 1 2)]) (* x 5))
    #; [(list 'let (list (list id val)) body)
     (let-exp (list id) 
              (list (parse val)) 
              (parse body))]

    ;; multiple variable let expressions
    ; e.g., (let ([x v1] [y v2] ...) body)
    [(list 'let (list (list id val) ...) body) ; "..." collects values in lists
     (let-exp id (map parse val) (parse body))]

    ;; error on unparseable input
    [_ 
     (error (format "Can't parse ~a" sexp))]
  ))


#|-----------------------------------------------------------------------------
;; Interpreter

- The interpreter's job is the take the (decorated) syntax tree and evalute it!
-----------------------------------------------------------------------------|#

;; Interpreter
(define (eval expr [env '()]) ; start eval with an empty env, by default
  (match expr
    ;; integers evaluate to themselves
    [(int-exp val)
     val]

    ;; use Racket's +/* to evaluate arithmetic expressions
    [(arith-exp "PLUS" lhs rhs)
     (+ (eval lhs env) (eval rhs env))] ; recursive evaluation
    [(arith-exp "TIMES" lhs rhs)
     (* (eval lhs env) (eval rhs env))] ; recursive evaluation

    ;; variables are looked up in the environment
    [(var-exp id)
     (cdr (assoc id env)) ; "strict" evaluation 
     ;(eval (cdr (assoc id env)) env) ; "lazy" evaluation 
     ]

    ;; single variable let expression
    #; [(let-exp (list id)
              (list val)
              body)
     (eval body (cons (cons id (eval val env)) env)) ; "strict" evaluation 
     ;(eval body (cons (cons id val) env)) ; "lazy" evaluation 
     ]

    ;; multiple variable let expressions, lazy evaluation
    #; [(let-exp (list id ...)
              (list val ...)
              body)
     (let ([nvars (map cons id val)])
       (eval body (append nvars env)))]
    
    ;; multiple variable let expressions, strict evaluation
    [(let-exp (list id ...)
              (list val ...)
              body)
     (let ([nvars (map cons id
                            (map (lambda (v) (eval v env))
                                 val))])
       (eval body (append nvars env)))]))

(trace eval)

;; define a REPL
(define (repl)
  ;; Read (then parse)
  (let ([ir (parse (read))]) ; use interactive form of read
    ;; Evaluate then Print
    (println (eval ir))
    ;; Loop
    (repl)))
