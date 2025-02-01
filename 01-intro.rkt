#lang racket ; <- determines the language (and syntax) used in this file

#|
First day Programming in Racket
-	Files ending in .rkt are the racket source files
-	Racket is not a language, it’s a collection of languages
•	So when you have a racket source file, it doesn’t tell you what language you are programming in
•	You could be programming in basic racket, a dialect of racket

-	You need to say what version of what language you are using
•	Semicolon is for a comment

; Multi line comment start with a #| (hash bar) and ends with a |# (bar hash)

|#

#|
- Racket is a dialect of LISP
- LISP is one of the oldest programming langauges still in active use
- It is beat by Fortran
- LISP was  originally the language for AI researchers back in the 1960s
- LISP is also a scheme dialect
- LISP is kind of like a family of languages that all share some very specific features

- LISP code is a chalk full of parenthesis RIP (have to get use to it, love hate thing)

- Universal feature of the LISP family of programming languages is homoconicity, they are 
- homoiconic in terms of data and code representation
- homo meaning the same icon means the representation of a thing
- homoiconic means the same representation
- the data and the code in Racket take the same form
- In racket or any LISP language, the code and data look the same, same representation, its just a LISP
- its a language that is malleable in terms of if you have a program, you can manipulate it like a data structure
|#


#|-----------------------------------------------------------------------------
;; Racket at a glance

- LISP/Scheme dialect

  - LISP = "LISt Processing", 
           "Lots of Insidious, Silly Parentheses", 
           "Lost In a Sea of Parentheses"

- Homoiconic; i.e., shared representation for code & data

- Supports (but does not enforce) functional style

  - First class functions
  - Higher-order functions
  - Anonymous functions

- Dynamically and Strongly typed 
    - (a programming language that uses dynamic typing means variables can hold any type, type of variable can change)
    - Static typed variable, its type cannot change over time
    - Strong typing means it is the type enforced, Racket is strongly type but it is also dynamic

  - Sister language "Typed Racket" is statically typed

- Pass by value with pointer semantics
    - explanation:
          - we duplicate the reference, not the object itself
          - key term: pointer semantics
          - when you pass by value with pointer semantics, what this means is like:
                    - I create an object, it's somewhere in memory,
                    - I write down the reference to the object (thats it's address),
                    - Then I copy the reference in another variable (being the pointer)
                    - Now, theres I and another variable, we both have a copy that references the same object
                    - However it just only references to the address of the object,
                    - If the copied variable makes changes to the referenced object, 
                    - then my current object WILL be affect by the changes
            
            Summary:        
            In Racket, when you use pass by value with pointer semantics (or similar concepts 
            involving mutable objects like boxes, vectors, or hash tables), if the copied reference (pointer) is 
            used to modify the object, the changes will also be reflected in the original object. 
            This is because both references (the original and the copied one) point to the same memory location.        

- Lexically scoped
    - fancy term, where the thing (variable) is defined, thats where its valid
    - explained later in another lecture


- Heap-based storage with garbage collection
    - when you allocate an object, it goes to the heap, and when it is no longer referenced, garbage collector gets rid of it
-----------------------------------------------------------------------------|#

;; a bit of Racket to whet your appetite
(define (quicksort < l)
  (match l
    ['() '()]
    [(cons x xs) 
     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
       (append (quicksort < xs-lt) 
               (list x) 
               (quicksort < xs-gte)))]))

#|
- The level of nesting indicates the scoping of expressions
- Brackets and Parenthesis are the same thing (just a conventional thing to use both)

- explanation of the code above
  - defining a quicksort function that takes 2 parameters, 
  - it takes a parameter that i am reffering to with a symbol < (less than)
  - and I am taking a 2nd parameter which is a list (l)
  - quicksort is just a function that sorts something recursively

  - if you make a change to your file, you have to reload the terminal
    - In Dr Racket, you just press CTRL R to reload the command line
    - In VScode, you can just hit the terminal button on the top right or ,en (shortcut for 're-enter the file')

|#

#| 
- To call a procedure (or function call, same thing) in Racket is always surrounded by parenthesis ()
  - ex. (quicksort < '(5 3 1 4 2))
    - breakdown: (quicksort <- this is the procedure followed by the '<' less than function, followed by the list '(5 3 2 1))
    - output: '(1 2 3 4 5)
    - I called quicksort with < (less than) being the function that I use to compare opbjects in this list 
  - ex. (quicksort > '(5 3 1 4 2))
    - also works this way too, sorts in decending order

  - I can use symbols as variable names
  - function calls kind of look like list
    - a list is just a bunch of values surrounded by parenthesis
    - a function call is also just a bunch of symbols surrounded by parenthesis
|#



#|-----------------------------------------------------------------------------
;; Basic syntax

The essential syntactic unit of Racket is the *S-expression*, or sexp.

        - Only one piece of syntax I need to be really familiar with,
        - something known as the S-expression,
        - the S-expression is the universal bit of syntax I need to know to be able
        - to decipher racket code

  - two types of s-expression (sexp)
A sexp is either an *atom* or a *list*.

---------------------------------------------------------------------------------------
- An atom is one of:

  - a Number 
    - e.g., 42, -23, 3.14, 5/8, 2+3i, 6.02e+23

  - a '#' prefixed value 
    - e.g., booleans #t and #f, chars #\a, #\newline)
    - 'hash pre-fixed values', these are for certain special types of values

  - a String
    - e.g., "hello", "foo\nbar"

  - a Symbol, written as an identifier made of any chars except #()[]{}|\'"`,;    <-- Note: cannot include these characters
    - e.g., foo, hot-enough?, <=, list->set, call/cc, bang!!!                     <-- Note: these are all valid symbols
    - symbols can be used as a name of a variable, function, special form, racket symbols are more permissive
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
- A *list* is a sequence of sexps (recursion!) separated by spaces, 
  enclosed by parentheses

  - [] and {} can also be used as delimiters
---------------------------------------------------------------------------------------

-----------------------------------------------------------------------------|#

;; Let's write some (syntactically) valid sexps!
;; Note: `#;` is a convenient, special "sexp comment"

;; basic atoms:
#; 52
#; -5
#; 5/8
#; "hello"
#; #\a
#; #t
#; #f
#; foo ;; <-- that is a perfectly VALID symbol, it has to be defined before evaluated, you can ask something that is syntactically valid, but semantically meaningless (not semantically valid)

;; #;  <-- this is a special type of comment in a racket source file, what is does it comment out the next sexp whereever it appears
;; examples
#; 52 -5 5/8
#; 
21
1000

;; this is convenient b/c if you're working on a source code, you may want to comment out a sexp, you comment out the next line

#; ()      ;; <-- valid empty list s-expression

#; (1 2 3) ;; <-- list of atoms, each atom is a sexp

#; (foo)   ;; <-- a list that contains a symbol

#; (define (foo x y)) ;; <-- a list that encloses one s-expression (define), and another list that is a valid s-expression, which itself encloses three symbols


#|-----------------------------------------------------------------------------
;; Evaluating sexps

- Numbers, strings, and `#` prefixed values evaluate to themselves

- Symbols are identifiers whose *bindings* are looked up in the current scope
      - symbols are treated as an identifier, a symbol is an identifier bound to something
      - the binding is determined by the current scope
          - in the current scope, if the identifier is just a variable, then we evaluate the identifier, we get back the binding (the bound value)

  - If an identifier corresponds to a *variable*, its value is returned

- For lists, if the first element evaluates to a *function*, that function is
  applied to the rest of the values in the list
    - explanation: if i give you a list, and the first thing in the list is a function, then you apply the function to the rest of the elements in the list

  - e.g., `(f x y z)` applies function `f` to the values `x`, `y`, and `z`

  - Arguments are passed *by value*; i.e., the argument sexps are evaluated 
    first, then their results are passed to the function

- The first element of a list may also be a *special form*, which is applied 
  like a function to its arguments, but with special semantics

-----------------------------------------------------------------------------|#

;; try evaluating some sexps

;; call functions by putting them in a list (function argument_1 argument_2)
#; (+ 10 10) ;; <-- evaluates to 10
#; (* 10 10) ;; <-- evaluates to 100
#; (abs -10) ;; -10

#; (+ 1 2 (+ 3 4)) ;; 10
#; (+ 1 2 (+ 3 4) (+ 5 8)) ;; 23

#;                 (+ 1 
#;                    2
#;                    (+ 3 4)
#;                    (+ 5 8))     ;; can be written like this too, evaluates to 23
                  ;; ORDER OF EVALUATION: the arguments have to be evaluated first before the function is called
                  ;; however, in this case, the first thing to be evaluated is the 1, then 2, then (3) then (4) then (3 + 4 = 7), etc

;; functions are just user-defined functions, + is just code attached to the symbol +, functions are all equal, + and quicksort are just user defined functions

#; (= (abs -10) (+ -5 -5)) ;; equals is testing for equality, evaluates to #f

#; (println "hello world") ;; print outputs "hello world", this is an output, not a result of the sexp: (println "hello world")

#;                (if (< 1 5)
#;                  (println "less than")
#;                  (println "greater than"))
                  ;; is if a function? read below for explanantion


#;              (list
#;               (< 1 5)
#;               (println "less than")
#;               (println "greater than"))
              ;; if list is a function call, it starts by evaluating the arguments, the first 1 < 5 is true,
              ;; then it evaluates the "less than" which prints, then it also does the same for "greater than"
              ;; what does it return? print statements return nothing, "void" is null
              ;; return this: '(#t #<void> #<void>)
                  ;; - true, nothing, and nothing which puts it into a list: '(#t #<void> #<void>) (TRUE, VOID, VOID)

;; The reason the code snippet (list (< 1 5) (println "less than") (println "greater than")) prints out 
;; less than and greater than before returning a list is due to the evaluation of arguments in a function call in Clojure.

;; 1.	Clojure evaluates function arguments eagerly:
;; o	When you call a function like list, all of its arguments are evaluated before the function itself is invoked.

;; 2.	Side-effects during evaluation:
;; o	In your example, println is one of the arguments passed to list. Since println is a function with a side-effect (printing to the console), 
;; it is executed during the argument evaluation step, not when list is being constructed.

#;                (if (< 1 5)
#;                  (println "less than")
#;                  (println "greater than"))
                  ;; returns only "less than"
                  ;; if is not a function:      NOTE: a function always evaluates all of its arguments
                  ;; the if did not evaluate (println"greater than") s-expression here
                  ;; structure: if, test case, if_return_clause, else_return_clause
                  ;; if is a *special form*
                  ;; it would be comparatively to a statement in other languages
                  ;; special forms are different from functions in that they do not always evaluate its arguments

                  ;; if is not a function b/c in racket if is not a statement, if is also not a control structure in racket
                  ;; this is because we can build our own, the tools to build these things are baked in the language
                  ;; they're just syntactic structures

                  ;; extra fun thing: how do you make an if without using an if?
                          ;; there are a handful of very low level primitives that are baked into the language
                          ;; there are some axioms you have to have for other things to follow

                  ;; additionally, if is a control structure in imperative languages, just a control flow

;; all of these different things we evaluated are one of three categories of things
;; they are: atoms, functions, or special forms

;; a function that returns nothing is called a void function like println in racket return nothing, just prints out on the screen
;; #<void> represents the lack of a return value from a function call like println

;; recap: there are three different ways of evaluating s-expressions
;; - atoms: evaluate to themselves, only exception is if the atom is a symbol, is it a function:? is it a special form?
;; - list are evaluated depends if the first thing is a function, then you evaluate arguments and pass them into the function
;; - if it is not a function, then it is a special form, special forms have no rule, each one defines its own semantics for evaluation

#|-----------------------------------------------------------------------------
;; Quoting

The special form `quote` can be used to prevent the normal evaluation of a 
sexp, and instead just return the value of the sexp.

- ex. '(abs -5) is the same thing as (quote (abs -5))
- why are quotes useful?
    - when you quote something you get an s-expression
    - b/c we are building data structures
    - we can take a list and interpret it as a collection of pieces of data or a piece of code

    - some class ex.
          '(define (quicksort < l)
            (match l
              ['() '()]
              [(cons x xs) 
              (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
                (append (quicksort < xs-lt) 
                        (list x) 
                        (quicksort < xs-gte)))]))
    
    - if I quote this piece of code, I return a list that represents the code,
    - then I can manipulate this code as a data structure

    (list 'a '(x y) 'b) --> '(a (x y) b)
    - returns a single list containing 3 separate sexp
    - its much simpler to do this way:
              '(a (x y) b) --> '(a (x y) b)

    - '(a (x y) (+ 1 2)) --> '(a (x y) (+ 1 2))     so the (+ 1 2) is not evaluated because the whole thing is treated as a quote
    - (list 'a '(x y) (+ 1 2)) --> '(a (x y) 3)     so the (+ 1 2) is evaluated, 'manually unquoted this sexp'

    - sometimes we do this so frequently, there is a special form called, quasiquote
        - meaning: kind of quoting the thing, 'kind of quote'
    - (quasiquote (a (x y) (+ 1 2))) --> '(a (x y) (+ 1 2))      same as quoting
    
    -difference, when you quasiquote something, you can also unquote some component of it
    - (quasiquote (a (x y) (unquote (+ 1 2)))) --> '(a (x y) 3)
        - when I mark a subexpression as unquoted, quasi quote descends down the tree, it says its unquoted so I will evaluate that
        - think like this:
              - quote: don't evaluate
              - unquote: do evaluate 

                (quasiquote (+ (unquote (* 2 5) )
                               (unquote (+ 1 2))))
                evaluates to: '(+ 10 3)

                (quasiquote (unquote (+ (* 2 5)
                                        (+ 1 2))))
                evaluates to: 13

                basically, just unquote everything,
                additionally, unquote is a way of marking a subexpression as evaluatable

                we usually don't type quasiquote,
                we just type the back tick ` to mark the entire thing as a quasiquote
                    - to unquote something, you just use a comma ,

                ex. `(+ ,(* 2 5)
                        ,(+ 1 2))
                evaluates to: '(+ 10 3)

                ex. `,(+ (+ 10 5) (+ 2 3))
                evaluates to: 20

                Splicing a list: (splicing means I take a list, and I put it into the list that contains it)
                ex. `(a b (c (d) e) f g)   -->  '(a b (c (d) e) f g) nested list inside
                ex. `(a b ,@'(c (d) e) f g) --> '(a b c (d) e f g)   essentially, I am move it up into the containing list
                
                we will use this form a few times, later, we call this splicing, splice a list into another list


    Why are we doing this?
      - well we are building pieces of code, we have the ability to
        evaluate pieces of the sexp, and evaluate all the expression later

`quote` also has the short form `'` (i.e., syntactic sugar):

  (quote x) == 'x
  (quote (1 2 3)) == '(1 2 3)

There is also another special form, `quasiquote`, which can be used with 
`unquote` to selectively build sexps with some evaluated sub-sexps.

  (quasiquote x) == `x
  (quasiquote (x (unquote y) z)) == `(x ,y z)

Quasiquoting is particularly useful for metaprogramming!
-----------------------------------------------------------------------------|#

;; try evaluating some quote/quasiquote/unquote-based forms

#|-----------------------------------------------------------------------------
;; Variables

Define global variables with `define` and local variables with `let` and `let*`
-----------------------------------------------------------------------------|#

(define *course-id* "CS 440")  ; sometimes we use "earmuffs" for global vars

(define bignum (expt 2 50))

;; introducing local vars
(define cnum (let ([x 10]
                   [y 44])
               (* x y)))

;; find roots of x^2 + 3x - 4 = (x - 1)(x + 4) = 0
;;  - need let* to use earlier vars when defining later ones
(define roots (let* ([a  1]
                     [b  3]
                     [c -4]
                     [disc (- (* b b) (* 4 a c))]
                     [sqr-disc (sqrt disc)])
                `(,(/ (+ (- b) sqr-disc) (* 2 a))
                  ,(/ (- (- b) sqr-disc) (* 2 a)))))


#|
in racket, if we want to define variables we use define
(define *variable-name* "variable value") typical convention in racket, we call the stars earmuffs,
earmuffs designate a global variable

ex. *course-id* --> CS 440
    - this is an example of a symbol bound to a value

ex. (define bignum (expt 2 50))
    - bignum is bound to this expression (expt 2 50)
    - when you bind a variable to an expression, that expression is evaluated
    - expt is just an exponent so:
              - input: bignum --> output: 2^50 = 1125899906842624
                

    - instead of defining global variables for x and y, im using a 'let' special form
    - define and let are the two most common special forms for creating variables
    - let creates local variables  


            ex. (let ([a 5]
                      [b 6]
                      [c 10]
                      [foo 20])
                  (+ (* a b)
                     (- foo c)))

                     NOTE: this is the proper indentation
          output: 40
    
    - the first thing following a let is a list
        - a list of local variables along with their initial values
    - after that, within the second expression, I can use those local variables
    - the scope of these local variables are locally scoped so they wont work outside the let form
    - the entire let form itself is an expression,
        - ex. 40, is the value of this expression    (+ (* a b)
                                                        (- foo c)))
        - evaluated with the local variables, a, b, c, foo


              ex. (define cnum (let ([x 10]
                                    [y 44])
                                (* x y)))

              - cnum is bound to the value returned by the let expression
              - evaluation rule is a bit more complicated since let is a special form not a function


              ex. (define roots (let* ([a  1]
                                        [b  3]
                                        [c -4]
                                        [disc (- (* b b) (* 4 a c))]
                                        [sqr-disc (sqrt disc)])
                                    `(,(/ (+ (- b) sqr-disc) (* 2 a))
                                      ,(/ (- (- b) sqr-disc) (* 2 a)))))

          - a let form can be more complicated
          - we are computing roots of the quadratic formula
          - we are using let* 
            - let* is a version of let is where when you introduce new variables,
            - you can use earlier variables
                - note: with let, you can't access a previously introduced variable within the same sequence
                - so:       (let ([x 10] [y x])) does not work!
                - however:  (let* ([x 10] [y x])) works!

                - why the standard let doesn't support this seemingly fairly basic feature?
                  - answer: the implementation of the syntactic form let is very simple,
                  - racket is very much like a programming language designer's language
                  - so you have the option to have the most barebones, efficient form of a
                  - special form possible, or have one that is semantically complex
                  
              - What happens when we evaluate a list?
                  - you need a function/special form/symbol in the first posititon of the list (+ 10 10)
                  - you can't evaluate a list that contains just numbers because a number is not a function
                  - however you can quote, returns back the s-expression


              - This will not work 
              ex. (define roots (let* ([a  1]
                                        [b  3]
                                        [c -4]
                                        [disc (- (* b b) (* 4 a c))]
                                        [sqr-disc (sqrt disc)])
                                    ((/ (+ (- b) sqr-disc) (* 2 a))
                                      (/ (- (- b) sqr-disc) (* 2 a)))))

              - This will work because you preface it 
              - with a function list or quote the whole thing, then unquote the values you want to evaluate
                                    (list (/ (+ (- b) sqr-disc) (* 2 a))
                                      (/ (- (- b) sqr-disc) (* 2 a)))))

              - tldr; you can't evaluate a list where the first item is not either a function or a special form
|#


#|-----------------------------------------------------------------------------
;; Pairs and Lists

Programmatically, lists are built out of linked pairs, where a pair is 
constructed using the `cons` function:

  (cons x y)

                  ex. (cons 1 2) --> '(1 . 2)
                  - return this syntactic sugar representation of the cons cell
                  - this represents a node with 1 in the first half and 2 in the second half
                  
                  access:
                  - first half: (car (cons 1 2)) --> 1
                  - second half: (cdr (cons 1 2)) --> 2

                  - cons is a way of constructing a cons cell
                    - you could put whatever in the cells

                  ex. (define testcons (cons 21 69))
                  (car testcons) --> 21
                  (cdr testcons) --> 69

The functions `car` and `cdr` access the first and second slots of a pair.

  (car (cons x y)) => x
  (cdr (cons x y)) => y


            important notes when creating a cons list
            (cons 1 (cons 2 (cons 3 empty))) --> '(1 2 3)    VALID
            (cons 1 (cons 2 (cons 3 '())))   --> '(1 2 3)    VALID

            empty == '()     NOTE: both are essentially equivalent, they are necessary to terminate the linked-list

            (cons 1 (cons 2 (cons 3 ())))    --> ILLEGAL
                - this is illegal because () is syntactically invalid as a function call, meaning it has no function to apply


            Functions and Cons
            (define testlist (cons 1 (cons 2 (cons 3 empty))))
            (car testlist) --> 1
            (cdr testlist) --> '(2 3)
            (car (cdr testlist)) --> 2
            (car (cdr (cdr testlist))) --> 3

            how to build this list using con cells?
            '(1 (2 3) 4)

            (cons 2 (cons 3 empty))     for (2 3)
            (cons 1( cons (cons 2 (cons 3 empty)) (cons 4 empty)))


            * Key Concept * all list in rackets are built up of con cells
            - why is that useful
              - If I were to give you a function as a list, as an s-expression, 
              - can you extract the name of the function given the function s-expression?

            ex.

            (define code '(define (quicksort < l)
                  (match l
                    ['() '()]
                    [(cons x xs) 
                     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
                       (append (quicksort < xs-lt) 
                               (list x) 
                               (quicksort < xs-gte)))])))

             code -->                         NOTE: this now becomes the code for quicksort, returns the whole s-expression as a list
             (define (quicksort < l)
              (match
                l
                ('() '())
                ((cons x xs)
                (let-values (((xs-gte xs-lt) (partition (curry < x) xs)))
                  (append (quicksort < xs-lt) (list x) (quicksort < xs-gte))))))

            
              (car code) --> 'define          NOTE: returns define because it is the first symbol in the s-expression for quicksort
              
              (cdr code) -->                  NOTE: returns the rest of the list
                        '((quicksort < l)
                          (match
                          l
                          ('() '())
                          ((cons x xs)
                            (let-values (((xs-gte xs-lt) (partition (curry < x) xs)))
                              (append (quicksort < xs-lt) (list x) (quicksort < xs-gte))))))

              (car (cdr code)) --> '(quicksort < l)     NOTE: I take the cdr of the code which gets the rest of the list and we take the car of that which gets use the second s-expression

              (car (car (cdr code))) --> 'quicksort     NOTE: return name of function

              some personal testing:

              (car (cdr (car (cdr code)))) --> '<
              (cdr (cdr (car (cdr code)))) --> '(1)

              (car (cdr (cdr code))) -->

                                '(match
                                  l
                                  ('() '())
                                  ((cons x xs)
                                  (let-values (((xs-gte xs-lt) (partition (curry < x) xs)))
                                    (append (quicksort < xs-lt) (list x) (quicksort < xs-gte)))))

                
             Important: Car and Cdr it's more simple than you think!
             - if you forget just reread and try again kuru kuruuuuuuuuu!!!

             The way that special forms work, is in fact, the special form takes the code,
             and decides what pieces of code to evaluate, and put off evaluation if it wants to:

            Special Forms have control over evaluation
             1. Which arguments to evaluate
             2. When to evaluate them (or whether to evaluate them at all)



A list is either:  <-- ******************************************** IMPORTANT

- empty (expressed as `null`, `empty`, or `'()`), or
- a pair whose `car` refers to an element and whose `cdr` to a list

    - recursive definition: meaning that the cdr of a list that can only ever be another list that is empty or a con cell
      - (cons 1 2) is this a list? (list? (cons 1 2)) --> #f WHY IS THIS FALSE?
          - this is not a list, but it is a pair
          - racket knows something is a list or not, it has a notion of type

          From AI:

          A list must end with null (the empty list).
          In (cons 1 2), the cdr is 2, which is not a list. It’s just a number.
          Because the cdr is not a list, the entire structure is not a valid list.

        ex. (first (cons 1 2)) --> VIOLATION ERROR why? --> expected: (and/c list? (not/c empty?))
          - con cells are not a list, not a valid list
          - there are functions that have type restrictions in racket



Useful functions:
  - `cons`: constructs a pair from an ele ment and a list
  - `car`:  returns the first element of a pair
  - `cdr`:  returns the rest of a pair
  - `pair?`: tests whether an object is a pair
  - `list`: constructs a list from a sequence of elements
  - `first`: returns the first element of a non-empty list
  - `rest`:  returns the rest of a non-empty list  
  - `list?`: tests whether an object is a list
  - `empty?`: tests whether a list is empty

  -------------------------------------- TESTING -----------------------------------------------
    - These are all functions that have to do with list
    - a pair is a cons
      - ex. (pair? (cons 1 2)) --> #t
        - its true because if you build something using a cons, its a pair
        - a pair holds exactly two values which in itself is a pair, 2 values haha ez

      - ex. (pair? (list 1 2)) --> #t
        - this is true because a list is just a series of con cells hooked up to one another

      ex. (first (cons 1 2)) --> fails ERROR

      ex. (define templ (list 1 2 3)) --> '(1 2 3)    NOTE: creating a temporary list
          - (first templ) --> 1 return the same thing (car templ)
          basically, car and cdr are the type agnostic version of first and rest

      Type-agnostic means that the function does not impose any restrictions on the type of data it operates on.
      For car and cdr, this means they can work with any type of value stored in the car or cdr of a pair,
      whether it’s a number, string, list, symbol, procedure, or even another pair.

      car and cdr are type-agnostic functions. This means they do not care about the type of the data stored 
      in the car or cdr of a pair. They simply retrieve the value stored in the car (
      first element) or cdr (second element) of a pair, regardless of what that value is

-----------------------------------------------------------------------------|#

;; pairs aren't necessarily lists!

(define pair1 (cons 1 2))

(define pair2 (cons 3 pair1))

(define pair3 '(1 . 2)) ; `.` indicates that the next value is the cdr of a pair

(define pair4 '(3 1 . 2)) ; = (cons 3 (cons 1 2))


;; build and take apart some lists

(define lst1 '())

(define lst2 (cons 1 '()))

(define lst3 (cons 1 (cons 2 (cons 3 '()))))

(define lst4 (cons 1 (cons "hello" (cons #t '()))))

(define lst5 (list 1 "hello" #t))

(define lst6 '(1 "hello" #t))

(define lst7 '(1 "hello" #t . ()))

#; (define lst8 (list 1 (2 3) ((4 5) (6 7)))) ; what's wrong with this?

(define lst9 '(1 (2 3) ((4 5) (6 7))))

(define lst10 '(a (b (c d) (e f)) g))


#|

    some equivalences:

    '(1 2 3) == (list 1 2 3) == (cons 1 (cons 2 (cons 3 empty)))    NOTE: ALL THE SAME

(define lst7 '(1 "hello" #t . ())) 
- the dot notation is something you can use if you use a quoted list
- first understand that (cons 1 2) --> '(1 . 2)
    - that dot is explicitly saying that you have a con cell and the 2nd half is a value
    - that is not another list

    based on that, what is this suppose to be?
    - ex. '(1 2 . 3) --> '(1 2 . 3)
     
    - answer: ex. 
      - (cons 1 (cons 2 3)) --> '(1 2 . 3)      NOTE: shocking right its the same 
      - but this is not a legal list
    
    ex. (cons 1 (cons 2 empty)) --> '(1 2) 
    - now this is a legal list
    - you can also write this in quoted form:
      - ex. '(1 2 . empty) --> '(1 2 . empty)    NOTE: why is it different?
          - it is because of this: ' the quote doesnt eval so its just an empty symbol
          - so you just need to unquote the empty by:
            - ex. '(1 2 . ()) --> '(1 2)
            - ex. `(1 2 . ,empty) --> '(1 2)     NOTE: works to in quasiquote
            - OR ex. `(1 2 . ()) --> '(1 2)      NOTE: same too!!!!

            TLDR:
            becareful when you use a symbolic words, if put it in a quoted form, it will quote it for you
            remember you have to unquote it

|#


#|
                Some RECAP

(1 2) --> error (this is a list but theres an error why?) 
          - first s-epression must be a special form or function

how to make a valid list?
(cons 1 (cons 2 empty)) --> '(1 2)

(cons 1 2) --> is a valid pair but not a list... syntactically you can't evaluate it
(list? (cons 1 2)) --> #f

(list (+ 1 2) (+ 3 4)) --> '(3 7)

(quote ((+ 1 2) (+3 4))) --> '((+ 1 2) (+ 5 6)) (returns an unevaluated s-expression)

List is a function
Quote is a special form

(list (1 2) (3 4)) --> error (it doesnt evaluate because this is a function list, passing it 2 s-expression, try passing it and it fails)

              (if (< 1 2)                        --> "hello"
                    (println "hello")
                    (println "bye"))

              (if (< 1 2)                        --> "hello"    NOTE: this does not fail
                    (println "hello")
                    (1 2))

              (if (> 1 2)                        --> ERROR      NOTE: semantically invalid, it doesn't evaluate the second line
                    (println "hello")                                  the special form only evaluates the first line
                    (1 2))
|#


#|-----------------------------------------------------------------------------
;; User defined types with `struct`
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


;; define my own cons
(struct mycons (car cdr) #:transparent)

;; build and work with some of our lists ...

#|
Build on the notion of user defined type
- structures are kind of like classes, they are collections of attributes

;; define a `widget` type
(struct widget          ; type name     # struct is a special form, define a struct called widget
  (name purpose price)  ; attributes    # give it a list of symbol, each symbol corresponds to an attribute
  #:transparent)        ; when printing a widget, show its attributes     # this is a keyword

  - i'm creating a type called widget, every instance of widget will have a name, purpose, price
  - each attribute are dynamic

Creating an instance of the widget struct
(widget "foo" "foo-ing" 123) --> (widget "foo" "foo-ing" 123)

Creating an instance
(define w (widget "foo" "foo-ing" 123)) --> (widget "foo" "foo-ing" 123)
w --> (widget "foo" "foo-ing" 123)
(widget-name w) --> "foo"
(widget-purpose) --> "foo-ing"
(widget-price) --> 123
(widget? w) --> #t

struct is a keyword that is an easy way of creating a new type that we can use as a bucket of values

struct inheritance

define a `doohickey` type that is a sub-type of `widget`
(struct doohickey widget (special-power) #:transparent)

- doohickey is a subtype of widget
  - doohickey gives me an additional four attributes (special-power)

(define d1 (doohickey "thingamajig" "thinging" 199.99 "time travel"))
  - now if you create a doohickey, you need to provide it 4 attributes instead of 3 from widget



BREATH TAKING MOMENT WOWOW ----------------------------------------------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
If i wanted to, I can define my own cons
a cons cell is just a structure with two slots WOW

;; define my own cons
(struct mycons (car cdr) #:transparent)

(mycons 1 (mycons 2 (mycons 3 empty))) --> (mycons 1 (mycons 2 (mycons 3 '())))  MY OWN LINKED LIST WOW

(list (cons 1 (cons 2 '()))) --> '((1 2))

Cons cell are just a structure wtih two slots, nothing special whewwww ezzzz BABYYYYYYY

|#
