#lang racket

#|-----------------------------------------------------------------------------
;; Function definitions

- `lambda`: creates an anonymous function
- `define` supports a special syntax for binding variables to functions

-----------------------------------------------------------------------------|#

#|
functions are the primary executables modules in Racket
- primary modules will be functions
- define a bunch of functions
- higher order functions (functions that take other functions)

(lambda (x) x) --> #<procedure>        NOTE: you created a procedure
- lamba just represents the creation of an anonomous function creation
- when creating a lambda function, you just create a keyword (lambda) followed my a list of parameters (x) 
- im creating a function that takes a single parameter x and evaluates to that very same x

((lambda (x) x) 10) --> 10
- if I wan to call the function, it must go in the first position of the list
- hence it is evaluated as a function
- also the 10) represents the argument that is passed into the function

((lambda (x) x) (+ 10 10)) --> 20
- evaluate argument first then pass result to the lambda

((lambda (x) (+ x 20)) (* 2 5)) --> 30

the lambda is the way in which functions are defined in racket fundamentally

creating a function:
(define fn (lambda (x) (+ x 20))) 
fn --> #<procedure:fn>

(fn 100) --> 120

(define abc (lambda (x y z) (+ x (* y z))))
(abc 1 2 3) --> 7


lambda in-depth

(define f2 (lambda (x y z)
             (println x)
             (println y)
             (println z)
             (* x (+ y z))))

- a lambda can have multiple s-expression in its body
  - it only makes sense if the intervening expressions have a side effect
  - the last expression is technically always the return value of the lambda

________________________________________

(f2 2 3 5) --> 16

(print (void)) --> #<void>
- void by default is what you get when you call a function that performs some side effect



short cut instead of writing lambda: NOTE: this is also how we indent it
(define (f3 x)
 (add1 x))

- define is a special form that has a special syntax
  - if the second thing that you giver the define form is a list,
  - the first thing from the list is taken to be the name of the function,
  - then the next are the parameters of the function

- so, f3 is the function name
- x is the parameter of the function

ex. another example of indentation and special syntax of define
(define (f2 (x y z)
    (println x)
    (println y)
    (println z)
    (* x (+ y z)))

simplified bit of syntax is just prettier, we call it syntactic sugar



|#



(define f1 (lambda (x) (+ x 1)))

(define f2 (lambda (x y z)
             (println x)
             (println y)
             (println z)
             (* x (+ y z))))

(define (f3 x)
 (add1 x))

(define (f4 x y z)
  (println x)
  (println y)
  (println z)
  (* x (+ y z)))


;; `values` and `let-values` can be used to return/retrieve multiple values
;; from/to a function call.
;; return two values from the function
;; values is a way of packaging multiple values together into a return value from a function
;; now need a way of unpacking it, --> continue to next function
(define (quad-roots a b c)
  (let* ([disc (- (* b b) (* 4 a c))]
         [sqr-disc (sqrt disc)])
    (values (/ (+ (- b) sqr-disc) (* 2 a))
            (/ (- (- b) sqr-disc) (* 2 a)))))


;; if we want them to be assigned to local variable names so we can use them
;; we have another type of special form called let-values
;; let-values maps a list of symbols onto the result that returns multiple values

(define (test-quad-roots r1 r2)
  (let ([a 1] [b (- (+ r1 r2))] [c (* r1 r2)]) ; based on (x + r1)(x + r2)
    (let-values ([(rr1 rr2) (quad-roots a b c)])
      (println rr1)
      (println rr2))))


;; "rest" arguments

;; the . (dot) notation recap

;; ex. '(1 2 3 4) == (cons 1 (cons 2 (cons 3 (cons 4 empty))))
;; '(1 2 . 3) == (cons 1 (cons 2 3)), the 3 represents the rest of the list after you pluck the 2 out of it

;; when you write a defined form with a . (dot), you have x and y, and z, z represents the rest of the list after y
;; z is actually a list that conatians any arguments that follows x y
;; (f5 1 2 3) -->
#;                  1
#;                  2
#;                '(3)

;; the 3 is indicated a list of just the value 3

;; another ex. (f5 1 2 3 4 5 6 7) -->
#;                1
#;                2
#;               '(3 4 5 6 7)
;; another way of passing arbitrary number of arugments
(define (f5 x y . z) ; `z` is a list of any & all args after `x` and `y`
  (println x)
  (println y)
  (println z))


;; just another example here
;; (f6 1 2 3 4) --> 4   (4 arguments)
(define (f6 . rest)
  (length rest))


;; lambda expressions also support rest arguments
;; its just the representation not in syntactic sugar
;; args is just the the keyword in lambda if arguments are not in parenthesis, then the arguments are assigned to args
;; ex. (f7 1 2 3 4 5) --> 5  (5 arguments)
;; when you see a lambda without a list of its parameters, then we take the entire thing
;; ((lamda x x) 10 20) --> '(10 20)
;; Since x is not in parentheses, it is treated as a rest parameter, meaning it will collect all arguments into a list.
(define f7
  (lambda args ; `args` is a list of all the arguments
    (length args)))

;; (f8 1 2 3 4 5) --> 1
;;                    '(2 3 4 5)
(define f8
  (lambda (x . rest)
    (println x)
    (println rest)))


#|-----------------------------------------------------------------------------
;; Some more special forms

- `if`: if-then-else
- `begin`: sequences multiple sexps; evaluates to the result of the last
- `when`: if-then
- `cond`: multi-way conditional
- `case`: dispatch
- `match`: pattern match
- `set!`: mutation!
-----------------------------------------------------------------------------|#

#|

~a: Inserts the argument (name) as a string.
~: Insert New Line \n in other languages

in racket 

|#


(define (say-hi-1 name)
  (if (equal? name "Jane")
      (println "Me Tarzan!")
      (printf "Hello, ~a~n" name))) ; `printf` does interpolation/formatting


;; digression: equality testing
(list
 (list
  ;; `=` for numbers
  (= 2 2)                         ;; --> #t
  (= 2 2.0)                       ;; --> #t
  (= 2 2.01)                      ;; --> #f
  (= 2 2.0000000000000000001)     ;; --> #t
  (= 2 8/4))                      ;; --> #t

 (list
  ;; `eq?` for pointer comparison
  ;; we use eq? to see if two things are identical objects in memory
  ;; a symbol in racket we say (fancy way), are interned (internally tracked)
  ;; if racket sees the same symbol again, it refers to the same object
  ;; (#t #t #f #t)
  ;; (eq? '(a b c) '(a b c)) --> #f because, when racket encounters this, its a sequence of con cells, they 
  ;; are completely separate in memory, so it's not the same object in memory
  (eq? 'a 'a)
  (eq? "hello world" "hello world")
  (eq? '(a b c) '(a b c))             
  (let ([lst '(a b c)])      ;; same object in memory yayyyy
    (eq? lst lst)))

;; test for value comparison, if you have a list, it goes into the list to check if the list elements are identical
 (list
  ;; `equal?` for value comparison
  (equal? "hello world" "hello world") ;; #t
  (equal? '(a b c) '(a b c))))         ;; #t
 


;; _________________________________________________________________________________________________________________________________________



;; use `string-ref` to get a char by index in a string
;; string-ref, get a character by index
;;(string-ref "hello" 1) --> #\e
;; check to see whether the first character of a name is equal to the letter J
;; if so, I want the if special form to have multiple lines of code for the if-clause
;; enclose it with a beginning block, begin is like curly braces, begin creates a block
;; of code that encompasses the two print lines
;; ex. (say-hi-2 "tony") --> Hello, tony
;; ex. (say-hi-2 "John") -->
;;                            "Me Tarzan!"
;;                             You John!

(define (say-hi-2 name)
  (if (equal? (string-ref name 0) #\J)
      ;; `begin` for multi-expression "blocks"
      (begin (println "Me Tarzan!")
             (printf "You ~a!~n" name))
      (printf "Hello, ~a~n" name)))


;; `when` is an `else`-less `if`, with an automatic `begin`
;; use this if you don't need the else component
;; (when #t 1) --> 1
;; (print (when #f 1)) --> #<void>
;; ex. (say-hi-3 "Januke") -->
;;                              "Me Tarzan!"
;;                               You Januke!
(define (say-hi-3 name)
  (when (regexp-match? #rx"^Jan.*" name) ; regular exp matching!
    (println "Me Tarzan!")
    (printf "You ~a!~n" name)))


;; Cond is a special form that is baked into the language
;; each branch has two components, a test and a result expression
;; regexp-match? --> is a function that checks if a string matches a given regular expression (regex)
;; #rx"^Jan.*"  --> this is a regular expression
;;        - bascially it's matching if the letters of Jan is the beginning of the string and anything after that
;; The #rx prefix in Racket indicates that the following string is a regular expression.
;; The caret ^ means "the match must start at the beginning of the string."
;; "Jan" simply means the string must contain the exact characters "J", "a", and "n" in order.
;; (dot) → Matches any single character (except a newline).
;; (asterisk) → Matches zero or more occurrences of the preceding character.
;; .* "Match any number of any characters after 'Jan'", including an empty string.
(define (say-hi-4 name)
  ;; `cond` is a multi-way branch
  (cond [(equal? name "Jane")
         (println "Me Tarzan!")]
        [(regexp-match? #rx"^Jan.*" name)
         (printf "You ~a?~n" name)]
        [else
         (printf "Hello, ~a~n" name)]))

;; variation of cond exist
(define (say-hi-5 name)
  ;; `case` checks a value against multiple options using `equal?`
  (case (char-downcase (string-ref name 0))
    [(#\j) (println "Me Tarzan!")]
    [(#\a #\e #\i #\o #\u) (println "Ewoh!")]
    [else (printf "Hello, ~a~n" name)]))


(define (say-hi-6 name)
  ;; `match` is a general purpose pattern matcher with its own special language
  ;; pattern matching is something we will use a lot when writing an interpretor
  ;; you give it a value to pass in,
  ;; generally passing a string in this case
  ;; matching the string against one of these (11) cases
  ;; pattern matching mechanism is not a boolean test
  ;; every case starts with a pattern
  
  ;; (? string?) this pattern run this test, to see if the pattern is true for that test
  ;;    as long as i give it a string, it will return true, (say-hi-6 "Tony") -- > Hello, Tony

  ;; 


  (match name
    ["Jane" (println "Me Tarzan!")]
    [(regexp #rx"^Jan.*") (println "Me Tarzan?")]
    [(? string?) (printf "Hello, ~a~n" name)]                         ;; (say-hi-6 "Tony") -- > Hello, Tony
    [(list x y) (printf "Hello, ~a and ~a~n" x y)]                    ;; (say-hi-6 '("hello" 10)) --> Hello, hello and 10
    [(list "Jane") (println "Hello, Jane-in-a-list")]                 ;; (say-hi-6 '("Jane")) --> "Hello, Jane-in-a-list"
    [(list "Jane" x "Jane") (printf "Hello, ~a-between-Janes~n" x)]   ;; (say-hi-6 '("Jane" 100 "Jane")) --> Hello, 100-between-Janes
    [(list x y x) (printf "Hello, ~a-between-~as~n" y x)]             ;; (say-hi-6 '(1 100 1)) --> Hello, 100-between-1s
    [(list _ _ _ "Jane") (println "Hi, Jane in 4th place")]           ;; (say-hi-6 '(1 2 3 "Jane")) --> "Hi, Jane in 4th place"
    [(list x ...) (printf "Hi, lots of ~as~n" x)]                     ;; (say-hi-6 '(1 1 1 1)) --> Hi, lots of (1 1 1 1)s
    [(or 'Jane '(Jane)) (println "Hi, Jane in hiding")]               ;; 
    [_ (println "Stranger, Danger!")]))                               ;; 

;; println (use when i am just printing a string that wants to be on a line by itself)
;; printf for string interpolation
;; you see quotes in this interpretor because we are at this level
;; in a normal program, you wouldnt see the quotes of the strings


(define (say-hi-7 name)
  (let ([greetings '("Hello" "Hola" "你好")]
        [index 0])
    ;; what are we returning? what is going on?!
    (lambda ()
      (printf "~a, ~a~n" 
              (list-ref greetings index) name)
      (set! index (remainder (add1 index) 
                              (length greetings))))))

;; building two local variables
;; building a let around a lambda
;; (say-hi-7 "Jane") --> #<procedure:...es/02-functions.rkt:296:4>   (this function returns a function)
;; this procedure takes no arguments (zero arguments) because of this ()
;; the variables are irrelevant because the lambda is closed off
;; local variables are stack bound

#|
- explanantion of this function:
- because say-hi-7 returns a function --> #<procedure:...es/02-functions.rkt:296:4>

    (lambda ()
      (printf "~a, ~a~n" 
              (list-ref greetings index) name)
      (set! index (remainder (add1 index) 
                              (length greetings))))))


- this part of the function outlives the scope of this let form, these local variables are stored,
- they are not deallocated, CLOSURE !!! is the keyword
- this lambda closes over or keeps in memory these local variables 

play around with this

(define c (say-hi-7 "Jane")) --> #<procedure:...es/02-functions.rkt:296:4>

each time you run (c) separately:
(c) --> Hello, Jane
(c) --> Hola, Jane
(c) --> 你好, Jane

the point is, that the function that is returned can continue to reference this:
  (let ([greetings '("Hello" "Hola" "你好")]
        [index 0])

the index is used to keep track of the position of the list
this list keeps track of the strings to pre-fix to input the name with

This idea that we havea function that is returned that closes over values in its lexical scope
in the scope it was created is an important idea

closure is an important and significant addition to funactional langauge in general
make sure our interpretors can accomodate and implement

Closure is a special type of function, we will learn later HEHE


|#