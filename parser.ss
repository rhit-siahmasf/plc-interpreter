(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define (lit-type? l)
    (or (null? l) (boolean? l) (string? l) (number? l) (vector? l) (symbol? l) (equal? 'quote (car l))))


; lambda-exp does not check for list of symbols, just list
(define-datatype expression expression?
    [var-exp (id symbol?)]
    [lit-exp (id lit-type?)]
    [lambda-exp (id list?) (body expression?)]
    [app-exp (rator expression?) (rands (list-of expression?))]
    [set!-exp (id symbol?) (val-exp expression?)]
    [if-exp (condition expression?) (body expression?)]
    [if-else-exp (condition expression?) (true expression?) (elsef expression?)]
    [let-exp (defined (list-of list?)) (body expression?)] ; might be wrong
    
    )

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define (parse-exp datum)
    (cond 
        [(lit-type? datum) (lit-exp datum)]
        [(pair? datum)
            (cond 
                [(eqv? (1st datum) 'lambda) ; checking lambda
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of lambda expression is too short: ~s" datum)]
                        [(list? (2nd datum)) 
                            (if ((list-of symbol?) (2nd datum))
                                (lambda-exp (map parse-exp (2nd datum)) (parse-exp (3rd datum))) ; parses args and exp
                                (eopl:error 'parse-exp "2nd in lambda expression is not a list of symbols: ~s" datum))]
                        [else eopl:error 'parse-exp "lambda expressions is in incorrect form: ~s" datum]
                    )
                ]
                [(eqv? (1st datum) 'let)  ; let
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of let expression is too short: ~s" datum)]
                        [(not (list? (2nd datum)))  (eopl:error 'parse-exp "id not list: ~s" datum)]
                        ; the id's for let should be of the form  ((a 1) (b 3) etc etc)
                        [(andmap (lambda (x) (list? x)) (2nd datum)) ; make sure they are lists
                            (cond 
                                [(not (andmap (lambda (x) (equal? 2 (length x))) (2nd datum))) ; list must be of length 2 
                                    (eopl:error 'parse-exp "not all length 2" datum)]
                                [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))      ;first is symbol
                                     (eopl:error 'parse-exp "first element not a symbol/expression" datum)]
                                [(not (or (andmap (lambda (x) (number? (cadr x))) (2nd datum))
                                        (andmap (lambda (x) (expression? (cadr x))) (2nd datum))
                                        ))     ;second is number
                                     (eopl:error 'parse-exp "second element not a number/expression" datum)] 
                                [else (let-exp (map parse-exp (2nd datum)) (parse-exp (3rd datum)))]
                            )]
                        
                        [else (eopl:error 'parse-exp "identifiers are not lists" datum)]
                    )
                ]

                ; note to daughter of pilf. i didnt know how let and let* are different in terms of parsing. I also don't know if we store 
                ; identifiers of the let (the second thing in brackets) as a list of lists or what. which is why i have left them all as let.

                [(eqv? (1st datum) 'let*)  ; let*
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of let expression is too short: ~s" datum)]
                         [(not (list? (2nd datum)))  (eopl:error 'parse-exp "id not list: ~s" datum)]
                        ; the id's for let should be of the form  ((a 1) (b 3) etc etc)
                        [(andmap (lambda (x) (list? x)) (2nd datum)) ; make sure they are lists
                            (cond 
                                [(not (andmap (lambda (x) (equal? 2 (length x))) (2nd datum))) ; list must be of length 2 
                                    (eopl:error 'parse-exp "not all length 2" datum)]
                                [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))      ;first is symbol
                                     (eopl:error 'parse-exp "first element not a symbol/expression" datum)]
                                [(not (or (andmap (lambda (x) (number? (cadr x))) (2nd datum))
                                        (andmap (lambda (x) (expression? (cadr x))) (2nd datum))
                                        ))     ;second is number
                                     (eopl:error 'parse-exp "second element not a number/expression" datum)] 
                                [else (let-exp (map parse-exp (2nd datum)) (parse-exp (3rd datum)))]
                            )]
                        
                        [else (eopl:error 'parse-exp "identifiers are not lists" datum)]
                    )
                ]


                 [(eqv? (1st datum) 'letrec)  ; letrec
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of let expression is too short: ~s" datum)]
                         [(not (list? (2nd datum)))  (eopl:error 'parse-exp "id not list: ~s" datum)]
                        ; the id's for let should be of the form  ((a 1) (b 3) etc etc)
                        [(andmap (lambda (x) (list? x)) (2nd datum)) ; make sure they are lists
                            (cond 
                                [(not (andmap (lambda (x) (equal? 2 (length x))) (2nd datum))) ; list must be of length 2 
                                    (eopl:error 'parse-exp "not all length 2" datum)]
                                [(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))      ;first is symbol
                                     (eopl:error 'parse-exp "first element not a symbol/expression" datum)]
                                [(not (or (andmap (lambda (x) (number? (cadr x))) (2nd datum))
                                        (andmap (lambda (x) (expression? (cadr x))) (2nd datum))
                                        ))     ;second is number
                                     (eopl:error 'parse-exp "second element not a number/expression" datum)] 
                                [else (let-exp (map parse-exp (2nd datum)) (parse-exp (3rd datum)))]
                            )]
                        
                        [else (eopl:error 'parse-exp "identifiers are not lists" datum)]
                    )
                ]

                [(eqv? (1st datum) 'set!)            ; check set 
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of set expression is too short: ~s" datum)]
                        [(and (symbol? (2nd datum)) (expression? (3rd datum)))
                            (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))  
                        ]
                        [else (eopl:error 'parse-exp "wrong set! format" datum)]
                    )
                ]


                [(eqv? (1st datum) 'if) ; if statement
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'parse-exp "length of if expression is too short: ~s" datum)] 
                        [(< 4 (length datum)) (eopl:error 'parse-exp "length of if expression is too long: ~s" datum)] ; idk bout this
                        [(equal? 4 (length datum))  ; else
                            (cond 
                                [(and (expression? (2nd datum)) (expression? (3rd datum)) (expression? (4th datum)))
                                    (if-else-exp (expression? (2nd datum))(expression? (3rd datum)) (expression? (4th datum)))]                        
                                [else (eopl:error 'parse-exp "wrong if-else format: ~s" datum) ]
                            )
                        ]
                        [(and (expression? (2nd datum)) (expression? (3rd datum))) 
                                (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))
                        ]
                        [else (eopl:error 'parse-exp "wrong if format: ~s" datum)]
                    )            
                ]
            )   
        ]         
        [(app-exp? datum) (app-exp (parse-exp (1st datum)) (parse-exp (2nd datum)))]
        [(list? datum) (map parse-exp datum)]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))


;  [lambda-exp (id list?) (body expression?)]
   
    (define unparse-exp
        (lambda (pexp)
            (cases expression pexp
                [lambda-exp (id body) (list 'lambda id body)]
                [let-exp (defined body) (list 'let defined body) ]            
            )
        )
    )