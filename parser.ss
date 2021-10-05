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
    [set!-exp (id symbol?) (val-exp expression?)])

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (parse-exp datum)
    (cond 
        [(lit-type? datum) (lit-exp datum)]
        [(pair? datum)
            (cond 
                [(eqv? (car datum) 'lambda) ; checking lambda
                    (cond 
                        [(> 3 (length datum)) (eopl:error 'lambda-exp "length of lambda expression is too short: ~s" datum)]
                        [(list? (2nd datum)) 
                            (if ((list-of symbol?) (2nd datum))
                                (lambda-exp (map parse-exp (2nd datum)) (parse-exp (3rd datum)))
                                (eopl:error 'lambda-exp "2nd in lambda expression is not a list of symbols: ~s" datum))]
                        [else eopl:error 'lambda-exp "lambda expressions is in incorrect form: ~s" datum])]
                [((list-of symbol?) datum) (map var-exp datum)


















                     (lambda-exp (car (2nd  datum)) (parse-exp (3rd datum))))]
                    [else (app-exp (parse-exp (1st datum)) (parse-exp (2nd datum)))])]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))