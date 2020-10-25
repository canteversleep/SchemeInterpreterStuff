;This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)


;; We rely on the extended grammer defined with A10
;; <LcExp> ::= <var-exp> | <lambda-exp> | <app-exp> | <if-exp> | <set-exp> | <let-exp>
;; <var-exp> ::= identifier
;; <lambda-exp> ::= (lambda ({identifier}*) <LcExp>)
;; <app-exp> ::= (<LcExp>+);
;; <if-exp> ::= (if <LcExp> <LcExp> <LcExp>)
;; <set-exp> ::= (set! <LcExp> <LcExp>)
;; <let-exp> ::= (let ({(identifier <LcExp>)}*) {LcExp}*)


; NOTE: preds and resps are the map of car and cdr for cond, resp.
; TODO: implement cond let let* letrec or and syntax-expansion

(define parse-exp
  (lambda (datum)
    (cond
     [(symbol? datum)
      (if (eqv? 'else datum)
          'else
          (var-exp datum))]
     [(literal? datum) (lit-exp datum)]
     [(pair? datum)
      (if (list? datum)
          (cond
           [(eqv? (1st datum) 'lambda)
            (if (<= 3 (length datum))
                (letrec
                    ([formals-helper
                      (lambda (fs)
                        (cond
                         [(symbol? fs) fs]
                         [(null? fs) '()]
                         [(pair? fs)
                          (cond
                           [((list-of symbol?) fs) fs]
                           [(list? fs) (error-reporter 'lambda-formals-symbol fs)]
                           [(improper-safety fs) fs]
                           [else (error-reporter 'lambda-formals-symbol fs)])]
                         [else (error-reporter 'lambda-formals-symbol fs)]))])
                (let* ([bodies (cddr datum)]
                       [formals (2nd datum)]
                       [formals-final (formals-helper formals)])
                  (lambda-exp
                   formals-final
                   (map parse-exp bodies))))
                (error-reporter 'lambda-length datum))]
           [(eqv? (1st datum) 'set!)
            (if (= 3 (length datum))
                (set!-exp (if (symbol? (2nd datum))
                              (2nd datum)
                              (error-reporter 'set!-var-format datum))
                          (parse-exp (3rd datum)))
                (error-reporter 'set!-length datum))]
           [(eqv? (1st datum) 'if)
            (if (or (= 3 (length datum)) (= 4 (length datum)))
                (if-exp (parse-exp (2nd datum))
                        (parse-exp (3rd datum))
                        (if (= 3 (length datum))
                            #f
                            (parse-exp (4th datum))))
                (error-reporter 'if-length datum))]
           [(or (eqv? (1st datum) 'let) (eqv? (1st datum) 'let*) (eqv? (1st datum) 'letrec))
            (letrec*
                ([declare-safety
                  (lambda (bodies)
                    (if (list? bodies)
                        (or
                         (null? bodies)
                         (cond
                          [(not (andmap list? bodies))
                           (error-reporter 'let-declare-struct bodies)]
                          [(not (andmap (lambda (x) (= 2 (length x))) bodies))
                           (error-reporter 'let-declare-length bodies)]
                          [(not (andmap (lambda (x) (symbol? (1st x))) bodies))
                           (error-reporter 'let-declare-vars-invalid bodies)]
                          [else #t]))
                        (error-reporter 'let-declare-struct bodies)))]
                 [mutual-exclusion-check
                  (lambda (expr)
                    (if (or (eq? 'letrec (1st expr)) (eq? 'let* (1st expr)))
                        (error-reporter 'let-form expr)))]
                 [let-name-identifier-and-safety
                  (lambda (expr)
                    (let ([id (1st datum)])
                      (cond
                       [(symbol? (2nd expr))
                        (mutual-exclusion-check expr)
                        (declare-safety (3rd expr))
                        'named]
                       [else
                        (declare-safety (2nd expr))
                        (case id
                          [(let) 'let]
                          [(let*) 'let*]
                          [(letrec) 'letrec])])))])
              (if (<= 3 (length datum))
                  (let ([let-type (let-name-identifier-and-safety datum)])
                    (case let-type
                      [(named)
                       (let-exp
                        (2nd datum)
                        (map 1st (3rd datum))
                        (map parse-exp (map 2nd (3rd datum)))
                        (map parse-exp (cdddr datum))
                        'let)]
                      [(let let* letrec)
                       (let-exp
                        #f
                        (map 1st (2nd datum))
                        (map parse-exp (map 2nd (2nd datum)))
                        (map parse-exp (cddr datum))
                        let-type)]))
                  (error-reporter 'let-length datum)))]
           [(eqv? 'and (1st datum))
            (and-exp (map parse-exp (cdr datum)))]
           [(eqv? 'or (1st datum))
            (or-exp (map parse-exp (cdr datum)))]
           [(eqv? 'begin (1st datum))
            (begin-exp (map parse-exp (cdr datum)))]
           [(eqv? 'cond (1st datum))
            (let ([preds (map parse-exp (map car (cdr datum)))]
                  [resps (map (lambda (x) (map parse-exp x)) (map cdr (cdr datum)))])
              (cond-exp preds resps))]
           [(eqv? 'case (1st datum))
            (let ([groups (map (lambda (x) (if (eqv? 'else x)
                                               'else
                                               (map parse-exp x)))
                               (map car (cddr datum)))]
                  [exprs (map (lambda (x) (map parse-exp x)) (map cdr (cddr datum)))])
             (case-exp (parse-exp (2nd datum))
                       groups
                       exprs))]
           [(eqv? 'while (1st datum))
            (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
           [else (app-exp (parse-exp (1st datum))
                          (if (null? (cdr datum))
                              '()
                              (map parse-exp (cdr datum))))])
          (error-reporter 'improper-list-struct datum))]
     [else (error-reporter 'bad-expression datum)])))


(define (error-reporter id expr)
  (case id
    [(lambda-formals-symbol)
     (eopl:error 'parse-exp "lambda-expression: formal arguments ~s must all be symbols" expr)]
    [(lambda-length)
     (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" expr)]
    [(set!-var-format)
     (eopl:error 'parse-exp "set!-expression: variable in ~s must be a symbol" expr)]
    [(set!-length)
     (eopl:error 'parse-exp "set!-expression: ~s must contain both variable and expression only" expr)]
    [(if-length)
     (eopl:error 'parse-exp
                 "if-expression: ~s must contain test, consequent and possibly alternative only" expr)]
    [(let-length)
     (eopl:error 'parse-exp "let-expression: incorrect length ~s" expr)]
    [(let-declare-struct)
     (eopl:error 'parse-exp "let-expression: declarations ~s are invalid" expr)]
    [(let-declare-length)
     (eopl:error 'parse-exp "let-expression: declarations ~s are of incorrect lengths" expr)]
    [(let-declare-vars-invalid)
     (eopl:error 'parse-exp "let-expression: variables in declarations ~s are not all symbols" expr)]
    [(let-form)
     (eopl:error 'parse-exp "let-expression: ~s is of invalid form" expr)]
    [(improper-list-struct)
     (eopl:error 'parse-exp "expression: ~s must be a proper list" expr)]
    [(bad-expression)
     (eopl:error 'parse-exp "expression: ~s is of an unsupported variant or is bad" expr)]))
