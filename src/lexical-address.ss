;; A lexical addresser that takes in expanded syntax expressions produces expressions with
;; lexical addresses, with the purpose of making environmental lookups more efficient.


(define lexical-address
  (lambda (exp)
    (lexer exp (empty-senv))))

(define lexer
  (lambda (exp senv)
    (cases expression exp
           [var-exp (id)
                    (apply-senv senv id)]
           [lit-exp (val)
                    (lit-exp val)]
           [lambda-exp (formals bodies)
                       (cond
                        [(null? formals)
                         (lex-lambda-exp (map (lambda (x) (lexer x (extend-senv formals senv))) bodies) 0)]
                        [(symbol? formals)
                         (lex-lambda-exp (map (lambda (x) (lexer x (extend-senv (list formals) senv))) bodies) 0)]
                        [((list-of symbol?) formals)
                         (lex-lambda-exp (map (lambda (x) (lexer x (extend-senv formals senv))) bodies) (length formals))]
                        [(improper-safety formals)
                         (let ([when-improper (proper-counter formals)])
                           (lex-lambda-exp
                            (map (lambda (x)
                                   (extend-senv
                                    x
                                    (append
                                     (list-up-until formals when-improper)
                                     (list ((compose cdr when-improper) formals)))
                                    senv)) bodies)
                            when-improper))])]
           [app-exp (rator rands)
                    (app-exp (lexer rator senv) (map (lambda (x) (lexer x senv)) rands))]
           [def-exp (id def)
                    (def-exp id (lexer def senv))]
           [set!-exp (id val)
                     (lex-set!-exp (apply-senv senv id) (lexer val senv))]
           [if-exp (test consequent alternative)
                   (if-exp
                    (lexer test senv)
                    (lexer consequent senv)
                    (if alternative (lexer alternative senv) #f))]
           [while-exp (test bodies)
                      (while-exp (lexer test senv)
                                 (map (lambda (x) (lexer x senv)) bodies))]
           [else (eopl:error 'lexer "Bad input: ~s" exp)])))
