;; Syntax Expansion:
;; takes in parsed expression and converts non-core forms into core forms
;; for enhanced evaluation.
;; currently planned to include: cond case and or let and let*



(define syntax-expand
  (lambda (expr)
    (cases expression expr
           [let-exp
            (name vars exprs bodies let-var)
            (case let-var
              [(let) (app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand exprs))]
              [(let*)
               (syntax-expand
                (cond
                [(null? vars) (let-exp #f '() '() bodies 'let)]
                [else
                 (let-exp
                  #f (list (car vars)) (list (car exprs))
                  (list (syntax-expand (let-exp #f (cdr vars) (cdr exprs) bodies 'let*))) 'let)]))]
              [(letrec)])]
           [and-exp
            (preds)
            (cond
             [(null? preds) (lit-exp #t)]
             [(null? (cdr preds)) (syntax-expand (car preds))]
             [else (if-exp (syntax-expand (car preds))
                           (syntax-expand (and-exp (cdr preds)))
                           (lit-exp #f))])]
           [or-exp
            (preds)
            (cond
             [(null? preds) (lit-exp #f)]
             [(null? (cdr preds)) (syntax-expand (car preds))]
             [else (if-exp (syntax-expand (car preds))
                           (syntax-expand (car preds))
                           (syntax-expand (or-exp (cdr preds))))])]
           [cond-exp
            (preds exprs)
            (cond
             [(null? preds)
              (if (null? exprs)
                  (unspecified-exp)
                  (syntax-expand (begin-exp (car exprs))))]
             [(eq? 'else (car preds))
              (syntax-expand (begin-exp (car exprs)))]
             [else
              (syntax-expand
               (if-exp (car preds)
                       (begin-exp (car exprs))
                       (cond-exp (cdr preds) (cdr exprs))))])]
           [case-exp
            (key groups exprs)
            (syntax-expand
             (cond-exp
              (map
               (lambda (x)
                 (if (eqv? x 'else)
                     'else
                     (or-exp
                      (map (lambda (x) (app-exp (var-exp 'eqv?)  (list key x))) x)))) groups)
              exprs))]
           [begin-exp
            (exprs)
            (syntax-expand
             (let-exp #f '() '() exprs 'let))]
           [while-exp
            (test bodies)
            (while-exp test bodies)]
           [var-exp (id) (var-exp id)]
           [lit-exp (val) (lit-exp val)]
           [lambda-exp (formals bodies) (lambda-exp formals (map syntax-expand bodies))]
           [app-exp (rator rands) (app-exp (syntax-expand rator) (map syntax-expand rands))]
           [set!-exp (id val-exp) (set!-exp id (syntax-expand val-exp))]
           [if-exp (test consequent alternative)
                   (if-exp (syntax-expand test)
                           (syntax-expand consequent)
                           (if alternative (syntax-expand alternative) #f))]
           [unspecified-exp () (unspecified-exp)])))

;; ;; A bunch of helpers for the expander





(map car (cddr
 '(case hell
 [(1 2 3 4) 'some]
 [(5 6 7 8) 'other]
 [else #f])))
