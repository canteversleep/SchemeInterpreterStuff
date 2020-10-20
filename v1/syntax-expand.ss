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
                  (car vars) (car exprs)
                  (syntax-expand (let-exp #f (cdr vars) (cdr exprs) bodies 'let*)) 'let)]))])]
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
              (if (null? (exprs))
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
              ()
              (map syntax-expand exprs)))]
           [begin-exp
            (exps)
            (syntax-expand
             (let-exp #f '() '() (map syntax-expand bodies) 'let))]
           [else expr])))

;; ;; A bunch of helpers for the expander
