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
              [(let) (app-exp (lambda-exp vars (syntax-expand bodies)) (syntax-expand exprs))]
              [(let*)
               (syntax-expand
                (cond
                [(null? vars) (let-exp #f '() '() bodies 'let)]
                [else
                 (let-exp
                  (car vars) (car exprs)
                  (syntax-expand (let-exp #f (cdr vars) (cdr exprs) bodies 'let*)) 'let)]))])]
           [and-exp (preds)]
           [or-exp (preds)]
           [case-exp (groups exprs)]
           [cond-exp (preds exprs)]
           [begin-exp (exps)]
           [else expr])))

;; A bunch of helpers for the expander

(define let*->let
  (lambda ()))
