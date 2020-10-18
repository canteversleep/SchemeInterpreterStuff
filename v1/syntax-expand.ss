;; Syntax Expansion:
;; takes in parsed expression and converts non-core forms into core forms
;; for enhanced evaluation.
;; currently planned to include: cond case and or let and let*

(define syntax-expand
  (lambda (expr)
    (cases expression expr
           [let-exp (name vars exprs bodies let-var)]
           [and-exp (preds)]
           [or-exp (preds)]
           [case-exp (groups exprs)]
           [cond-exp (preds exprs)]
           [begin-exp (exps)]
           [else expr])))
