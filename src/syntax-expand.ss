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
              [(let)
               (if name
                   (syntax-expand
                    (app-exp
                     (let-exp #f (list name) (list (lambda-exp vars bodies)) (list (var-exp name)) 'letrec)
                     exprs))
                   (app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand exprs)))]
              [(let*)
               (syntax-expand
                (cond
                [(null? vars) (let-exp #f '() '() bodies 'let)]
                [else
                 (let-exp
                  #f (list (car vars)) (list (car exprs))
                  (list (syntax-expand (let-exp #f (cdr vars) (cdr exprs) bodies 'let*))) 'let)]))]
              [(letrec)
               (syntax-expand
                (let-exp #f vars (make-list (length vars) (lit-exp #f))
                         (list (let-exp #f (make-temp-vars (length vars)) (map syntax-expand exprs)
                                  (append
                                   (map set!-exp vars (map parse-exp (make-temp-vars (length vars))))
                                   (list (let-exp #f '() '() bodies 'let))) 'let)) 'let))])]
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
             [else (syntax-expand
                    (let-exp #f '(a) (list (car preds))
                    (list (if-exp (var-exp 'a)
                                  (var-exp 'a)
                                  (syntax-expand (or-exp (cdr preds))))) 'let))])]
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
           [def-exp (id definition)
             (def-exp
               id
               (syntax-expand definition))]
           [unspecified-exp () (unspecified-exp)])))

;; ;; A bunch of helpers for the expander

(define make-temp-vars
  (lambda (n)
    (map string->symbol
         (map string-append
              (map string-append
                   (make-list n "::TEMP")
                   (map number->string (iota n)))
              (make-list n "::")))))



(map car (cddr
 '(case hell
 [(1 2 3 4) 'some]
 [(5 6 7 8) 'other]
 [else #f])))