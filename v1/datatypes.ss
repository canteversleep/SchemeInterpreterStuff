
;; Parsed expression datatype.

; some helpers

(define (literal? x)
  (or (nqatom? x) (quoted? x)))

(define (quoted? x)
  (eq? (car x) 'quote))

(define (nqatom? x)
  (and (atom? x) (not (list? x))))

(define (optional? x)
  (or (eq? #f x) (symbol? x)))

(define (lit-pred? x)
  (or (eq? #f x) (eq? #t x)))


(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (val literal?)]
  [lambda-exp
   (formals
    (lambda (x)
      (or (null? x) (symbol? x) ((list-of symbol?) x)
          (and ((list-of symbol?) x) (symbol? (cdr x))))))
   (bodies (list-of expression?))]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))]
  [set!-exp
   (id symbol?)
   (val-exp expression?)]
  [if-exp
   (test expression?)
   (consequent expression?)
   (alternative (lambda (x) (or (eq? x #f) (expression? x))))]
  [let-exp
   (name optional?)
   (vars (list-of symbol?))
   (exprs (list-of expression?))
   (bodies (list-of expression?))
   (let-variant symbol?)])

;; environment type definitions
;; TODO: implement env based on different chosen representation later.
;; im not a big fan of this representation

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (ids scheme-value?)
   (bodies (list-of expression?))
   (current-env environment?)])
