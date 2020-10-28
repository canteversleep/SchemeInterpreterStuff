
;; Parsed expression datatype.

; some helpers

; conses a list up until the nth reference

(define list-up-until
  (lambda (ls n)
    (cond
     [(zero? n) '()]
     [else (cons (car ls) (list-up-until (cdr ls) (sub1 n)))])))

; composes the same procedure n-times

(define compose
  (lambda (proc n)
    (cond
     [(zero? n) (lambda (x) x)]
     [else (lambda (x) ((compose proc (sub1 n)) (proc x)))])))

; finds the length of the list component that can be made proper
; usage: (proper-counter '(x y . z) => 2)
; assumptions: all elements of the list are symbols

(define proper-counter
  (lambda (ls)
    (let recr ([counter 0] [ls ls])
      (if (symbol? ls)
          counter
          (recr (add1 counter) (cdr ls))))))

; guarantees that all elements of an improper list are symbols
; assumptions: non-empty improper list of the form (x1 x2 x3 ... . xn)

(define improper-safety
  (lambda (ls)
    (cond
     [(symbol? ls) #t]
     [(pair? ls)
        (and (symbol? (car ls))
             (improper-safety (cdr ls)))]
     [else #f])))


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

; TODO: check case-exp type

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (val literal?)]
  [lambda-exp
   (formals
    (lambda (x)
      (or (null? x) (symbol? x) ((list-of symbol?) x)
          (improper-safety x))))
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
   (let-variant symbol?)]
  [cond-exp
   (preds (list-of (lambda (x) (or (eqv? 'else x) (expression? x)))))
   (resps (list-of (list-of expression?)))]
  [case-exp
   (key expression?)
   (groups (lambda (x) (andmap (lambda (x) (or ((list-of expression?) x) (eqv? 'else x))) x)))
   (exprs (lambda (x) (map (list-of expression?) x)))]
  [or-exp
   (preds (list-of expression?))]
  [and-exp
   (preds (list-of expression?))]
  [begin-exp
   (exps (list-of expression?))]
  [while-exp
   (test expression?)
   (bodies (list-of expression?))]
  [for-exp
   (var symbol?)
   (beginning expression?)
   (end expression?)
   (doing expression?)
   (upordown (lambda (v) (or (eq? v 'to) (eq? v 'downto))))]
  [unspecified-exp])


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

; cell ADT
; DONE: fix

;; (define cell
;;   (lambda (v)
;;     (cons v '::CELLMARK::)))

;; (define cell-ref car)
;; (define deref cell-ref)

;; (define cell-set! set-car!)
;; (define set-ref! cell-set!)

;; (define cell?
;;   (lambda (pc)
;;     (and (pair? pc) (eq? (cdr pc) '::CELLMARK::))))

(define cell box)
(define cell? box?)
(define cell-ref unbox)
(define cell-set! set-box!)

(define deref cell-ref)
(define set!-ref cell-set!)
