
(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not >= car cdr list null? eq?
                              equal? length list->vector list? pair? vector->list vector?
                              number? symbol? caar cadr cadar procedure? set-car! set-cdr!
                              apply map vector vector-ref > < <= vector-set! eqv? quotient
                              append list-tail assq call/cc exit-list))


;; our global environment starts out as the empty environment but can be expanded

(define global-env
  ((lambda ()
    (let ([nenv (global-env-record (cell '()) (cell '()))])
      (extend-global-env
       *prim-proc-names*
       (map prim-proc *prim-proc-names*)
       nenv)
      nenv))))

; top-level-eval evaluates a form in the global environment with an initial continuation

(define top-level-eval
  (lambda (form)
    (eval-exp form global-env (init-k))))

; eval-exp is the main component of the interpreter
;; DONE: Add all grammar forms for initial implementation of eval-exp


(define eval-exp
  (lambda (exp env k)
    (cases expression exp
           [lit-exp
            (datum)
            (apply-k k datum)]
           [var-exp
            (id)
            (apply-k k (apply-env env id))]
           [lambda-exp
            (ids bodies)
            (apply-k k (closure ids bodies env))]
           [app-exp
            (rator rands)
            (eval-exp rator env (rator-k rands env k))]
           [set!-exp
            (id exp)
            (eval-exp exp env (set!-k id env k))]
           [if-exp
            (test consequent alternative)
            (eval-exp test env
                      (if-k consequent alternative env k))]
           [while-exp
            (test bodies)
            (eval-exp test env (while-k test bodies env k))]
           [def-exp
             (id definition)
             (eval-exp definition env (define-k id k))]
             ;; (extend-global-env (list id) (list (eval-exp definition env k)) global-env)]
           [unspecified-exp () (void)]
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands for proc calls, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map/k
     (lambda (exp k)
       (eval-exp exp env k))
     rands k)))


; evaluate the bodies in an environment expanding expression such as a lambda or a begin,
; returning the last

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env k)
        (eval-exp (car bodies) env (bodies-k (cdr bodies) env k)))))


;  Apply a procedure to its arguments.
;; DONE: implement user-defined procedures evaluation
;; no need to pass in the environment here since only values are ever passed in

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
           [prim-proc
            (op) (apply-prim-proc op args k)]
           [closure
            (ids bodies env)
            (eval-bodies
             bodies
             (closure-extend ids
                             args ;note that we do not evaluate the args as that was already done
                             env)
             k)]
           [cont-proc ; only possibly invoked after an application of call/cc
            (k)
            (apply-k k (car args))]
           [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                   proc-value)])))

; environment extender that takes into account variable arity closures

(define closure-extend
  (lambda (ids args env)
    (cond
     [(symbol? ids) (extend-env (list ids) (list args) env)]
     [(null? ids) env]
     [((list-of symbol?) ids) (extend-env ids args env)]
     [(improper-safety ids)
      (let ([when-improper (proper-counter ids)])
        (extend-env
         (append (list-up-until ids when-improper) (list ((compose cdr when-improper) ids)))
         (append
          (list-up-until args when-improper)
          (list ((compose cdr when-improper) args)))
         env))])))



; we define primitive procedures that need not be considered core forms
; and must already be present in the language for the language to be
; operational. in most cases, the continuation here is applied since the
; operands have already been evaluated. the only exception to this rule is
; map, apply, and call/cc
; exit-list applies an initial continuation to its args to escape the
; comupation

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply/k + args k)]
      [(-) (apply/k - args k)]
      [(*) (apply/k * args k)]
      [(add1) (apply-k k (+ 1 (car args)))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply/k = args k)]
      [(/) (apply/k / args k)]
      [(zero?) (apply/k zero? args k)]
      [(not) (apply-k k (not (car args)))]
      [(>=) (apply/k >= args k)]
      [(>) (apply/k > args k)]
      [(<) (apply/k < args k)]
      [(<=) (apply/k <= args k)]
      [(car) (apply-k k (car (car args)))]
      [(cdr) (apply-k k (cdr (car args)))]
      [(list) (apply/k list args k)]
      [(null?) (apply-k k (null? (car args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(length) (apply-k k (length (car args)))]
      [(list->vector) (apply-k k (list->vector (car args)))]
      [(list?) (apply-k k (list? (car args)))]
      [(pair?) (apply-k k (pair? (car args)))]
      [(vector->list) (apply-k k (vector->list (car args)))]
      [(vector?) (apply-k k (vector? (car args)))]
      [(number?) (apply-k k (number? (car args)))]
      [(symbol?) (apply-k k (symbol? (car args)))]
      [(caar) (apply-k k (caar (car args)))]
      [(cadr) (apply-k k (cadr (car args)))]
      [(cadar) (apply-k k (cadar (car args)))]
      [(procedure?) (apply/k proc-val? args k)]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector) (apply/k vector args k)]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(quotient) (apply-k k (quotient (car args) (cadr args)))]
      [(append) (apply/k append args k)]
      [(list-tail) (apply/k list-tail args k)]
      [(assq) (apply/k assq args k)]
      [(apply) (apply (lambda (k . x) (apply-proc (1st args) x k)) k (append (2nd args) (cddr args)))]
      [(map) (map/k (lambda (x k) (apply-proc (1st args) (list x) k)) (cadr args) k)]
      [(exit-list) (apply-k (init-k) args)]
      [(call/cc) (apply-proc (car args) (list (cont-proc k)) k)]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s" 
            prim-op)])))


; not written in cps. do you know why?

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: implement closure printing pretty later
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
