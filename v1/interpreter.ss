; top-level-eval evaluates a form in the global environment


;; our global environment starts out as the empty environment but can be expanded

(define global-env empty-env)

(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter
;; TODO: Add all grammar forms for initial implementation of eval-exp
;; NOTE: overridden later with syntax expander. Currently we evaluate everything as part of original grammar. Later, all exps will be converted to core form
(define eval-exp
  (lambda (exp env) 
    (cases expression exp
           [lit-exp (datum) (if (nqatom? datum)
                                datum
                                (cadr datum))]
           [var-exp (id)
                    (apply-env env id)]
           [lambda-exp (ids bodies)
                       (closure ids bodies env)]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args (eval-rands rands env)])
                      (apply-proc proc-value args))]
           [set!-exp (id exp)
                     (set!-ref
                      (apply-env-ref env id)
                      (eval-exp exp env))]
           [if-exp (test consequent alternative)
                   (if alternative
                       (if (eval-exp test env)
                           (eval-exp consequent env)
                           (eval-exp alternative env))
                       (if (eval-exp test env)
                           (eval-exp consequent env)))]
           [let-exp (name vars exps bodies variant)
                    (case variant
                      [(let)
                       (if name
                           (eopl:error 'eval-exp "Yet to be implemented")
                           (eval-bodies
                            bodies
                            (extend-env vars
                                        (eval-rands exps env)
                                        env)))]
                      [else (eopl:error 'eval-exp "Yet to be implemented")])]
           [while-exp (test bodies)
                      (if (eval-exp test env)
                          (begin
                            (for-each (lambda (x) (eval-exp x env)) bodies)
                            (eval-exp (while-exp test bodies) env)))]
           [unspecified-exp () (void)]
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
;; DONE: Add env field
(define eval-rands
  (lambda (rands env)
    (map
     (lambda (exp)
       (eval-exp exp env))
     rands)))


; evaluate the bodies in an environment expanding expression such as a lambda or a begin,
; returning the last

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin (eval-exp (car bodies) env)
               (eval-bodies (cdr bodies) env)))))


;  Apply a procedure to its arguments.
;; TODO: implement user-defined procedures evaluation
;; no need to pass in the environment here since only values are ever passed in
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
           [prim-proc
            (op) (apply-prim-proc op args)]
           [closure
            (ids bodies env)
            (eval-bodies
             bodies
             (closure-extend ids
                             args ;note that we do not evaluate the args as that was already done
                             env))]
           [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                   proc-value)])))

; Closure helper for variables ids

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

; helpers for closure-extend


(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not >= car cdr list null? eq?
                              equal? length list->vector list? pair? vector->list vector?
                              number? symbol? caar cadr cadar procedure? set-car! set-cdr!
                              apply map vector vector-ref > < <= vector-set! eqv? quotient
                              append list-tail))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [(/) (apply / args)]
      [(zero?) (apply zero? args)]
      [(not) (apply not args)]
      [(>=) (apply >= args)]
      [(>) (apply > args)]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(list) (apply list args)]
      [(null?) (apply null? args)]
      [(eq?) (eq? (1st args) (2nd args))]
      [(eqv?) (eqv? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(vector->list) (apply vector->list args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(caar) (apply caar args)]
      [(cadr) (apply cadr args)]
      [(cadar) (apply cadar args)]
      [(procedure?) (apply proc-val? args)]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(apply) (apply (lambda x (apply-proc (1st args) x)) (append (2nd args) (cddr args)))]
      [(map) (apply map (lambda x (apply-proc (1st args) x)) (cdr args))]
      [(vector) (apply vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(quotient) (apply quotient args)]
      [(append) (apply append args)]
      [(list-tail) (apply list-tail args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
