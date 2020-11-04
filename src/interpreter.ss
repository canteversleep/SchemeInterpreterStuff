; top-level-eval evaluates a form in the global environment

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not >= car cdr list null? eq?
                              equal? length list->vector list? pair? vector->list vector?
                              number? symbol? caar cadr cadar procedure? set-car! set-cdr!
                              apply map vector vector-ref > < <= vector-set! eqv? quotient
                              append list-tail assq))


;; our global environment starts out as the empty environment but can be expanded

(define global-env
  ((lambda ()
    (let ([nenv (cons (cell '()) (cell '()))])
      (extend-global-env
       *prim-proc-names*
       (map prim-proc *prim-proc-names*)
       nenv)
      nenv))))

(define top-level-eval
  (lambda (form)
    (eval-exp form global-env)))

; eval-exp is the main component of the interpreter
;; TODO: Add all grammar forms for initial implementation of eval-exp
;; NOTE: overridden later with syntax expander. Currently we evaluate everything as part of original grammar. Later, all exps will be converted to core form
(define eval-exp
  (lambda (exp env)
    (cases expression exp
           [lit-exp
            (datum)
            (if (nqatom? datum)
                datum
                (cadr datum))]
           [bound-var-exp
            (depth index)
            (apply-env env `(: ,depth index))]
           [free-var-exp
            (id)
            (apply-env env `(: free ,id))]
           [lex-lambda-exp
            (bodies proper)
            (closure bodies env proper)]
           [app-exp
            (rator rands)
            (let ([proc-value (eval-exp rator env)]
                  [args (eval-rands rands env)])
              (apply-proc proc-value args))]
           [lex-set!-exp
            (addr exp)
            (set!-ref
             (apply-env-ref env
                            (cases expression id
                                   [free-var-exp (id)
                                                 `(: free ,id)]
                                   [bound-var-exp (depth index)
                                                  `(: ,depth ,index)]))
             (eval-exp exp env))]
           [if-exp
            (test consequent alternative)
            (if alternative
                (if (eval-exp test env)
                    (eval-exp consequent env)
                    (eval-exp alternative env))
                (if (eval-exp test env)
                    (eval-exp consequent env)))]
           [while-exp
            (test bodies)
            (if (eval-exp test env)
                (begin
                  (for-each (lambda (x) (eval-exp x env)) bodies)
                  (eval-exp (while-exp test bodies) env)))]
           [def-exp
             (id definition)
             (extend-global-env (list id) (list (eval-exp definition env)) global-env)]
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
            (bodies env proper)
            (eval-bodies
             bodies
             (extend-env
              (cond
               [(and (zero? proper) (not (null? args)))
                (list args)]
               [(zero? proper) '()]
               [else
                (let ([proper-args (list-up-until args proper)]
                      [improper-args ((compose cdr proper) args)])
                  (if (null? improper-args)
                      proper-args
                      (append proper-args (list improper-args))))])
              env))]
           [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                   proc-value)])))

; Closure helper for variables ids
; not used for lexical branch
;; (define closure-extend
;;   (lambda (ids args env)
;;     (cond
;;      [(symbol? ids) (extend-env (list ids) (list args) env)]
;;      [(null? ids) env]
;;      [((list-of symbol?) ids) (extend-env ids args env)]
;;      [(improper-safety ids)
;;       (let ([when-improper (proper-counter ids)])
;;         (extend-env
;;          (append (list-up-until ids when-improper) (list ((compose cdr when-improper) ids)))
;;          (append
;;           (list-up-until args when-improper)
;;           (list ((compose cdr when-improper) args)))
;;          env))])))

; helpers for closure-extend


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
      [(assq) (apply assq args)]
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
