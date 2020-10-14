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
           ;[lambda]
           [app-exp (rator rands)
                    (let ([proc-value (eval-exp rator env)]
                          [args (eval-rands rands env)])
                      (apply-proc proc-value args))]
           ;[set!]
           ;[if]
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
           [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list
;; TODO: Add env field
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
               (eval-bodies (cdr bodies) env)))


;  Apply a procedure to its arguments.
;; TODO: implement user-defined procedures evaluation
;; no need to pass in the environment here since only values are ever passed in
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

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
  (lambda (x) (top-level-eval (parse-exp x))))










