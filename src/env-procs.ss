; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3
; For the lexical address branch, the environment datatype
; will not be used at all. Rather, a dynamic global environment
; will include free-vars and static environments will contain
; all other bound variables



(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
            [(eq? sym (car los)) pos]
            [else (loop (cdr los) (add1 pos))]))))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (vals env)
    (cons (map cell vals) env))) ; map cell vals simply gives us cells instead of values

;; DONE: add apply-env-ref
;; change apply-env to (deref (apply-env-ref env sym))
;; note that what is now done is that apply-env dereferences
;; whatever address is returned to it from apply-env-ref
;; you will note that apply-env-ref contains the same
;; code as the previous apply-env did. this is because
;; we now return the cells associated, as you see with
;; extend-env
;; note that for the lexical address branch, applying an
;; environment is as simple as calling list-ref

(define apply-env-ref
  (lambda (env lxadr)
    (let ([depth (2nd lxadr)]
          [index (3rd lxadr)])
      (if (eq? 'free depth)
          (apply-global-env index)
          (list-ref index (list-ref depth env))))))

(define apply-env
  (lambda (env lxadr)
    (deref (apply-env-ref env lxadr))))


;; global environment implementation



(define apply-global-env
  (lambda (sym)
    (let* ([syms (car global-env)]
           [vals (cdr global-env)]
           [pos (list-find-position sym syms)])
      (if (number? pos)
          (list-ref vals pos)
          (eopl:error 'global-env "global environment is missing symbol ~s" sym)))))
   
(define extend-global-env
  (lambda (syms vals env)
    (letrec ([merge
              (lambda (syms vals o-syms o-vals)
                (if (null? syms)
                    (cons o-syms o-vals)
                    (let ([pos (list-find-position (car syms) o-syms)])
                      (if (number? pos)
                          (begin
                            (set!-ref
                             (list-ref o-vals pos)
                             (car vals))
                            (merge (cdr syms) (cdr vals) o-syms o-vals))
                          (merge (cdr syms)
                                 (cdr vals)
                                 (cons (car syms) o-syms)
                                 (cons (cell (car vals)) o-vals))))))])
      (let* ([o-syms (car env)]
             [o-vals (cdr env)]
             [new (merge syms vals (deref o-syms) (deref o-vals))])
        (set!-ref o-syms (car new))
        (set!-ref o-vals (cdr new))))))


(define reset-global-env
  (lambda ()
    (let ([syms (car global-env)]
          [vals (cdr global-env)])
      (set!-ref syms *prim-proc-names*)
      (set!-ref vals (map prim-proc *prim-proc-names*)))))


;; lexical addressing also requires the use of static environments for lexing step
;; we implement those here


(define empty-senv
  (lambda ()
    '()))


(define extend-senv
  (lambda (vars senv)
    (cons vars senv)))

(define apply-senv
  (lambda (senv var)
    (let recur ([depth 0]
                [senv senv])
      (if (null? senv)
          (free-var-exp var)
          (let ([index (list-find-position var (car senv))])
          (if (number? index)
              (bound-var-exp depth index)
              (recur (add1 depth) (cdr senv))))))))
