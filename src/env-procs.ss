; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3
; For the lexical address branch, the environment datatype
; will not be used at all. Rather, a dynamic global environment
; will include free-vars and static environments will contain
; all other bound variables

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
  (lambda (env sym)
    (cases environment env
           [extended-env-record
            (syms vals env)
            (let ((pos (list-find-position sym syms)))
              (if (number? pos)
                (list-ref vals pos)
                (apply-env-ref env sym)))]
           [global-env-record
            (syms vals)
            (let ([syms (deref syms)]
                  [vals (deref vals)])
              (let ((pos (list-find-position sym syms)))
                (if (number? pos)
                    (list-ref vals pos)
                    (eopl:error 'global-env "Symbol ~s is not bound in the global environment" sym))))]
           [empty-env-record
            ()
            (eopl:error 'global-env "Fatal error: global environment improperly constructed")])))


(define apply-env
  (lambda (env lxadr)
    (deref (apply-env-ref env lxadr))))


;;
(define apply-global-env
  (lambda (sym)
    (eopl:error 'global-env "apply-global-env should not be called")))
    ;; (cases environment init-env
    ;;        [extended-env-record
    ;;         (syms vals env)
    ;;         (let ([pos (list-find-position sym syms)])
    ;;           (if (number? pos)
    ;;               (list-ref vals pos)
    ;;               (eopl:error 'global-env
    ;;                           "Symbol ~s is not bound in the global environment"
    ;;                           sym)))]
    ;;        [empty-env-record
    ;;         ()
    ;;         (eopl:error 'global-env "Fatal error: global environment improperly extended")])))

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
      (cases environment env
             [extended-env-record
              (o-syms o-vals env)
              (extend-global-env syms vals env)]
             [global-env-record
              (o-syms o-vals)
              (let ([new (merge syms vals (deref o-syms) (deref o-vals))])
                (set!-ref o-syms (car new))
                (set!-ref o-vals (cdr new)))]
             [empty-env-record
              ()
              (eopl:error 'global-env "Fatal error: environments improperly extended")]))))


(define reset-global-env
  (lambda ()
    (set! global-env
      ((lambda ()
         (let ([nenv (global-env-record (cell '()) (cell '()))])
           (extend-global-env
            *prim-proc-names*
            (map prim-proc *prim-proc-names*)
            nenv)
           nenv))))))


;; not needed for lexical address branch
;;

;; (define list-find-position
;;   (lambda (sym los)
;;     (let loop ([los los] [pos 0])
;;       (cond [(null? los) #f]
;; 	    [(eq? sym (car los)) pos]
;; 	    [else (loop (cdr los) (add1 pos))]))))
