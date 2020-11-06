; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))


; note that with extend-env and extend-env-w-ref

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map cell (map cdr vals)) env))) ; map cell vals simply gives us cells instead of values

(define extend-env-w-ref
  (lambda (syms vals env newnev)
    (let* ([sym-group?
            (lambda (group)
              (let ([id (car group)])
                (if (symbol? id) #t #f)))]
           [group (map cons syms vals)]
           [sym-group (filter sym-group? group)]
           [ref-group (remp sym-group? group)]
           [sym-vals (if (null? sym-group) '() (map cell (map cdr (cdr sym-group))))]
           [ref-vals-for-lookup (map cadr (map car (map cdr ref-group)))]
           [ref-vals (map (lambda (x) (apply-env-ref newnev x)) ref-vals-for-lookup)])
      (extended-env-record
       (append (car sym-group) (car ref-group))
       (append sym-vals ref-vals)
       env))))


(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
;; DONE: add apply-env-ref
;; change apply-env to (deref (apply-env-ref env sym))
;; note that what is now done is that apply-env dereferences
;; whatever address is returned to it from apply-env-ref
;; you will note that apply-env-ref contains the same
;; code as the previous apply-env did. this is because
;; we now return the cells associated, as you see with
;; extend-env

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
  (lambda (env var)
    (deref (apply-env-ref env var))))


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
