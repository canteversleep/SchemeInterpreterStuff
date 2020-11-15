;; code for the continuation datatype. defined with eopl:datatypes



;; continuation datatype


(define-datatype continuation continuation?
  [init-k]
  [map-k
   (ls list?)
   (proc/k procedure?)
   (k continuation?)]
  [bodies-k
   (bodies (list-of expression?))
   (env environment?)
   (k continuation?)]
  [map-cons-k
   (item scheme-value?)
   (k continuation?)]
  [if-k
   (consequent expression?)
   (alternative (lambda (x) (or (expression? x) (eq? #f x))))
   (env environment?)
   (k continuation?)]
  [rator-k
   (rands (list-of expression?))
   (env environment?)
   (k continuation?)]
  [rands-k
   (proc-value proc-val?)
   (k continuation?)]
  [set!-k
   (id symbol?)
   (env environment?)])


(define apply-k
  (lambda (k v)
    (cases continuation k
           [init-k () v]
           [map-k (ls proc/k k)
                  (map/k proc/k ls (map-cons-k v k))]
           [bodies-k (bodies env k)
                     (eval-bodies bodies env k)]
           [map-cons-k (item k)
                       (apply-k k (cons item v))]
           [if-k (consequent alternative env k)
                 (if v
                     (eval-exp consequent env k)
                     (if alternative
                         (eval-exp alternative env k)))]
           [rator-k (rands env k)
                    (eval-rands rands
                                env
                                (rands-k v k))]
           [rands-k (proc-value k)
                    (apply-proc proc-value v k)]
           [set!-k (id env)
                   (set!-ref
                    (apply-env-ref env id)
                    v)])))

(define map/k
  (lambda (proc/k ls k)
    (if (null? ls)
        (apply-k k '())
        (proc/k (car ls)
                (map-k (cdr ls) proc/k k)))))

;; (define apply/k
;;   (lambda (proc/k . args)))

;; ;
;; (define map-cps
;;   (lambda (pred-cps ls k)
;;     (if (null? ls)
;;         (apply-k k '())
;;         (pred-cps (car ls)
;;                   (make-k
;;                    (lambda (v)
;;                      (map-cps pred-cps (cdr ls)
;;                               (make-k
;;                                (lambda (ret)
;;                                  (apply-k k (cons v ret)))))))))))
