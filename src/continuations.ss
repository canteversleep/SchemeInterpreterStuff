;; code for the continuation datatype. defined with eopl:datatypes



;; continuation datatype


(define-datatype continuation continuation?
  [init-k]
  [zero?-k
   (k continuation?)]
  [if-k
   (consequent expression?)
   (alternative optional?)
   (env environment?)
   (k continuation?)])


(define apply-k
  (lambda (k v)
    (cases continuation k
           [init-k () v]
           [zero?-k (sk)
                    (apply-k sk (zero? v))]
           [if-k (consequent alternative env sk)
                 (if v
                     (eval-exp consequent env)
                     (eval-exp alternative env))])))
 ;
