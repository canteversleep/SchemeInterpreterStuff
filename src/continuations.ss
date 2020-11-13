;; code for the continuation datatype. defined with eopl:datatypes



;; continuation datatype


(define-datatype continuation continuation?
  (end-cont)
  (zero-cont
   (cont continuation?))
  (if-cont
   (consequent expression?)
   (alternative optional?)
   (env environment?)
   (cont continuation?)))



 ;
