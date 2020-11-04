;; A lexical addresser that takes in expanded syntax expressions produces expressions with
;; lexical addresses, with the purpose of making environmental lookups more efficient.


(define lexical-address
  (lambda (exp)
    (cases expression exp
           [])))
