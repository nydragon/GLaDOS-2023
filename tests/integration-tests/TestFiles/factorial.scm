(define (fact x)
    (if (== x 1)
        1
        (* x (fact (- x 1)))))
(print (fact 10))