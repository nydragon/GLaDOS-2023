(define (> a b)
    (if (== a b)
        #f
        (if (< a b)
            #f
            #t)))
(print (> 10 -2))