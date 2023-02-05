(define (> a b)
    (if (eq? a b)
        #f
        (if (< a b)
            #f
            #t)))
(print (> 10 -2))