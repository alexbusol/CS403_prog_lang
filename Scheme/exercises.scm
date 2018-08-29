; Exercise 1. Write a power function such that (power m n) returns m raised to the power n, where n is non-negative integer.

(define (power m n) 
      (if (= n 0) 1
        (* m (power m (- n 1)))
      )
)

; here, the order of the recursion calls is as follows
;   (power 5 2)
;   (* 5 (power 5 1))
;   (* 5 (* 5 (power 5 0))
;   (* 5 (* 5 1)) = 25.

; works for any m and n>=0.


; Exercise 2. Write a log function such that (log m q) returns n such that (power m n) returns q.

(define (log m q)
      (if (<= m q) 1
        (+ (log (/ m q) q) 1)
      )
)