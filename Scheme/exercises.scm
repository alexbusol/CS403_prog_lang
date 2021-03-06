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

; Exercise 3. Write a comb function such that (comb n k) returns the number of combinations n-choose-k.

; step 1: need to write a helper factorial function
(define (factorial n)
      (if (< n 0) #f)
      (if (<= n 1) 1
            (* n (factorial (- n 1)))
      )
)

; step 2: write combination function that calls facturial
; to implement the combination formula: n! / k!(n-k)!
(define (comb n k) 
      (/ (factorial n) (* (factorial k) (factorial (- n k))))
)

; Exercise 4. Write an insertion_sort function such that (insertion_sort L)
; returns the elements of L rearranged into ascending order

(define (insertion_sort L)
	(if (null? L)
		'()
	    	;recursivelly sort the list if L is not empty
		(insHelper (car L) (insertion_sort (cdr L)))
	)
)

(define (insHelper x L)
	(if (null? L)
	     (list x)	
      ;y is the first element of L
	;ys is the rest of L
	(let ((y (car L)) (ys (cdr L)))
		(if (< x y)
			(cons x L)
		(cons y (insertionHelper x ys))
		)
	)
)
)
