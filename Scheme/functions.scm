(define (mymap func list)
     (if (null? list) '() 
        (cons (func (car list)) (mymap func (cdr list)))
        )
    )

(define (inc num) (+ num 1))

(define (sq num) (* num num))

(define (factorial n) 
    (if (eq? n 0) 1 
        (* n (fact (- n 1)))))

;filters even elements from the given list

(define (filterEven list) 
    (if (null? list) '() 
        (if (= (modulo (car list) 2) 0) 
            (cons (car list) (filterEven (cdr list)) 
                (filterEven (cdr list))))))

;counts how many non-nested elements are in the list

(define (listlen list) 
    (if (null? list) 0
        (+ 1 (listlen (cdr list)))))


;counts how many elements are in a given nested list

(define (countElements list) 
    (if (null? list) 0 
        (if (not (pair? list)) 1 
            (+ (countElements (car list)) (countElements (cdr list))))))


;filter elements based on some test 

(define (filter test list) 
    (if (null? list) '()
        (if (test (car list)) (cons (car list) (myfilter test (cdr list))) 
            (myfilter test (cdr list)))))


;returns true if the element is a part of the list
(define (detect element list) (if (null? list) #f 
    (if (eq? element (car list)) #t 
        (detect element (cdr list)))))


;appends the second list to the first

(define (append list1 list2) (if (null? list1) list2 
    (cons (car list1) (append (cdr list1) list2))))

;reverses the list

(define (reverse L) 
    (if (null? L) '()
    (append (reverse (cdr L)) (list (car L)))))

;transforms a nested list into a single-level list
(define (flatten list) 
    (if (null? list) '()
        (if (not (pair? list) (cons list '()) ;this triggers if list is not a pair (when a single element is passed)
            (append (flatten (car list)) (flatten (cdr list)))))) 
                
;recursive power function            
(define (power num expt)
    (if (= expt 0) 1
        (* num (power num (- expt 1)))))


;square root function that uses binary search for better efficiency
(define (mysqrt num) (mysqrt_help num 0 num))

(define (mysqrt_help num i k) 
    (let ((m (quotient (+ i k) 2)))
        (if (= (square m) num) m
            (if (> (square m) num) (mysqrt_help num i (- m 1))
                (mysqrt_help num m k)))))


;computes a logarithm of any base
(define (log base num) (log_help base 1 num))

(define (log_help base expt num) 
    (if (= (power base expt) num) expt
        (log_help base (+ expt 1) num)))
