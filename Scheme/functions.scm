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


