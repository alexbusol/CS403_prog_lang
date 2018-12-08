(define (find key list) (if (null? list) #f 
    (if (eq? key (car (car list))) (cdr (car list)) 
        (find key (cdr list)))))

(define (inner_productR X Y f g) (if (null? (cdr X)) 0
    (f (g (car X) (car Y)) (inner_productR (cdr X) (cdr Y) f g))))

(define (inner_product X Y f g)
    (fold-right f (g (car X) (car Y)) (zip g (cdr X) (cdr Y))))

(define (foldmap func L) 
    (fold-right (lambda (x y) (cons (func x) y)) '() L))

(define (sumProdRec list) (if (null? list) 0
    (+ (product (car list)) (sumProdRec (cdr list)))))

(define (prodRec list) (if (null? list) 1
    (* (car list) (prodRec (cdr list)))))

(define (sumProd list) (fold-right + 0 (map prod list)))

(define (prod list) (fold-right * 1 list))

(define (scan op id L) (if (null? L) (list id)
    (cons id (scan op (op id (car L)) (cdr L)))))

(define (a2p alist) (if (null? alist) '()
    (if (not (pair? alist)) (cons alist '())
    (append (a2p (car alist)) (a2p (cdr alist))))))
