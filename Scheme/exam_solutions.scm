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


(define (p2a plist) (if (null? plist) '()
    (cons (cons (car plist) (car (cdr plist))) (p2a (cdr (cdr plist))))))

(define (sequence s f n) (if (= n 0) '()
    (cons s (sequence (f s) f (- n 1)))))

;reverses the list L and all of the nested sublists
(define (twist L) (if (not (pair? L)) L
    (map twist (reverse L))))

;counts how many non-null elements are in L and its sublists
(define (countall L) (if (null? L) 0
    (if (not (pair? L)) 1
    (+ (countall (car L)) (countall (cdr L))))))


(define (mapall L) (if (null? L) '()
    (if (not (pair? L)) (f L)
    (cons (mapall f (car L)) (mapall f (cdr L))))))


(define (fun L) (if (null? L) 0
    (if (and (= (modulo (car L) 2) 1) (> (car L) 0)) (+ (square (square (car L))) (fun (cdr L))) (fun (cdr L)))))


(define (matrix row col n) (if (= row 0) '()
    (cons (buildrow col n) (matrix (- row 1) col (+ n col)))))

(define (buildrow col n) (if (= col 0) '()
    (cons n (buildrow (- col 1) (+ n 1)))))

(define (last L) (car (reverse L)))

(define (init L) (reverse (cdr (reverse L))))

(define (antitranspose M) (if (null? (car M)) '() (cons (reverse (map last M)) (antitranspose (map init M)))))

(define (apply-left funlist x) (if (null? funlist) x
    (apply-left (cdr funlist) ((car funlist) x))))

(define (apply-right funlist x) (apply-left (reverse ))
  
(define (first-atom L) (if (not (pair? L)) L 
    (let ((x (first-atom (car L)))) (if (null? x) (first-atom (cdr L)) x))))
  
(define (last-atom L) (if (not (pair? L)) L
    (let ((x (last-atom (cdr L)))) (if (null? x) (last-atom (car L)) x))))
  
(define (maxdepth L) (if (null? L) 1
    (if (not (pair? L)) 0
        (+ 1 (fold-right max 0 (map maxdepth L))))))
