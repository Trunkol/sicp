(define (sum-integers a b)
  (if (> a b)
  	0
  	(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (define (cube a)
    (* a a a))
  (if (> a b)
  	0
  	(+ (cube a) (sum-cubes (+ a 1) b))))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (+ (lambda (x) (/ 1.0 (* x (+ x 2))))
  		a
  		(lambda (x) (+ x 4))
  		b))
