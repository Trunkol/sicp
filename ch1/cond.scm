(define (abs x)
	(cond ((> x 0) x)
	      ((= x 0) 0)
	      ((< x 0) (-x))))

(define (abss x)
  (cond ((< x 0) (- x))
         (else x)))

(define (absss x)
  (if (< x 0) (- x) x))

(define (sqrt x)
	(define (average a b)
		(/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess sqrt-iter(improve guess)))
	(sqrt-iter 1.0))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
       	product
        (fact-iter (* counter product) (+ counter 1) max-count)))