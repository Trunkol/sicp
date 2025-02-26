"""
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))
"""
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count) 
  			product 
  			(fact-iter (* counter product) (+ counter 1) max-count)))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (fib n)
  (cond ((= n 0) 0)
  			((= n 1) 1)
  			(else (+ (fib (- n 1))
  							 (fib (- n 1))))))

(define (fibperf n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0) 
  		b 
  		(fib-iter (+ a b) a (- count 1))))

(define (C x)
  (cond ((< x 3) x)
        (else (+ (C (- x 1)) (* 2 (C (- x 2))) (* 3 (C (- x 3)))))))

(define (expt b n)
  (if (= n 0) 
    1 
    (* b (expt b (- n 1)))))

(define (exptperf b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

