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


(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)