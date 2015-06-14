(define factorial
  (lambda (n)
    (if (= n 0)
      1
      (* n (factorial (dec n))))))

(define dec (lambda (n) (- n 1)))

(factorial 5)
