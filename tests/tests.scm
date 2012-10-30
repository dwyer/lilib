(define (fact n)
  (if (< n 2)
    1
    (* n (fact (- n 1)))))

(define (fact-iter n)
  (define (iter n p)
    (if (< n 2)
      p
      (iter (- n 1) (* n p))))
  (iter n 1))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (fib-iter n)
  (define (iter n p q)
    (cond ((= n 0) p)
          ((= n 1) q)
          (else (iter (- n 1) q (+ p q)))))
  (iter n 0 1))

(let ((lst (list fact fact-iter fib fib-iter)))
  (for-each (lambda (f)
              (display (f 12))
              (newline))
            lst))
