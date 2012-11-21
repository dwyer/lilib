; set theory lib

(define (set? a)
  (if (null? a)
    #t
    (if (pair? a)
      (let ((x (car a))
	    (a (cdr a)))
	(if (member x a)
	  #f
	  (set? a)))
      #f)))

(define (set a)
  (union a '()))

; the set of all objects that are a member of A, or B, or both
(define (union a b)
  (if (null? a)
    b
    (let ((x (car a))
	  (b (union (cdr a) b)))
      (if (member x b)
	b
	(cons x b)))))

; the set of all objects that are members of both A and B
(define (intersection a b)
  (if (null? a)
    '()
    (let ((x (car a))
	  (c (intersection (cdr a) b)))
      (if (member x b)
	(cons x c)
	c))))

; set of all members of U that are not members of A
(define (set-difference u a)
  (if (null? u)
    '()
    (let ((x (car u))
	  (u (set-difference (cdr u) a)))
      (if (member x a)
	u
	(cons x u)))))

; set of all objects that are a member of exactly one of A and B
; (elements which are in one of the sets, but not in both)
(define (symmetric-difference a b)
  (set-difference (union a b) (intersection a b)))

; the set whose members are all possible ordered pairs (a,b)
; where a is a member of A and b is a member of B
(define (cartesian-product a b)
  (apply append
	 (map (lambda (x)
		(map (lambda (y)
		       (cons x y))
		     b))
	      a)))
