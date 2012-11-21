; relational algebra library

(load "set.scm")

; get relation from database
(define (relation name db)
  (cadr (assq name db)))

; get schema from relation
(define (schema rel)
  (car rel))

; get table from relation
; a table is a set of tuples
(define (table rel)
  (cdr rel))

; get attribute from tuple with schema
(define (column attr tup scm)
  (cond ((or (null? tup) (null? scm))
         (error "invalid attribute:" attr))
        ((eq? attr (car scm))
         (car tup))
        (else (column attr (cdr tup) (cdr scm)))))

; get columns named attrs from tuple tup with schema scm.
(define (columns attrs tup scm)
  (map (lambda (attr) (column attr tup scm)) attrs))

; projection
(define (project attrs rel)
  (let ((scm (schema rel)))
    (cons attrs
          (map (lambda (tup)
                 (columns attrs tup scm))
               (table rel)))))

; selection
(define (select pred rel)
  (define ops `((< ,< ,string<?)
                (<= ,<= ,string<=?)
                (= ,= ,string=?)
                (> ,> ,string>?)
                (>= ,>= ,string>=?)))
  (define (pred? tup scm)
    (let* ((theta (car pred))
           (a (cadr pred))
           (b (caddr pred))
           (u (column a tup scm))
           (v (if (symbol? b)
                (column b tup scm)
                b))
           (op ((cond ((number? v) cadr)
                      ((string? v) caddr)
                      (else (error 'idk)))
                (assoc theta ops))))
      (op u v)))
  (cons (schema rel)
        (filter (lambda (tup)
                  (pred? tup (schema rel)))
                (table rel))))

(define (rename a b rel)
  (define (iter scm)
    (cond ((null? scm) '())
          ((eq? (car scm) b)
           (cons a (iter (cdr scm))))
          (else
            (cons (car scm) (iter (cdr scm))))))
  (cons (iter (schema rel)) (table rel)))

; cross project of two relations
(define (relational-cross-product rel1 rel2)
  (define (iter tab1 tab2)
    (if (null? tab1)
      '()
      (append (map (lambda (tup) (append (car tab1) tup)) tab2)
	      (iter (cdr tab1) tab2))))
  (cons (append (schema rel1) (schema rel2))
	(iter (table rel1) (table rel2))))

; natural join
(define (natural-join rel1 rel2)
  (let* ((scm1 (schema rel1))
	 (scm2 (schema rel2))
	 (lst (append scm1 scm2))
	 (scm3 (set lst))
	 (u (intersection scm1 scm2)))
    (define (iter1 tab1 tab2)
      (if (null? tab1)
	'()
	(append (iter2 (car tab1) tab2)
		(iter1 (cdr tab1) tab2))))
    (define (iter2 tup tab)
      (cond ((null? tab) '())
	    ((join? tup (car tab))
	     (cons (join-tuples tup (car tab))
		   (iter2 tup (cdr tab))))
	    (else (iter2 tup (cdr tab)))))
    (define (join-tuples tup1 tup2)
      (columns scm3 (append tup1 tup2) lst))
    (define (join? tup1 tup2)
      (define (iter u)
	(cond ((null? u) #t)
	      ((equal? (column (car u) tup1 scm1)
		       (column (car u) tup2 scm2))
	       (iter (cdr u)))
	      (else #f)))
      (iter u))
    (cons scm3 (iter1 (table rel1) (table rel2)))))
