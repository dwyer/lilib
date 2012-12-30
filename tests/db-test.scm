(load "rel.scm")

(define (print obj)
  (display obj)
  (newline))

(define E
  '((nr name    salary)
    (1  "John"  100)
    (5  "Sarah" 300)
    (7  "Tom"   100)))

(print (project '(salary) E))
(print (project '(nr salary) E))
(print (select '(< salary 200) E))
(print (select '(and (< salary 200) (>= nr 7)) E))
(print (project '(name salary) (select '(< salary 200) E)))

(define E
  '((enr ename dept)
    (1 "Bill" "A")
    (2 "Sarah" "C")
    (3 "John" "A")))

(define D
  '((dnr dname)
    ("A" "Marketing")
    ("B" "Sales")
    ("C" "Legal")))

(print (product E D))
(print (join '(= dept dnr) E D))

(define E
  '((nr        name    dept)
    (1       "Bill"    "A")
    (2       "Sarah"   "C")
    (3       "John"    "A")))

(define D
  '((nr name)
    ("A"  "Marketing")
    ("B"  "Sales")
    ("C"  "Legal")))

(print (join '(= dept dnr)
             (rename '(enr ename dept) E)
             (rename '(dnr dname) D)))
;(print (join '(= dept (b nr)) E D))

(define E
  '((nr name salary dept)
    (1 "John" 100 "A")
    (5 "Sarah" 300 "C")
    (7 "Tom" 100 "A")
    (12 "Anne" #f "C")))

(print (function '(sum salary) E))
(print (function '(count salary) E))
(print (function '(count salary) (distinct (project '(salary) E))))
(print (function '(avg salary) E))
