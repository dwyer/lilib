(load "../rel.scm")

(define (print obj)
  (write obj)
  (newline))

(define (print-rel rel)
  (for-each print rel))

(define person
  '((name age weight)
    ("Harry" 34 80)
    ("Sally" 28 64)
    ("George" 29 70)
    ("Helena" 54 54)
    ("Peter" 34 80)))

(define db
  '((employee
      ((name id dept)
       ("harry" 1 "finance")
       ("sally" 2 "sales")
       ("george" 3 "finance")
       ("harriet" 4 "sales")))
    (dept
      ((dept manager)
       ("finance" "george")
       ("sales" "harriet")
       ("production" "charles")))))

(print person)
(print (project '(weight name) person))
(print (select '(>= age 34) person))
(print (select '(= age weight) person))

(let ((employee (relation 'employee db))
      (dept (relation 'dept db)))
  (print-rel (natural-join employee dept)))
