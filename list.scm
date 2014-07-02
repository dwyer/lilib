(define (fold-left proc init lst)
  (if (null? lst)
    init
    (fold-left proc (proc (car lst) init) (cdr lst))))

(define (fold-right proc init lst)
  (if (null? lst)
    init
    (fold-right proc (proc init (car lst)) (cdr lst))))

(define (list-split lst obj)
  (let loop ((next lst)
             (last '()))
    (cond ((null? next)
           (list last))
          ((equal? (car next) obj)
           (cons last (loop (cdr next) '())))
          (else (loop (cdr next)
                      (append last (list (car next))))))))

(define (take lst k)
  (if (zero? k)
    '()
    (cons (car lst) (take (cdr lst) (- k 1)))))
