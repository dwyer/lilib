; Expands begin, case, cond, let and let* in terms of if and lambda.
; Useful for preprocessing scheme code before compilation. 
(define expand
  (begin

    (define (expand-begin expr)
      (list (cons 'lambda (cons '() (expand-expr (cdr expr))))))

    (define (expand-case expr)
      expr)

    (define (expand-cond expr)
      (define (iter seq)
        (if (null? seq)
          '()
          (let* ((this (car seq))
                 (rest (cdr seq))
                 (pred (car this))
                 (con (cdr this)))
            (if (eq? pred 'else)
              (expand-expr con)
              (list 'if (expand-expr pred) (expand-expr (cons 'begin con)) (iter rest))))))
      (iter (cdr expr)))

    (define (expand-define expr)
      (if (pair? (cadr expr))
        (let* ((defn (cadr expr))
               (body (cddr expr))
               (func (car defn))
               (vars (cdr defn)))
          (list 'define
                (expand-expr func)
                (cons 'lambda
                      (cons (expand-expr vars)
                            (expand-expr body)))))))

    (define (expand-expr expr)
      (if (pair? expr)
        (expand-pair expr)
        expr))

    (define (expand-let expr)
      (let* ((defn (cadr expr))
             (body (cddr expr))
             (vars (map car defn))
             (vals (map cadr defn)))
        (cons (cons 'lambda
                    (cons vars
                          (expand-expr body)))
              vals)))

    (define (expand-let* expr)
      (let ((defn (cadr expr))
            (body (cddr expr)))
        (if (or (null? defn) (null? (cdr defn)))
          (expand-let expr)
          (let ((var (caar defn))
                (val (cadar defn))
                (body (list (cons 'let (cons (cdr defn) body)))))
            (list (cons 'lambda (cons (list var) body)) val)))))

    (define (expand-pair expr)
      (let ((this (car expr))
            (rest (cdr expr)))
        (case this
          ((begin) (expand-begin expr))
          ((case) (expand-case expr))
          ((cond) (expand-cond expr))
          ;((define) (expand-define expr))
          ((let) (expand-let expr))
          ((let*) (expand-let* expr))
          (else (cons (expand-expr this)
                      (expand-expr rest))))))

      expand-expr))
