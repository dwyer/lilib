(load "expand.scm")

(define (scm->js expr)

  (define (eval-atom expr ret?)
    (string-append
      (return ret?)
      (cond ((boolean? expr) (if expr "true" "false"))
            ((null? expr) "null")
            ((number? expr) (number->string expr))
            ((string? expr) (string-append "'" expr "'"))
            ((symbol? expr) (symbol->string expr))
            (else (error 'atom expr)))))

  (define (eval-define expr ret?)
    (let* ((defn (cadr expr)))
      (if (pair? defn)
        (eval-define-proc expr ret?)
        (eval-define-var expr ret?))))

  (define (eval-define-proc expr ret?)
    (let* ((defn (cadr expr))
           (body (eval-seq (cddr expr) #t))
           (proc (eval-expr (car defn) #f))
           (vars (string-join (map (lambda (var) (eval-expr var #f)) (cdr defn)) ",")))
      (string-append (return ret?) "function " proc "(" vars "){" body "}")))
  
  (define (eval-expr expr ret?)
    (if (pair? expr)
      (eval-pair expr ret?)
      (eval-atom expr ret?)))

  (define (eval-for-each expr ret?)
    (let ((proc (eval-expr (cadr expr) #f))
          (args (eval-expr (caddr expr) #f)))
      (string-append "for(__elem__ in (" args ")){(" proc ")((" args ")[__elem__])}")))

  (define (eval-if expr ret?)
    (let ((pred (eval-expr (cadr expr) #f))
          (con (eval-expr (caddr expr) #f))
          (alt (if (null? (cdddr expr))
                 (eval-expr '() #f)
                 (eval-expr (cadddr expr) #f))))
      (string-append (return ret?) "(" pred "?" con ":" alt ")")))

  (define (eval-lambda expr ret?)
    (let ((vars (string-join (map (lambda (var) (eval-expr var #f)) (cadr expr)) ","))
          (body (eval-seq (cddr expr) #t)))
      (string-append (return ret?) "function(" vars "){" body "}")))

  (define (eval-oper oper args ret?)
    (let* ((oper (symbol->string oper))
           (args (string-join (map (lambda (arg) (eval-expr arg #f)) args) oper)))
      (string-append (return ret?) "(" args ")")))

  (define (eval-pair expr ret?)
    (let ((this (car expr))
          (rest (cdr expr)))
      (case this
        ((+ - * / < > <= >=) (eval-oper this rest ret?))
        ((= eq? eqv? equal?) (eval-oper '=== rest ret?))
        ((for-each) (eval-for-each expr ret?))
        ((define) (eval-define expr ret?))
        ((if) (eval-if expr ret?))
        ((lambda) (eval-lambda expr ret?))
        (else (eval-proc-call expr ret?)))))

  (define (eval-proc-call expr ret?)
    (let ((proc (eval-expr (car expr) #f))
          (args (string-join (map (lambda (arg) (eval-expr arg #f)) (cdr expr)) ",")))
      (string-append (return ret?) "(" proc ")(" args ")")))

  (define (eval-seq expr ret?)
    (cond ((null? expr) (eval-expr '() ret?))
          ((null? (cdr expr)) (eval-expr (car expr) ret?))
          (else (string-append (eval-expr (car expr) #f) ";"
                               (eval-seq (cdr expr) ret?))))) ; something's wrong

  (define (return ret?)
    (if ret? "return " ""))

  (eval-expr (expand expr) #f))

(define (loop)
  (let ((expr (read)))
    (if (not (eof-object? expr))
      (begin (display (scm->js expr))
             (newline)
             (loop)))))

(loop)
