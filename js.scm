(load "expand.scm")

(define (eval-atom expr ret?)
  (string-append (eval-return ret?)
                 (cond ((boolean? expr) (if expr "true" "false"))
                       ((char? expr) (string-append "'" (string expr) "'"))
                       ((null? expr) "null")
                       ((number? expr) (number->string expr))
                       ((string? expr) (string-append "\"" expr "\""))
                       ((symbol? expr) (eval-symbol expr))
                       (else (error 'eval-atom expr)))))

(define (eval-define expr ret?)
  (let ((defn (car expr)))
    (if (pair? defn)
      (eval-define-proc defn (cdr expr) ret?)
      (eval-define-variable defn (cadr expr) ret?))))

(define (eval-define-proc defn body ret?)
  (string-append (eval-return ret?)
                 "function "
                 (eval-return #f)
                 (eval-proc-defn defn #f)
                 "{"
                 (eval-seq body #t)
                 "}"))

(define (eval-define-variable defn body ret?)
  (string-append (eval-return ret?) "var " (eval-symbol defn) "="
                 (eval-expr body #f) ";"))

(define (eval-expr expr ret?)
  (if (pair? expr)
    (eval-pair expr ret?)
    (eval-atom expr ret?)))

(define (eval-for-each expr ret?)
  (let ((func (eval-expr (cadr expr) #f))
        (lst (eval-expr (caddr expr) #f)))
    (string-append "for(__elem__ in " lst "){(" func ")(" lst "[__elem__])};"
                   (if ret? (eval-atom '() #t) ""))))

(define (eval-if expr ret?)
  (let ((pred (eval-expr (car expr) #f))
        (con (eval-expr (cadr expr) #f))
        (alt (if (cddr expr) (eval-expr (caddr expr) #f) "null")))
    (string-append (eval-return ret?) "(" pred "?" con ":" alt ")")))

(define (eval-lambda expr ret?)
  (let ((args (eval-proc-args (car expr)))
        (body (eval-seq (cdr expr) #t)))
    (string-append (eval-return ret?) "function(" args "){" body "}")))

(define (eval-operation op args ret?)
  (string-append (eval-return ret?)
                 "("
                 (string-join (map (lambda (expr)
                                     (eval-expr expr #f)) args)
                              (symbol->string op))
                 ")"))

(define (eval-proc-args args)
  (string-join (map (lambda (expr) (eval-expr expr #f)) args) ","))

(define (eval-proc-call expr ret?)
  (let ((func (car expr))
        (args (cdr expr)))
    (string-append (eval-return ret?)
                   "("
                   (eval-expr func #f)
                   ")("
                   (eval-proc-args args)
                   ")")))

(define (eval-proc-defn expr ret?)
  (let ((func (car expr))
        (args (cdr expr)))
    (string-append (eval-expr func ret?)
                   "("
                   (eval-proc-args args)
                   ")")))

(define (eval-pair expr ret?)
  (let ((tag (car expr))
        (args (cdr expr)))
    (case tag
      ((+ - * / < <= >= >) (eval-operation tag args ret?))
      ((= eq? eqv? equal?) (eval-operation '== args ret?))
      ((append string-append) (eval-operation '+ args ret?))
      ((boolean?) (eval-type (car args) "boolean" ret?))
      ((car) (string-append (eval-expr (car args) ret?) "[0]"))
      ((cdr) (string-append (eval-expr (car args) ret?) ".slice(1)"))
      ((cons) (string-append (eval-return ret?) "[" (eval-expr (car args) ret?) "].concat(" (eval-expr (cadr args) ret?) ")"))
      ((display) (eval-proc-call (cons 'console.log args) ret?))
      ((for-each) (eval-for-each expr ret?))
      ((list) (string-append "[" (eval-proc-args args) "]"))
      ((null?) (string-append (eval-expr (car args) ret?) "==null"))
      ((and) (eval-operation '&& args ret?))
      ((define) (eval-define args ret?))
      ((if) (eval-if args ret?))
      ((lambda) (eval-lambda args ret?))
      ((or) (eval-operation '|| args ret?))
      ((set!) (eval-set! args ret?))
      ((load newline)
       (string-append "/*..." (symbol->string tag) "...*/"))
      (else (eval-proc-call expr ret?)))))

(define (eval-return ret?)
  (if ret? "return " ""))

(define (eval-seq expr ret?)
  (if (null? expr)
    (eval-atom expr ret?)
    (let ((first (car expr))
          (rest (cdr expr)))
      (if (null? rest)
        (eval-expr first ret?)
        (string-append (eval-expr first #f)
                       ";"
                       (eval-seq rest ret?))))))

(define (eval-set! expr ret?)
  (let ((var (car expr))
        (val (cadr expr)))
    (string-append (eval-expr var #f)
                   "="
                   (eval-expr val #f)
                   ";")))

(define (eval-symbol expr)
  (define (iter lst)
    (if (null? lst)
      '()
      (cons (let ((char (car lst)))
              (case char
                ((#\- #\>) #\_)
                ((#\?) #\q)
                (else char)))
            (iter (cdr lst)))))
  (list->string (iter (string->list (symbol->string expr)))))

(define (eval-type expr type ret?)
  (string-append (eval-return ret?)
                 "typeof "
                 (eval-expr expr #f)
                 "==="
                 type))

(define (scm->js expr)
  (eval-expr (expand expr) #f))

(define (loop)
  (let ((expr (read)))
    (if (not (eof-object? expr))
      (begin (display (eval-expr (expand expr) #f))
             (newline)
             (loop)))
    ""))

;(loop)
