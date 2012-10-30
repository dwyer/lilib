(define (sexp->html expr)
  ; Converts an S-expression to XML.
  ; TODO: document

  (define (tagged-list? expr)
    (and (pair? expr) (symbol? (car expr))))

  (define (atom->string expr)
    (cond ((number? expr) (number->string expr))
          (else expr)))
  (define (iter expr)
    (cond ((null? expr) '())
          ((tagged-list? expr)
           (make-element (symbol->string (car expr)) '() (cdr expr)))
          ((pair? expr) `(,@(iter (car expr)) ,@(iter (cdr expr))))
          (else `(,(atom->string expr)))))

  (define (make-element name attrs expr)
    (cond ((null? expr) `("<" ,name ,@attrs "/>"))
          ((tagged-list? expr)
           (make-element name
                         `(,@attrs " " ,(symbol->string (car expr))
                                   "=\"" ,(atom->string (cadr expr)) "\"")
                         (cddr expr)))
          (else `("<" ,name ,@attrs ">" ,@(iter expr) "</" ,name ">"))))

  (apply string-append (iter expr)))
