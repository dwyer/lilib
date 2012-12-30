(define (get-environment-variable key)
  (or (getenv key) ""))

(define (get-request-method)
  (get-environment-variable "REQUEST_METHOD"))

(define (post-request?)
  (string=? (get-request-method) "POST"))

(define (get-request?)
  (not (post-request?)))

(define (query-string)
  (get-environment-variable "QUERY_STRING"))

(define request-data-string
  (if (get-request?)
    (query-string)
    (symbol->string (read))))

(define (unquote-data str)
  (define (char->num char)
    (cond ((char=? char #\A) 10)
          ((char=? char #\B) 11)
          ((char=? char #\C) 12)
          ((char=? char #\D) 13)
          ((char=? char #\E) 14)
          ((char=? char #\F) 15)
          (else (string->number (string char)))))
  (define (iter lst)
    (if (null? lst)
      '()
      (let ((char (car lst)))
        (cond ((char=? char #\+)
               (cons #\space (iter (cdr lst))))
              ((char=? char #\%)
               (cons (integer->char (+ (* 16 (char->num (cadr lst)))
                                       (char->num (caddr lst))))
                     (iter (cdddr lst))))
              (else (cons char (iter (cdr lst))))))))
  (list->string (iter (string->list str))))

(define (request-data-alist)
  (map (lambda (str) (string-split str #\=))
       (string-split request-data-string #\&)))

(define (request-data-pair key)
  (assoc key (request-data-alist)))

(define (request-data-value key)
  (let ((pair (request-data-pair key)))
    (if pair
      (let ((value (cadr pair)))
        (if value
          (unquote-data value)
          #f))
      #f)))
