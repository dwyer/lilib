(load "char.scm")
(load "list.scm")

(define (request-method)
  (get-environment-variable "REQUEST_METHOD"))

(define (request-method-post?)
  (string=? (request-method) "POST"))

(define (request-method-get?)
  (not (request-method-post?)))

(define (query-string)
  (get-environment-variable "QUERY_STRING"))

(define (query-list)
  (string->list (query-string)))

(define (read-chars)
  (let loop ((chr (read-char)))
    (if (eof-object? chr)
      '()
      (cons chr (loop (read-char))))))

(define post-data-list
  (let ((result #f))
    (lambda ()
      (if (not result)
        (set! result (read-chars)))
      result)))

(define (post-data-string)
  (list->string (post-data-list)))

(define (post-data-alist)
  (map (lambda (lst)
         (map list->string
              (list-split lst #\=)))
       (list-split (post-data-list) #\&)))

(define (unquote-data lst)
  (cond ((null? lst) '())
        ((and (eqv? (car lst) #\%)
              (and (not (null? (cdr lst)))
                   (char-digit? (cadr lst) 16))
              (and (not (null? (cddr lst)))
                   (char-digit? (caddr lst) 16)))
         (cons (integer->char
                 (string->number
                   (list->string
                     (list #\# #\x (cadr lst) (caddr lst)))))
               (unquote-data (cdddr lst))))
        (else (cons (let ((chr (car lst)))
                      (if (eqv? chr #\+)
                        #\space
                        chr))
                    (unquote-data (cdr lst))))))

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

(define (parse-query lst)
  (map (lambda (lst)
         (map (lambda (lst)
                (list->string (unquote-data lst)))
              (list-split lst #\=)))
       (list-split lst #\&)))

(define (display-html html)
  (display "Content-type: text/html\n\n")
  (display html))
