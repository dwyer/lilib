(load "html.scm")
(load "string.scm")

(define author "Casey Dwyer")
(define description (string-append author "'s Home Page"))
(define keywords '("nightsticks" "water cannons" "tear gas" "padlocks"
                   "Molotov cocktails" "rocks"))
(define title description)
(define subtitle "Hello, world!")
(define links '("http://caseydwyer.org/"
                "https://github.com/dwyer"))

(define expr
  `(html
     (head
       (meta charset "utf-8")
       (meta name "author" content ,author)
       (meta name "description" content ,description)
       (meta name "keywords" content ,(string-join keywords ","))
       (title ,title)
       )
     (body
       (h1 ,title)
       (h2 ,subtitle)
       (p "Welcome to " ,title "!")
       (h3 "My links")
       (p "Here are some links of mine.")
       (ul ,(map (lambda (url)
                   `(li (a rel "me" href ,url ,url)))
                 links)))))

(display "Content-Type:text/html\n\n")
(display "<!DOCTYPE html>")
(display (sexp->html expr))
