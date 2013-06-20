;; Exercise 4.6.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.6

;; Let expressions are derived expressions, because

;; (let ((<var1> <exp1>) ... (<varn> <expn>))
;;   <body>)

;; is equivalent to

;; ((lambda (<var1> ... <varn>)
;;    <body>)
;;  <exp1>
;;  <expn>)

;; Implement a syntactic transformation let->combination that reduces
;; evaluating let expressions to evaluating combinations of the type
;; shown above, and add the appropriate clause to eval to handle let
;; expressions.

(load "interp.scm")

(define (let-assignments exp) (cadr exp))
(define (let-assignment-name exp) (car exp))
(define (let-assignment-value exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-assignment-names assignments) (map let-assignment-name assignments))
(define (let-assignment-values assignments) (map let-assignment-value assignments))

(define (let->combination exp)
  (define assignments (let-assignments exp))
  (cons (make-lambda (let-assignment-names assignments)
                     (let-body exp))
        (let-assignment-values assignments)))
