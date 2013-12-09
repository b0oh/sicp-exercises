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

(define (let? exp) (tagged-list? exp 'let))

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


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((if? exp) (eval-if exp env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
