;; Exercise 4.3.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.3

;; Rewrite eval so that the dispatch is done in data-directed style.
;; Compare this with the data-directed differentiation procedure of
;; exercise 2.73. (You may use the car of a compound expression as the
;; type of the expression, as is appropriate for the syntax implemented
;; in this section).

(load "interp.scm")
(load "table.scm")

(define eval-table (make-table))

(define (put exp handler) (insert! exp handler eval-table))
(define (get exp) (lookup exp eval-table))

(put 'quote  (lambda (exp env) (text-of-quotation exp)))
(put 'set!   (lambda (exp env) (eval-assignment exp env)))
(put 'define (lambda (exp env) (eval-definition exp env)))
(put 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp)
                       (lambda-body exp)
                       env)))

(put 'begin  (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'if     (lambda (exp env) (eval-if exp env)))
(put 'cond   (lambda (exp env) (eval (cond->if exp) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get (car exp)) ((get (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
