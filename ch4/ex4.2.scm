;; Exercise 4.2.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.2

;; Louis Reasoner plans to reorder the cond clauses in eval so that the
;; clause for procedure applications appears before the clause for
;; assignments. He argues that this will make the interpreter more
;; efficient: Since programs usually contain more applications than
;; assignments, definitions, and so on, his modified eval will usually
;; check fewer clauses than the original eval before identifying the type
;; of an expression.

;; Louis is upset that his plan didn't work. He is willing to go to any
;; lengths to make his evaluator recognize procedure applications before
;; it checks for most other kinds of expressions. Help him by changing
;; the syntax of the evaluated language so that procedure applications
;; start with call. For example, instead of (factorial 3) we will now
;; have to write (call factorial 3) and instead of (+ 1 2) we will have
;; to write (call + 1 2).

(load "interp.scm")

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((if? exp) (eval-if exp env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
