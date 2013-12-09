;; Exercise 4.4.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.4

;; Recall the definitions of the special forms and and or from chapter 1:

;; and: The expressions are evaluated from left to right. If any
;; expression evaluates to false, false is returned; any remaining
;; expressions are not evaluated. If all the expressions evaluate to true
;; values, the value of the last expression is returned. If there are no
;; expressions then true is returned.

;; or: The expressions are evaluated from left to right. If any
;; expression evaluates to a true value, that value is returned; any
;; remaining expressions are not evaluated. If all expressions evaluate
;; to false, or if there are no expressions, then false is returned.

;; Install and and or as new special forms for the evaluator by defining
;; appropriate syntax procedures and evaluation procedures eval-and and
;; eval-or. Alternatively, show how to implement and and or as derived
;; expressions.

(load "tests.scm")
(load "interp.scm")

(define (eval-and exp env)
  (define (iter args)
    (if (null? args)
        true
        (let ((arg (eval (first-exp args) env)))
          (cond ((last-exp? args) arg)
                ((true? arg) (iter (rest-exps args)))
                (else false)))))
  (iter (rest-exps exp)))

(define (eval-or exp env)
  (define (iter args)
    (if (null? args)
        false
        (let ((arg (eval (first-exp args) env)))
          (cond ((last-exp? args) arg)
                ((true? arg) arg)
                (else (iter (rest-exps args)))))))
  (iter (rest-exps exp)))


;; Implementation as derived expressions

(define (and->if exp env)
  (define (expand-and args)
    (if (null? args)
        'true
        (let ((arg (eval (first-exp args) env)))
          (if (last-exp? args)
              arg
              (make-if arg (expand-and (rest-exps args)) 'false)))))
  (expand-and (rest-exps exp)))

(define (or->if exp env)
  (define (expand-or args)
    (if (null? args)
        'false
        (let ((arg (eval (first-exp args) env)))
          (if (last-exp? args)
              arg
              (make-if arg arg (expand-or (rest-exps args)))))))
  (expand-or (rest-exps exp)))

(define (ex4.4-tests)
  (define env (setup-environment))
  (assert (eval-and '(and)         env) true)
  (assert (eval-and '(and true)    env) true)
  (assert (eval-and '(and 1)       env) 1)
  (assert (eval-and '(and false)   env) false)
  (assert (eval-and '(and true 1)  env) 1)
  (assert (eval-and '(and false 1) env) false)
  (assert (eval-or '(or)               env) false)
  (assert (eval-or '(or true)          env) true)
  (assert (eval-or '(or false)         env) false)
  (assert (eval-or '(or false 1)       env) 1)
  (assert (eval-or '(or false 1 2)     env) 1)
  (assert (eval-or '(or false 1 false) env) 1))
