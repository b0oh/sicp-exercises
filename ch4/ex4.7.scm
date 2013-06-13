;; Exercise 4.7.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.7

;; Let* is similar to let, except that the bindings of the let variables
;; are performed sequentially from left to right, and each binding is
;; made in an environment in which all of the preceding bindings are
;; visible. For example

;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))

;; returns 39. Explain how a let* expression can be rewritten as a set of
;; nested let expressions, and write a procedure let*->nested-lets that
;; performs this transformation. If we have already implemented let
;; (exercise 4.6) and we want to extend the evaluator to handle let*, is
;; it sufficient to add a clause to eval whose action is

;; (eval (let*->nested-lets exp) env)

;; or must we explicitly expand let* in terms of non-derived expressions?

(load "ex4.6.scm")

(define (let*->nested-lets exp)
  (define (iter assignments)
    (if (null? assignments)
        '()
        (let ((assignment (list (car assignments))))
          (if (last-exp? assignments)
              (cons 'let (cons assignment (let-body exp)))
              (cons 'let (list assignment (iter (rest-exps assignments))))))))
  (iter (let-vars exp)))
