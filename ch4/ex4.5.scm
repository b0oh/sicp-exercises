;; Exercise 4.5.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.5

;; Scheme allows an additional syntax for cond clauses,
;; (<test> => <recipient>). If <test> evaluates to a true value,
;; then <recipient> is evaluated. Its value must be a procedure
;; of one argument; this procedure is then invoked on the value of
;; the <test>, and the result is returned as the value of the cond expression.
;; For example

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false))

;; returns 2.
;; Modify the handling of cond so that it supports this extended syntax.

(load "tests.scm")
(load "interp.scm")

(define (cond-extended-clause? clause)
  (eq? '=> (car (cond-actions clause))))
(define (cond-extended-action clause)
  (cadr (cond-actions clause)))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ((first (first-exp clauses))
              (rest (rest-exps clauses)))
          (cond ((cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-actions first))
                     (error "ELSE clause isn't last -- COND->IF" clauses)))
                ((cond-extended-clause? first)
                 (make-if (cond-predicate first)
                          (list (cond-extended-action first)
                                (cond-predicate first))
                          (expand-clauses rest)))
                (else
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest)))))))
  (expand-clauses (cond-clauses exp)))

(define (ex4.5-tests)
  (define env (setup-environment))
  (describe "extended cond")

  (assert (eval '(cond (true 1))           env) 1)
  (assert (eval '(cond (false 1) (else 2)) env) 2)
  (assert (eval '(cond (false 1) (true 2)) env) 2)
  (assert (eval '(cond (false 1))          env) false)
  (assert (eval '(cond (true 1 2 3))       env) 3)
  (assert (eval '(begin
                   (define (assoc key alist)
                     (cond ((null? alist) false)
                           ((eq? key (car (car alist))) (car alist))
                           (else (assoc key (cdr alist)))))
                   (cond ((assoc 'b '((a 1) (b 2))) => cadr)))
                env)
          2))
