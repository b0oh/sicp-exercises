;; Exercise 4.12.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.12

;; The procedures set-variable-value!, define-variable!, and lookup-variable-value
;; can be expressed in terms of more abstract procedures for traversing the environment structure.
;; Define abstractions that capture the common patterns
;; and redefine the three procedures in terms of these abstractions.

(load "interp.scm")

(define (scan found-proc not-found-proc var env)
  (define frame (first-frame env))
  (define (iter vars vals)
    (cond ((null? vars) (not-found-proc env))
          ((eq? var (car vars)) (found-proc vals))
          (else (iter (cdr vars) (cdr vals)))))
  (iter (frame-variables frame)
        (frame-values    frame)))

(define (env-loop found-proc var env)
  (define (not-found env)
    (env-loop found-proc var (enclosing-environment env)))

  (if (eq? env the-empty-environment)
      (error "Unbound variable -- ENV-LOOP" var)
      (scan found-proc not-found var env)))

(define (define-variable! var val env)
  (define (not-found env)
    (add-binding-to-frame! var val (first-frame env)))

  (define (found vals)
    (set-car! vals val))

  (scan found not-found var env))

(define (set-variable-value! var val env)
  (env-loop (lambda (vals) (set-car! vals val)) var env))

(define (lookup-variable-value var env)
  (env-loop (lambda (vals) (car vals)) var env))
