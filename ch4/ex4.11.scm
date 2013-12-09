;; Exercise 4.11.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.11

;; Instead of representing a frame as a pair of lists,
;; we can represent a frame as a list of bindings,
;; where each binding is a name-value pair.
;; Rewrite the environment operations to use this alternative representation.

(load "interp.scm")

(define (make-frame variables values)
  (if (null? variables)
       '()
       (cons (cons (car variables) (car values))
             (make-frame (cdr variables) (cdr values)))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var val))
  (set-cdr! frame (cons (car frame) (cdr frame))))

(define (make-scan var val frame)
  (define (iter fn pairs)
    (if (null? pairs)
        (fn)
        (let ((pair (car pairs)))
          (if (eq? var (car pair))
              (set-cdr! pair val)
              (iter fn (cdr pairs))))))
  (lambda (fn) (iter fn frame)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (on-empty)
      (env-iter (enclosing-environment env)))

    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          ((make-scan var val (first-frame env)) on-empty))))
  (env-iter env))

(define (define-variable! var val env)
  (define frame (first-frame env))
  (define (on-empty)
    (add-binding-to-frame! var val frame))

  ((make-scan var val (first-frame env)) on-empty))

;; for test:
;; (load "interp_tests.scm")
;; (load "ex4.11.scm")
;; (env-tests)
