;; Exercise 4.8.
;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_thm_4.8

;; ``Named let'' is a variant of let that has the form

;; (let <var> <bindings> <body>)

;; The <bindings> and <body> are just as in ordinary let, except that
;; <var> is bound within <body> to a procedure whose body is <body> and
;; whose parameters are the variables in the <bindings>. Thus, one can
;; repeatedly execute the <body> by invoking the procedure named <var>.
;; For example, the iterative Fibonacci procedure (section 1.2.2) can be
;; rewritten using named let as follows:

;; (define (fib n)
;;   (let fib-iter ((a 1)
;;                  (b 0)
;;                  (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))

;; Modify let->combination of exercise 4.6 to also support named let.

(load "tests.scm")
(load "ex4.6.scm")

(define (named-let? exp)
  (symbol? (named-let-name exp)))

(define (named-let-name exp) (cadr exp))
(define (named-let-assignments exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

(define (named-let->seq exp)
  (define name (named-let-name exp))
  (define assignments (named-let-assignments exp))
  (make-begin
    (list (cons 'define
                (cons
                  (cons name (let-assignment-names assignments))
                  (named-let-body exp)))
          (cons name (let-assignment-values assignments)))))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->seq exp)
      (let ((assignments (let-assignments exp)))
        (cons (make-lambda (let-assignment-names assignments)
                           (let-body exp))
              (let-assignment-values assignments)))))

(define (ex4.8-tests)
  (describe "named let")
  (assert (eval '(begin
                   (define (fib n)
                     (let fib-iter ((a 1)
                                    (b 0)
                                    (count n))
                       (if (= count 0)
                           b
                           (fib-iter (+ a b) a (- count 1)))))
                   (fib 12))
                (setup-environment))
          144))
