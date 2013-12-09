(load "tests.scm")
(load "interp.scm")

;; -----------------------------------------------------------------------------
;; Environments tests
;; -----------------------------------------------------------------------------
(define (env-tests)
  (define first-level-env (extend-environment '(fl-first fl-second fl-third)
                                              '("first" "second" "third")
                                              the-empty-environment))
  (define second-level-env (extend-environment '(sl-first sl-second sl-third)
                                               '("second first" "second second" "second third")
                                               first-level-env))
  (define third-level-env (extend-environment '(tl-first tl-second tl-third)
                                              '("third first" "third second" "third third")
                                              second-level-env))

  (define (first-level env)
    (assert (lookup-variable-value 'fl-first env)  "first")
    (assert (lookup-variable-value 'fl-second env) "second")
    (assert (lookup-variable-value 'fl-third env)  "third"))


  (define (second-level env)
    (assert (lookup-variable-value 'sl-first env)  "second first")
    (assert (lookup-variable-value 'sl-second env) "second second")
    (assert (lookup-variable-value 'sl-third env)  "second third"))

  (define (third-level env)
    (assert (lookup-variable-value 'tl-first env)  "third first")
    (assert (lookup-variable-value 'tl-second env) "third second")
    (assert (lookup-variable-value 'tl-third env)  "third third"))

  (describe "environments")
  (describe "first level access")
  (first-level first-level-env)

  (describe "second level access")
  (first-level second-level-env)
  (second-level second-level-env)

  (describe "third level access")
  (first-level third-level-env)
  (second-level third-level-env)
  (third-level third-level-env)

  (describe "define variable")
  (define-variable! 'just-defined-in-fl "just defined in first level" first-level-env)
  (define-variable! 'just-defined-in-sl "just defined in second level" second-level-env)
  (define-variable! 'just-defined-in-tl "just defined in third level" third-level-env)
  (assert (lookup-variable-value 'just-defined-in-fl third-level-env) "just defined in first level")
  (assert (lookup-variable-value 'just-defined-in-sl third-level-env) "just defined in second level")
  (assert (lookup-variable-value 'just-defined-in-tl third-level-env) "just defined in third level")
  (define-variable! 'just-defined-in-sl "overwrited" third-level-env)
  (assert (lookup-variable-value 'just-defined-in-sl third-level-env) "overwrited")

  (describe "set variable value")
  (set-variable-value! 'just-defined-in-fl "first setted" second-level-env)
  (set-variable-value! 'just-defined-in-sl "second setted" third-level-env)
  (set-variable-value! 'just-defined-in-tl "third setted" third-level-env)
  (assert (lookup-variable-value 'just-defined-in-fl third-level-env) "first setted")
  (assert (lookup-variable-value 'just-defined-in-sl third-level-env) "second setted")
  (assert (lookup-variable-value 'just-defined-in-tl third-level-env) "third setted"))

;; -----------------------------------------------------------------------------
;; Eval / Apply Tests
;; -----------------------------------------------------------------------------
(define (eval-tests)
  (describe "eval")

  (let ((env (extend-environment '(eleven) '("eleven") (setup-environment))))
    (describe "self evaluated")
    (assert (eval 11 env) 11)
    (assert (eval "eleven" env) "eleven")

    (describe "quotation")
    (assert (eval '11 env) 11)
    (assert (eval '"eleven" env) "eleven")

    (describe "variable")
    (assert (eval '(begin eleven) env) "eleven")

    (describe "define variable")
    (assert (eval '(begin (define twelve 12) twelve) env) 12)

    (describe "set variable")
    (assert (eval '(begin (set! twelve "tw") twelve) env) "tw")

    (describe "lambda")
    (assert (eval '((lambda (f) (f eleven)) (lambda (x) x)) env) "eleven")

    (describe "if")
    (assert (eval '(if true  1 2) env) 1)
    (assert (eval '(if false 1 2) env) 2)
    (assert (eval '(if 0     1 2) env) 1)
    (assert (eval '(if '()   1 2) env) 1)
    (assert (eval '(if true  1)   env) 1)
    (assert (eval '(if false 1)   env) false)

    (describe "cond")
    (assert (eval '(cond (true 1))           env) 1)
    (assert (eval '(cond (false 1) (else 2)) env) 2)
    (assert (eval '(cond (false 1) (true 2)) env) 2)
    (assert (eval '(cond (false 1))          env) false)
    (assert (eval '(cond (true 1 2 3))       env) 3)

    (describe "application")
    (describe "primitive procedures")
    (assert (eval '(cons 1 2) env) (cons 1 2))
    (assert (eval '(car (cons 1 2)) env) (car (cons 1 2)))
    (assert (eval '(null? '()) env) true)

    (describe "compound procedure")
    (assert (eval '(begin (define (dec x)
                            (- x 1))
                          (define (fac x)
                            (if (= x 1)
                                1
                                (* (fac (dec x)) x)))
                          (fac 6)) env)
            720)))

(define (run-tests)
  (env-tests)
  (eval-tests))
