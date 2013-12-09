(define (describe message)
  (newline)
  (display "Test ")
  (display message)
  (newline))

(define-syntax assert
  (syntax-rules ()
    ((_ form test)
     (let* ((value form)
            (equal (equal? value test)))
       (if equal
          (display "[OK]   ")
          (display "[FAIL] "))
       (write 'form)
       (display " should be equal to ")
       (write test)
       (if (not equal)
           (begin (display ". Actual value is ")
                  (write value)))
       (newline)))))

'test-framework-loaded
