(letrec ([fact
          ((lambda (y)
             ((lambda (f*) (f* f*))
              (lambda (f*)
                (y (lambda (x) ((f* f*) x))))))
           (lambda (fact)
             (lambda (x)
               (if (= x 0)
                   1
                   (* x (fact (- x 1)))))))])
  (fact 5))
