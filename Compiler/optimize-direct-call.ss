(library (Compiler optimize-direct-call)
  (export optimize-direct-call)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (optimize-direct-call x)
    (define (optimize x)
      (match x
        ((letrec ([,var* ,[progs*]] ...) ,[body])
         `(letrec ([,var* ,progs*] ...) ,body))
        ((let ([,var* ,[bind*]] ...) ,[body])
         `(let ([,var* ,bind*] ...) ,body))
        ((begin ,[eff*] ... ,[eff])
         (make-begin `(,@eff* ,eff)))
        ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
        ;; begin rator/rand matching
        (((lambda (,fml* ...) ,[body]) ,[exp*] ...)
         (if (= (length fml*) (length exp*))
             `(let ([,fml* ,exp*] ...) ,body)
             (error who "Arity mismatch ~s"
                    `((lambda (,@fml*) ,body) ,@exp*))))
        ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
        ;; the dream-catcher
        (,x x)))
    (optimize x)))
