(library (Compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (remove-anonymous-lambda x)
    (define (Lambda x)
      (match x
        ((lambda ,fml ,[Exp -> body])
         `(lambda ,fml
            ,(match body
               ((lambda ,fml ,[body])
                (let ([tmp (unique-name 'anon)])
                  `(letrec ([,tmp (lambda ,fml ,body)]) ,tmp)))
               (,x x))))
        (,[Exp -> x] x)))
    (define (Exp x)
      (match x
        ((letrec ([,var* ,[Lambda -> bind*]] ...) ,[body])
         `(letrec ([,var* ,bind*] ...) ,body))
        ((let ([,var* ,[Lambda -> bind*]] ...) ,[body])
         `(let ([,var* ,bind*] ...) ,body))
        ((lambda (,fml* ...) ,[body])
         (let ([tmp (unique-name 'anon)])
           `(letrec ([,tmp (lambda (,fml* ...) ,body)]) ,tmp)))
        ((begin ,[eff*] ... ,[eff])
         (make-begin `(,@eff* ,eff)))
        ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
        ;; rator/rand
        ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
        ;; the dream-catcher
        (,x x)))
    (Exp x)))
