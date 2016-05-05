(library (Compiler remove-let)
  (export remove-let)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (remove-let x)
    (define (bangify x)
      (match x
        ((locals ,var (let ([,uvar* ,[val*]] ...) ,[exp]))
         `(locals ,var ,(make-begin `((set! ,uvar* ,val*) ... ,exp))))
        ((let ([,uvar* ,[val*]] ...) ,[exp]) (make-begin `((set! ,uvar* ,val*) ... ,exp)))
        ((,[exp*] ...) `(,@exp*))
        (,x x)))
    (match x
      ((letrec ([,label* (lambda ,fml ,[bangify -> body*])] ...) ,[bangify -> body])
       `(letrec ([,label* (lambda ,fml ,body*)] ...) ,body)))))
