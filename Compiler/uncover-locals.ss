(library (Compiler uncover-locals)
  (export uncover-locals)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (uncover-locals x)
    (define (localize x)
      (match x
        ((let ([,uvar* ,[val* var*]] ...) ,[exp evar*])
         (values `(let ([,uvar* ,val*] ...) ,exp) (apply append `(,uvar* ,@var* ,evar*))))
        ((,[exp* evar*] ...) (values `(,@exp*) (apply append evar*)))
        (,x (values x '()))))
    (match x
      ((letrec ([,label* (lambda ,fml ,[localize -> body* bvar*])] ...) ,[localize -> body bvar])
       `(letrec ([,label* (lambda ,fml (locals ,bvar* ,body*))] ...) (locals ,bvar ,body))))))
