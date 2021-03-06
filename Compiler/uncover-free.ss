(library (Compiler uncover-free)
  (export uncover-free)
  (import (chezscheme) (Framework match) (Framework helpers) (Compiler helpers))
  (define-who (uncover-free x)
    (define (Expr x)
      (match x
        ((letrec ([,var* (lambda (,fml* ...) ,[exp*])] ...) ,[exp])
         (let ([fr (map uncover exp* fml*)])
           `(letrec ([,var* (lambda (,@fml*) (free ,fr ,exp*))] ...) ,exp)))
        ((let ([,var* ,[exp*]] ...) ,[exp]) `(let ([,var* ,exp*] ...) ,exp))
        ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
        ((begin ,[exp*] ... ,[exp]) `(begin ,@exp* ,exp))
        ((,[rat] . ,[rand]) `(,rat . ,rand))
        (,x x)))
    (define (uncover x fml)
      (match x
        (,var (guard (uvar? var)) (if (member var fml) '() `(,var)))
        ((letrec ([,var* ,lam*] ...) ,exp)
         (filter (lambda (x) (not (member x fml)))
                 (union (uncover lam* var*) (uncover exp var*))))
        ((lambda (,fml* ...) ,exp)
         (filter (lambda (x) (not (member x fml))) (uncover exp fml*)))
        ((let ([,var* ,exp*] ...) ,exp)
         (filter (lambda (x) (not (member x fml)))
                 (union (uncover exp* fml) (uncover exp var*))))
        ((,rat . ,rand) (union (uncover rat fml) (uncover rand fml)))
        (,x '())))
    (Expr x)))
