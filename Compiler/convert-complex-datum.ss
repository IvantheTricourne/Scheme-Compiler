(library (Compiler convert-complex-datum)
  (export convert-complex-datum)
  (import (chezscheme) (Framework match) (Framework helpers) (Compiler helpers))
  (define-who (convert-complex-datum x)
    (define (Datum dat)
      (match dat
        (,x (guard (imm? x)) `(quote ,x))
        ((,[a] . ,[d]) `(cons ,a ,d))
        (#(,[x*] ...)
         (let ([tmp (unique-name 'tmp)]
               [vl (length x*)])
           `(let ([,tmp (make-vector (quote ,vl))])
              (begin
                ,@(let loop ([ls x*] [i 0])
                    (if (null? ls) `(,tmp)
                        (cons `(vector-set! ,tmp (quote ,i) ,(car ls))
                              (loop (cdr ls) (add1 i)))))))))))
    (define (Exp env)
      (lambda (x)
        (let ([lift-binds (lambda (x) (if (null? env) x `(let ,env ,x)))])
          (lift-binds
           (match x
             ;; Quote stuff
             ((quote ,imm) (guard (imm? imm)) `(quote ,imm))
             ((quote ,[Datum -> dat])
              (let ([tmp (unique-name 'tmp)])
                (set! env (cons `(,tmp ,dat) env)) tmp))
             ;; BNF
             ((letrec ([,var* ,[exp*]] ...) ,[exp])
              `(letrec ([,var* ,exp*] ...) ,exp))
             ((let ([,var* ,[exp*]] ...) ,[exp])
              `(let ([,var* ,exp*] ...) ,exp))
             ((lambda (,fml* ...) ,[exp])
              `(lambda (,@fml*) ,exp))
             ((begin ,[eff*] ... ,[eff])
              (make-begin `(,@eff* ,eff)))
             ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
             ;; rator/rand
             ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
             ;; dream catcher
             (,x x))))))
    ((Exp '()) x)))
