(library (Compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (sanitize-binding-forms x)
    (define (handle-let var* bind* body)
      (let-values
          ([(lam* exp*)
            (let loop ([var* var*] [bind* bind*]
                       [lam* '()] [exp* '()])
              (if (null? var*) (values lam* exp*)
                  (match (car bind*)
                    ((lambda ,fml ,[Exp -> exp])
                     (loop (cdr var*) (cdr bind*)
                           (cons `(,(car var*) (lambda ,fml ,exp)) lam*)
                           exp*))
                    (,[Exp -> x]
                     (loop (cdr var*) (cdr bind*)
                           lam*
                           (cons `(,(car var*) ,x) exp*))))))])
        (let* ([ltr-exp (if (null? lam*) body `(letrec ,lam* ,body))]
               [let-exp (if (null? exp*) ltr-exp `(let ,exp* ,ltr-exp))])
          let-exp)))
    (define (Exp x)
      (match x
        ((letrec ([,var* (lambda (,fml* ...) ,[body*])] ...) ,[body])
         `(letrec ([,var* (lambda (,fml* ...) ,body*)] ...) ,body))
        ((let ([,var* ,bind*] ...) ,[body])
         (handle-let var* bind* body))
        ((begin ,[eff*] ... ,[eff])
         (make-begin `(,@eff* ,eff)))
        ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
        ;; rator/rand
        ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
        ;; dream catcher
        (,x x)))
    (Exp x)))
