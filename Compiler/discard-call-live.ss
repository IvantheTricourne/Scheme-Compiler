(library (Compiler discard-call-live)
  (export discard-call-live)
  (import (chezscheme) (Framework helpers) (Framework match))
  (define-who (discard-call-live x)
    (define (Body b)
      (match b
        ((locate (,binds* ...) ,[Tail -> tail]) `(locate (,@binds*) ,tail))))
    (define (Tail t)
      (match t
        ((begin ,[Effect -> eff*] ... ,[tail]) `(begin ,@eff* ,tail))
        ((if ,[Pred -> pred] ,[c] ,[a]) `(if ,pred ,c ,a))
        ((,triv ,loc* ...) `(,triv))))
    (define (Pred p)
      (match p
        ((if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3))
        ((begin ,[Effect -> eff*] ... ,[pred]) `(begin ,@eff* ,pred))
        (,x x)))
    (define (Effect e)
      (match e
        ((if ,[Pred -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2))
        ((begin ,[eff*] ... ,[eff]) `(begin ,@eff* ,eff))
        ((set! ,uvar ,bind) `(set! ,uvar ,bind))
        ((mset! ,base ,off ,exp) `(mset! ,base ,off ,exp))
        ((return-point ,lab ,[Tail -> tail]) `(return-point ,lab ,tail))
        ((,triv ,loc* ...) `(,triv))
        (,x x)))
    (match x
      ((letrec ((,label* (lambda () ,[Body -> body*])) ...) ,[Body -> body])
       `(letrec ((,label* (lambda () ,body*)) ...) ,body)))))
