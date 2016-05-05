(library (Compiler remove-complex-opera*)
  (export remove-complex-opera*)
  (import (chezscheme) (Framework helpers) (Framework match) (Compiler helpers))
  (define-who (remove-complex-opera* x)
    (define (Body bd)
      (define new-local* '())
      (define (new-t)
        (let ([t (unique-name 't)])
          (set! new-local* (cons t new-local*))
          t))
      (define (trivialize-call expr*)
        (let-values ([(call set*) (break-down-expr* expr*)])
          (make-begin `(,@set* ,call))))
      (define (break-down-expr* expr*)
        (match expr*
          [() (values '() '())]
          [(,s . ,[rest* set*]) (guard (simple? s)) (values `(,s ,@rest*) set*)]
          [(,[Value -> expr] . ,[rest* set*])
           (let ([t (new-t)])
             (values `(,t ,@rest*) `((set! ,t ,expr) ,@set*)))]
          [,expr* (error who "invalid Expr ~s" expr*)]))
      (define (simple? x)
        (or (uvar? x) (label? x) (and (integer? x) (exact? x))
            (memq x '(+ - * logand logor sra mset! alloc mref)) (memq x '(= < <= > >=))))
      (define (triv? x) (or (uvar? x) (int64? x) (label? x)))
      (define (Tail tail)
        (match tail
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,@ef* ,tail))]
          [(mref ,v1 ,v2) (trivialize-call `(mref ,v1 ,v2))]
          [(alloc ,v) (trivialize-call `(alloc ,v))]
          [(,binop ,x ,y) (guard (binop? binop)) (trivialize-call `(,binop ,x ,y))]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,@rand*))]
          [,tr (guard (triv? tr)) tr]
          [,tail (error who "invalid Tail ~s" tail)]))
      (define (Pred pr)
        (match pr
          [(true) '(true)]
          [(false) '(false)]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,@ef* ,pr))]
          [(,relop ,x ,y) (guard (relop? relop)) (trivialize-call `(,relop ,x ,y))]
          [,pr (error who "invalid Pred ~s" pr)]))
      (define (Effect ef)
        (match ef
          [(nop) '(nop)]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[ef*] ... ,[ef]) (make-begin `(,@ef* ,ef))]
          [(set! ,var ,[Value -> val]) `(set! ,var ,val)]
          [(mset! ,v1 ,v2 ,v3) (trivialize-call `(mset! ,v1 ,v2 ,v3))]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,@rand*))]
          [,ef (error who "invalid Effect ~s" ef)]))
      (define (Value val)
        (match val
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(begin ,[Effect -> ef*] ... ,[val]) (make-begin `(,@ef* ,val))]
          [(mref ,v1 ,v2) (trivialize-call `(mref ,v1 ,v2))]
          [(alloc ,v) (trivialize-call `(alloc ,v))]
          [(,binop ,x ,y) (guard (binop? binop)) (trivialize-call `(,binop ,x ,y))]
          [,tr (guard (triv? tr)) tr]
          [(,rator ,rand* ...) (trivialize-call `(,rator ,@rand*))]
          [,val (error who "invalid Value ~s" val)]))
      (match bd
        [(locals (,local* ...) ,[Tail -> tail]) `(locals (,@local* ,@new-local*) ,tail)]
        [,bd (error who "invalid Body ~s" bd)]))
    (match x
      [(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...) ,[Body -> bd])
       `(letrec ([,label* (lambda (,@fml**) ,bd*)] ...) ,bd)]
      [,x (error who "invalid Program ~s" x)])))
