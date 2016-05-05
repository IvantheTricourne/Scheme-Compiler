(library (Compiler impose-calling-conventions)
  (export impose-calling-conventions)
  (import (chezscheme) (Framework match) (Framework helpers) (Compiler helpers))
  (define-who (impose-calling-conventions x)
    (define nf '())
    (define (set-rp rp) `(set! ,rp ,return-address-register))
    (define (set-ra rp) `(set! ,return-address-register ,rp))
    (define (Expr rp)
      (lambda (exp)
        (make-begin `((set! ,return-value-register ,exp)
                      (,rp ,frame-pointer-register
                           ,allocation-pointer-register
                           ,return-value-register)))))
    (define (make-return-point triv triv*)
      (let* ([assoc (make-assoc/uvar triv* parameter-registers)]
             [binds (map cdr assoc)] [nfv (filter uvar? binds)]
             [lab (unique-label 'rp)]
             [tail (filter (lambda (x) (not (null? x)))
                           (make-begin
                            `(,(set-pow assoc) ,(set-ra lab)
                              (,triv ,return-address-register ,frame-pointer-register
                                     ,allocation-pointer-register ,@binds))))])
        (set! nf (cons nfv nf))
        `(return-point ,lab ,tail)))
    (define (Body x)
      (match x
        ((lambda (,uvar* ...) (locals (,local* ...) ,tail))
         (set! nf '())
         (let* ([assoc (make-assoc uvar* parameter-registers)]
                [set!* (set-boom assoc)] [rp (unique-name 'rp)]
                [b^ ((Tail rp) tail)])
           `(lambda ()
              (locals (,rp ,@uvar* ,@local* ,nf ... ...)
                (new-frames (,@nf)
                  ,(if (null? set!*)
                       (make-begin `(,(set-rp rp) ,b^))
                       (make-begin `(,(set-rp rp) ,set!* ,b^))))))))
        ((locals (,local* ...) ,tail)
         (set! nf '())
         (let* ([rp (unique-name 'rp)] [b^ ((Tail rp) tail)])
           `(locals (,rp ,@local* ,nf ... ...)
              (new-frames (,@nf)
                ,(make-begin `(,(set-rp rp) ,b^))))))))
    (define (Tail rp)
      (lambda (x)
        (match x
          ((if ,[(Pred rp) -> p] ,[(Tail rp) -> c] ,[(Tail rp) -> a]) `(if ,p ,c ,a))
          ((begin ,[(Effect rp) -> eff*] ... ,[(Tail rp) -> tail]) (make-begin `(,@eff* ,tail)))
          ((,op ,x ,y) (guard (or (binop? op) (relop? op))) ((Expr rp) `(,op ,x ,y)))
          ((mref ,x ,y) ((Expr rp) `(mref ,x ,y)))
          ((alloc ,x) ((Expr rp) `(alloc ,x)))
          ((,triv ,triv* ...)
           (let* ([assoc (make-assoc triv* parameter-registers)]
                  [set!* (set-pow assoc)] [triv^ (get-bind triv assoc)]
                  [tlist (map (lambda (x) (get-bind x assoc)) triv*)]
                  [texp (filter (lambda (x) (not (null? x)))
                                `(,set!* ,(set-ra rp)
                                         (,triv^
                                          ,return-address-register ,frame-pointer-register
                                          ,allocation-pointer-register ,@tlist)))])
             (make-begin texp)))
          (,x ((Expr rp) x)))))
    (define (Pred rp)
      (lambda (x)
        (match x
          ((if ,[p1] ,[p2] ,[p3]) `(if ,p1 ,p2 ,p3))
          ((begin ,[(Effect rp) -> eff*] ... ,[p]) (make-begin `(,@eff* ,p)))
          (,x x))))
    (define (Effect rp)
      (lambda (x)
        (match x
          ((if ,[(Pred rp) -> p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2))
          ((begin ,[eff*] ... ,[eff]) (make-begin `(,@eff* ,eff)))
          ((set! ,lhs ,rhs) (guard (non-rp? rhs)) `(set! ,lhs ,rhs))
          ((mset! . ,args) `(mset! . ,args))
          ((set! ,uvar (,triv ,triv* ...))
           (make-begin `(,(make-return-point triv triv*)
                         (set! ,uvar ,return-value-register))))
          ((nop) `(nop))
          ((,triv ,triv* ...) (make-return-point triv triv*)))))
    (match x
      ((letrec ((,label* ,[Body -> progs*]) ...) ,prog)
       (let ([prog^ (Body prog)]) (set! nf '()) `(letrec ((,label* ,progs*) ...) ,prog^))))))
