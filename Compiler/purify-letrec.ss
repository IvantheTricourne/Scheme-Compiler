(library (Compiler purify-letrec)
  (export purify-letrec)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (purify-letrec x)
    (define (Expr x)
      (match x
        ;;  S1
        ((letrec ([,var* ,[exp*]] ...) (assigned ,s ,[exp]))
         ;; S2
         (let-values ([(s l c) (purify (map list var* exp*) s)]) 
           ;; S3: you can make the following single pass
           (let* ([cvar* (map car c)] [cexp* (map cadr c)]
                  [tmp* (map (lambda (x) (unique-name 'x)) c)]
                  [sexp* (map (lambda (c t) `(set! ,c ,t)) cvar* tmp*)]
                  [letc (if (null? c) exp
                            (make-begin
                             `((let ([,tmp* ,cexp*] ...)
                                 (assigned ()
                                   ,(make-begin `(,@sexp*))))
                               ,exp)))]
                  [letl (if (null? l) letc
                            `(letrec ,l ,letc))]
                  [letw (if (null? cvar*) letl
                            `(let ([,cvar* (void)] ...)
                               (assigned (,@cvar*) ,letl)))]
                  [lets (if (null? s) letw
                            `(let ,s (assigned () letw)))])
             lets)))
        ;; AST
        ((let ([,var* ,[exp*]] ...) (assigned ,s ,[exp]))
         `(let ([,var* ,exp*] ...) (assigned ,s ,exp)))
        ((lambda (,fml* ...) (assigned ,s ,[exp]))
         `(lambda (,fml* ...) (assigned ,s ,exp)))
        ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
        ((begin ,[eff*] ... [eff]) (make-begin `(,@eff* eff)))
        ;; rator/rand
        ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
        ;; dream catcher
        (,x x)))
    (define (purify binds s)
      ;; -> Simple Binds, Lambda Binds, Complex Binds
      ;;
      ;; A simple bind: isn't a lambda bind or a complex bind
      ;; and is not bound to an assigned var
      ;;
      ;; A lambda bind: is a lambda expression at top-level
      ;; and is not associated with an assigned var
      ;;
      ;; A complex bind: is associated with an assigned var
      ;; and isn't simple or a lambda exp.
      (let loop ([ls binds] [s '()] [l '()] [c '()])
        (if (null? ls) (values s l c)
            (match (car ls)
              ((,var . ,bind) (guard (memq var s))
               (loop (cdr ls) s l (append c `((,var . ,bind)))))
              ((,var . (lambda ,b* ...))
               (loop (cdr ls) s (append l `((,var . (lambda ,@b*)))) c))
              ((,var . (,rat ,rand* ...))
               (loop (cdr ls) s l (append c `((,var . (,rat ,@rand*))))))
              (,x (loop (cdr ls) (append s `(,x)) l c))))))
    (Expr x)))
