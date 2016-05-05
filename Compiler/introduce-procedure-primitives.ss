(library (Compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (introduce-procedure-primitives x)
    ;; var-ref is used on every uvar within the expression body.
    ;; in the event that the uvar is a free-var (scoped by a bind-free)
    ;; it is converted into a procedure-ref expression
    (define (var-ref var env)
      ;; -> '(procedure-ref cp-var cp-offset)
      (cond
       ((assq var env) =>
        (lambda (x) `(procedure-ref ,(cadr x) (quote ,(caddr x)))))
       (else var)))
    ;; Intro needs to be curried to hold an environment
    ;; It starts out null, but is extended ONLY by the bind-free
    ;; expression contained immediately after a lambda **
    (define (intro env)
      (lambda (x)
        (match x
          ((letrec ([,var* ,[lam*]] ...) ,[exp])
           `(letrec ([,var* ,lam*] ...) ,exp))
          ((lambda ,fml (bind-free ,[Free -> fr] ,exp)) ;; **
           `(lambda ,fml ,((intro (append fr env)) exp))) ;; ext-env
          ((let ([,var* ,[exp*]] ...) ,[exp])
           `(let ([,var* ,exp*] ...) ,exp))
          ((begin ,[exp*] ... ,[exp])
           (make-begin `(,@exp* ,exp)))
          ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
          ((closures (,[(Closure env) -> mproc* proc-set*] ...) ,[exp])
           `(let (,@mproc*) ,(make-begin `(,proc-set* ... ... ,exp))))
          ((,rat ,[rand*] ...) (guard (uvar? rat))
           `((procedure-code ,((intro env) rat)) ,@rand*))
          ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
          (,uv (guard (uvar? uv)) (var-ref uv env))
          (,x x))))
    ;; Bind-free handler (sets up environment extension)
    ;; -> '([free-var cp-var cp-offset]*)
    (define (Free fr)
      (let ([parent (car fr)])
        (let loop ([ls (cdr fr)] [i 0])
          (if (null? ls) '()
              (cons `(,(car ls) ,parent ,i)
                    (loop (cdr ls) (add1 i)))))))
    ;; Closure form handler
    ;; -> (values (uvar (make-procedure label arg-num)) Proc-set*)
    (define (Closure env) 
      (lambda (clos)
        (match clos
          ((,uvar ,lab ,arg* ...)
           (values
            `(,uvar (make-procedure ,lab (quote ,(length arg*))))
            (let loop ([proc-set* arg*] [i 0])
              ;; -> '([procedure-set! uvar uv-offset Proc-arg]*)
              (if (null? proc-set*) '()
                  (cons
                   `(procedure-set! ,uvar (quote ,i) ,(var-ref (car proc-set*) env))
                   (loop (cdr proc-set*) (add1 i))))))))))
    ((intro '()) x)))
