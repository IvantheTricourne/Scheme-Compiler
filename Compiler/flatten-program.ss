(library (Compiler flatten-program)
  (export flatten-program)
  (import (chezscheme) (Framework helpers) (Framework match))
  (define-who flatten-program
    (define (Spit-code code)
      (match code
        ((set! ,var ,rhs) `((set! ,var ,rhs)))
        ((begin ,[eff*] ... ,[tail]) `(,@eff* ,@tail))
        ((,triv) `(((jump ,triv))))
        ((if ,rexp (,c) (,a)) `(((if ,rexp (jump ,c))) ((jump ,a))))))
    (define (Code first tail)
      (match tail
        (,x (guard (null? first)) (Spit-code x))
        ((,triv) (if (eqv? first triv) `(,first) `((jump ,triv) ,first)))
        ((begin ,[Spit-code -> eff*] ... ,[tail]) `(,eff* ... ... ,@tail))
        ((if ,rexp (,tlab) (,flab))
         (cond
          ((eqv? first flab) `((if ,rexp (jump ,tlab)) ,first))
          ((eqv? first tlab) `((if (not ,rexp) (jump ,flab)) ,first))
          (else `((if ,rexp (jump ,tlab)) (jump ,flab) ,first))))))
    (define (Code* rest tail*)
      (match (car tail*)
        (,x (guard (null? rest)) (Spit-code x))
        ((,triv)
         (let ((first (car rest)))
           (if (eqv? triv first) (cons `(,first) (Code* (cdr rest) (cdr tail*)))
               (cons `((jump ,triv) ,first) (Code* (cdr rest) (cdr tail*))))))
        ((begin ,[Spit-code -> eff*] ... ,[tail]) `(,@eff* ,@tail))
        ((if ,rexp (,tlab) (,flab))
         (let ((first (car rest)))
           (cond
            ((eqv? first flab)
             (cons `((if ,rexp (jump ,tlab)) ,first) (Code* (cdr rest) (cdr tail*))))
            ((eqv? first tlab)
             (cons `((if (not ,rexp) (jump ,flab)) ,first) (Code* (cdr rest) (cdr tail*))))
            (else
             (cons `((if ,rexp (jump ,tlab)) (jump ,flab) ,first) (Code* (cdr rest) (cdr tail*)))))))))
    (lambda (x)
      (match x
        ((letrec () ,code) (let ((to (Code '() code))) `(code ,to ... ...)))
        ((letrec ((,label* (lambda () ,code*)) ...) ,code)
         (let ((ti (Code* (cdr label*) code*)) (to (Code (car label*) code)))
           `(code ,to ...  ,ti ... ...)))))))
