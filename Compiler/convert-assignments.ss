(library (Compiler convert-assignments)
  (export convert-assignments)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who (convert-assignments x)
    ;; Initiate: Dirty Tree Walk
    (define (Exp env)
      (lambda (x)
        (match x
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; To do:
          ;;
          ;; **This needs to be done right after let or lambda
          ;; - (assigned (x ...) expr) -> (let ((x (cons t (void))) ...) expr)
          ;;
          ;; **These last two can be done within Exp
          ;; (extend env when assigned is found)
          ;;
          ;; - Replace all "assigned-var x" references to (car x)
          ;; - Replace all (set! x e) exp to (set-car! x e)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((letrec ([,var* ,[exp*]] ...) ,[exp])
           `(letrec ([,var* ,exp*] ...) ,exp))
          ((let ([,var* ,[exp*]] ...)
             (assigned ,s ,[(Exp (append s env)) -> exp]))
           (let-values ([(lbind* bind) (unassign var* s)])
             `(let ([,lbind* ,exp*] ...)
                ,(if (null? bind) exp `(let ,bind ,exp)))))
          ((lambda (,fml* ...)
             (assigned ,s ,[(Exp (append s env)) -> exp]))
           (let-values ([(fbind* bind) (unassign fml* s)])
             `(lambda (,@fbind*)
                ,(if (null? bind) exp `(let ,bind ,exp)))))
          ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
          ((begin ,[eff*] ... ,[eff]) (make-begin `(,@eff* ,eff)))
          ((set! ,uvar ,[exp]) (guard (memq uvar env))
           `(set-car! ,uvar ,exp))
          ;; rator/rand
          ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
          ;; dreamcatcher
          (,x (guard (memq x env)) `(car ,x))
          (,x x))))
    (define (unassign var* svar*)
      ;; -> var*^ and bind
      ;; var*^ is the updated var* for the given formal args
      ;; bind is the generated list of svar* bound to (cons t (void))
      (if (null? svar*) (values var* svar*)
          (let ([env (map (lambda (s) (cons s (unique-name 'x))) svar*)])
            (values
             (map (lambda (x) (if (assq x env) (cdr (assq x env)) x)) var*)
             (map (lambda (x) (list (car x) `(cons ,(cdr x) (void)))) env)))))
    ((Exp '()) x)))
