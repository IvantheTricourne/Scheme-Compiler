(library (Compiler assign-frame)
  (export assign-frame)
  (import (chezscheme) (Framework helpers) (Framework match) (Compiler helpers))
  (define-who (assign-frame x)
    (define (Body b)
      (match b
        ((locals (,uvar* ...)
           (ulocals (,uuvar* ...)
             (spills ,suvar*
               (locate ,binds*
                 (frame-conflict ,ct ,tail)))))
         (let loop ([ls suvar*] [binds*^ binds*])
           (if (null? ls)
               `(locals (,@uvar*)
                  (ulocals (,@uuvar*)
                    (locate ,binds*^
                      (frame-conflict ,ct ,tail))))
               (let-values ([(uct fct) (split-confs (car ls) ct)])
                 (let ([binds*^^ (get-free-fv (car ls) uct fct binds*^)])
                   (loop (cdr ls) binds*^^))))))
        ((locate ,binds* ,tail) `(locate ,binds* ,tail))))
    (define (get-free-fv var uct fct binds)
      (let loop ([cnt 0])
        (let ([fv (index->frame-var cnt)])
          (cond
           ((or (memq fv fct) (memq fv (get-uvar-binds uct binds))) (loop (add1 cnt)))
           (else (append binds (list (list var fv))))))))
    (match x
      ((letrec ((,label* (lambda () ,[Body -> body*])) ...) ,[Body -> body])
       `(letrec ((,label* (lambda () ,body*)) ...) ,body)))))
