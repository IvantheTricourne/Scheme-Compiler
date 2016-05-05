(library (Compiler pre-assign-frame)
  (export pre-assign-frame)
  (import (chezscheme) (Framework helpers) (Framework match) (Compiler helpers))
  (define-who (pre-assign-frame x)
    (define (Body b)
      (match b
        ((locals (,uvar* ...)
           (new-frames (,frame* ...)
             (spills ,suvar*
               (frame-conflict ,ct
                 (call-live (,lv* ...) ,tail)))))
         (let loop ([ls suvar*] [binds (map list suvar* suvar*)])
           (if (null? ls)
               `(locals (,@uvar*)
                  (new-frames (,@frame*)
                    (locate ,binds
                      (frame-conflict ,ct
                        (call-live (,@lv*) ,tail)))))
               (let-values ([(uct fct) (split-confs (car ls) ct)])
                 (let ([binds^ (get-free-fv (car ls) uct fct binds)])
                   (loop (cdr ls) binds^))))))))
    (define (get-free-fv var uct fct binds)
      (let loop ([cnt 0])
        (let ([fv (index->frame-var cnt)])
          (cond
           ((or (memq fv fct) (memq fv (get-uvar-binds uct binds))) (loop (add1 cnt)))
           (else (map (lambda (x) (if (eq? var (car x)) (list var fv) x)) binds))))))
    (match x
      ((letrec ((,lab (lambda () ,[Body -> body*])) ...) ,[Body -> body])
       `(letrec ((,lab (lambda () ,body*)) ...) ,body))
      (,x (error who "primary did not match")))))
