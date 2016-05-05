(library (Compiler assign-new-frame)
  (export assign-new-frame)
  (import (chezscheme) (Framework helpers) (Framework match) (Compiler helpers))
  (define-who (assign-new-frame x)
    (define (Body b)
      (match b
        ((locals (,uvar* ...)
           (new-frames ,nfv*
             (locate ,binds
               (frame-conflict ,ct
                 (call-live ,lv* ,tail)))))
         (let* ([fs (frame-size lv* binds)]
                [fbinds (assign-nfv nfv* fs)]
                [tail^ (Tail fs tail)]
                [uvar*^ (difference uvar* (apply append nfv*))])
           `(locals (,@uvar*^)
              (ulocals ()
                (locate (,@binds ,@fbinds)
                  (frame-conflict ,ct ,tail^))))))))
    (define (Tail fs t)
      (match t
        ((if ,[(Pred fs) -> pred] ,[t1] ,[t2])
         `(if ,pred ,t1 ,t2))
        ((begin ,[(Effect fs) -> eff*] ... ,[tail])
         (make-begin `(,@eff* ,tail)))
        (,x x)))
    (define (Pred fs)
      (lambda (p)
        (match p
          ((if ,[p1] ,[p2] ,[p3])
           `(if ,p1 ,p2 ,p3))
          ((begin ,[(Effect fs) -> eff*] ... ,[pred])
           (make-begin `(,@eff* ,pred)))
          (,x x))))
    (define (Effect fs)
      (lambda (e)
        (match e
          ((if ,[(Pred fs) -> pred] ,[e1] ,[e2])
           `(if ,pred ,e1 ,e2))
          ((begin ,[eff*] ... ,[eff])
           (make-begin `(,@eff* ,eff)))
          ((return-point ,label ,tail)
           (format-return-point fs `(return-point ,label ,tail)))
          (,x x))))
    (define (format-return-point fs rp)
      (let ([shift (* fs word-size)])
        (make-begin
         `((set! ,frame-pointer-register (+ ,frame-pointer-register ,shift))
           ,rp
           (set! ,frame-pointer-register (- ,frame-pointer-register ,shift))))))
    (define (frame-size call-live* home*)
        (let f ([call-live* call-live*] [fs 0])
          (if (null? call-live*)
              fs
              (f (cdr call-live*)
                 (let ([idx (let ([x (car call-live*)])
                              (frame-var->index
                                (if (frame-var? x)
                                    x
                                    (cadr (assq x home*)))))])
                   (if (>= idx fs) (+ idx 1) fs))))))
    (define (assign-nfv nfv* fs)
      (let loop ([ls nfv*])
        (if (null? ls) '()
            (let floop ([fls (car ls)] [c fs] [binds '()])
              (cond
               ((null? fls) (append binds (loop (cdr ls))))
               (else (floop
                      (cdr fls)
                      (add1 c)
                      (cons (list (car fls) (index->frame-var c)) binds))))))))
    (match x
      ((letrec ((,lab (lambda () ,[Body -> body*])) ...) ,[Body -> body])
       `(letrec ((,lab (lambda () ,body*)) ...) ,body)))))
