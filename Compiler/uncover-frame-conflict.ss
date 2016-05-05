(library (Compiler uncover-frame-conflict)
  (export uncover-frame-conflict)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define warn-if-dead-at-assignment (make-parameter #f))
  (define-who (uncover-frame-conflict x)
    (define (add-conflicts! ct lhs live*)
      (define (add-conflict! var1 var2)
        (let ([a (assq var1 ct)])
          (set-cdr! a (set-cons var2 (cdr a)))))
      (when (uvar? lhs)
        (for-each
         (lambda (live) (add-conflict! lhs live))
         live*))
      (for-each
       (lambda (live) (when (uvar? live) (add-conflict! live lhs)))
       live*))
    (define (Body x)
      (define call-live* '())
      (define (Triv x) (if (or (uvar? x) (frame-var? x)) `(,x) '()))
      (define (Effect* x live* ct)
        (match x
          [() live*]
          [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
          [,x (error who "invalid Effect* list ~s" x)]))
      (define (Effect x live* ct)
        (match x
          [(nop) live*]
          [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
          [(set! ,lhs ,rhs)
           (guard (or (uvar? lhs) (frame-var? lhs)) (not (memq lhs live*)))
           (when (warn-if-dead-at-assignment)
             (warning who "~s is not live at assignment ~s" lhs `(set! ,lhs ,rhs)))
           (Effect `(set! ,lhs ,rhs) (cons lhs live*) ct)]
          [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
           (let ([live* (difference live* `(,lhs))])
             (when (or (uvar? lhs) (frame-var? lhs))
               (add-conflicts! ct lhs live*))
             (union x-live* y-live* live*))]
          [(set! ,lhs ,rhs)
           (let ([live* (difference live* `(,lhs))])
             (when (or (uvar? lhs) (frame-var? lhs))
               (add-conflicts! ct lhs (remq rhs live*)))
             (union (Triv rhs) live*))]
          [(mset! ,[Triv -> x-live*] ,[Triv -> y-live*] ,[Triv -> z-live*])
           (union x-live* y-live* z-live* live*)]
          [(return-point ,rplab ,tail)
           (set! call-live* (union live* call-live*))
           (union live* (Tail tail ct))]
          [,x (error who "invalid Effect list ~s" x)]))
      (define (Pred x t-live* f-live* ct)
        (match x
          [(true) t-live*]
          [(false) f-live*]
          [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
          [(,relop ,[Triv -> x-live*] ,[Triv -> y-live*])
           (union t-live* f-live* x-live* y-live*)]
          [,x (error who "invalid Pred ~s" x)]))
      (define (Tail x ct)
        (match x
          [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
          [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
          [(,[Triv -> target-live*] ,live* ...)
           (union target-live*
                  (filter
                   (lambda (x) (or (frame-var? x) (uvar? x)))
                   live*))]
          [,x (error who "invalid Tail ~s" x)]))
      (match x
        [(locals (,uvar* ...) (new-frames (,nfv** ...) ,tail))
         (let ([ct (map (lambda (x) (cons x '())) uvar*)])
           (let ([uvar* (filter uvar? (Tail tail ct))])
             (unless (null? uvar*)
               (warning who "found variables ~s live on entry" uvar*)))
           (let ([spill* (filter uvar? call-live*)])
             `(locals (,(difference uvar* spill*) ...)
                (new-frames (,nfv** ...)
                  (spills ,spill*
                    (frame-conflict ,ct
                      (call-live (,call-live* ...) ,tail)))))))]
        [,x (error who "invalid Body ~s" x)]))
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))
