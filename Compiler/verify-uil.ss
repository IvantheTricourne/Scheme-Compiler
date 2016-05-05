(library (Compiler verify-uil)
  (export verify-uil)
  (import (chezscheme) (Framework match) (Framework helpers))
  (define-who verify-uil
    (define binops '(+ - * logand logor sra))
    (define relops '(< > <= >= =))
    (define (verify-x-list x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*)))))))
    (define (Triv label* uvar*)
      (lambda (t)
        (unless (or (label? t) (uvar? t) (and (integer? t) (exact? t)))
          (error who "invalid Triv ~s" t))
        (when (and (integer? t) (exact? t))
          (unless (int64? t)
            (error who "integer out of 64-bit range ~s" t)))
        (when (uvar? t)
          (unless (memq t uvar*)
            (error who "reference to unbound uvar ~s" t)))
        (when (label? t)
          (unless (memq t label*)
            (error who "unbound label ~s" t)))
        (values)))
    (define (Value label* uvar*)
      (lambda (val)
        (match val
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(sra ,[] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))
           (values)]
          [(alloc ,[(Value label* uvar*) ->])  (values)]
          [(mref ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (values)]
          [(,binop ,[] ,[]) (guard (memq binop binops)) (values)]
          [(,[] ,[] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)])))
    (define (Effect label* uvar*)
      (lambda (ef)
        (match ef
          [(nop) (values)]
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[] ... ,[]) (values)]
          [(set! ,var ,[(Value label* uvar*) ->])
           (unless (memq var uvar*)
             (error who "assignment to unbound var ~s" var))
           (values)]
          [(mset! ,[(Value label* uvar*) ->]
                  ,[(Value label* uvar*) ->]
                  ,[(Value label* uvar*) ->])
           (values)]
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,ef (error who "invalid Effect ~s" ef)])))
    (define (Pred label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (values)]
          [(false) (values)]
          [(if ,[] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(,relop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (memq relop relops))
           (values)]
          [,pr (error who "invalid Pred ~s" pr)])))
    (define (Tail label* uvar*)
      (lambda (tail)
        (match tail
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(sra ,[(Value label* uvar*) ->] ,y)
           (unless (uint6? y)
             (error who "invalid sra operand ~s" y))
           (values)]
          [(alloc ,[(Value label* uvar*) ->])  (values)]
          [(mref ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (values)]
          [(,binop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (memq binop binops))
           (values)]
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)])))
    (define (Body label* fml*)
      (lambda (x)
        (match x
          [(locals (,local* ...) ,tail)
           (let ([uvar* `(,fml* ... ,local* ...)])
             (verify-x-list uvar* uvar? 'uvar)
             ((Tail label* uvar*) tail)
             (values))]
          [,x (error who "invalid Body ~s" x)])))
    (define (Lambda label*)
      (lambda (x)
        (match x
          [(lambda (,fml* ...) ,[(Body label* fml*) ->]) (values)]
          [,x (error who "invalid Lambda ~a" x)])))
    (lambda (x)
      (match x
        [(letrec ([,label* ,[(Lambda label*) ->]] ...) ,[(Body label* '()) ->])
         (verify-x-list label* label? 'label)]
        [,x (error who "invalid Program ~s" x)])
      x)
    )
  )

