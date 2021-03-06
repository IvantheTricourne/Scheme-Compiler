(library (Compiler parse-scheme)
  (export parse-scheme)
  (import (chezscheme) (Framework match) (Framework helpers) (Compiler helpers))
  (define-who (parse-scheme x)
    (define primitives
      '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
        (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
        (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
        (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
        (set-cdr! . 2) (vector? . 1) (vector-length . 1)
        (vector-ref . 2) (vector-set! . 3) (void . 0)))
    (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (error who "integer ~s is out of fixnum range" x)))))
    (define (datum? x)
      (or (constant? x)
          (if (pair? x)
              (and (datum? (car x)) (datum? (cdr x)))
              (and (vector? x) (andmap datum? (vector->list x))))))
    (define (Var env)
      (lambda (x)
        (unless (assq x env) (error who "Unbound variable ~s" x))
        (cdr (assq x env))))
    (define (Program x)
      (define (Expr env)
        (lambda (x)
          (match x
            ((void) `(void)) ;; Because
            (,constant (guard (constant? constant)) `(quote ,constant))
            (,var (guard (symbol? var)) ((Var env) var))
            ;; Anti-clobberer
            ((,rat ,[rand*] ...) (guard (assq rat env))
             `(,((Var env) rat) ,@rand*))
            ;; Things that can be clobbered
            ((quote ,datum)
             (unless (datum? datum)
               (error who "Invalid datum ~s" datum))
             `(quote ,datum))
            ((if ,[p] ,[c] ,[a]) `(if ,p ,c ,a))
            ((if ,[p] ,[c]) `(if ,p ,c (void)))
            ((begin ,[exp*] ... ,[exp]) (make-begin `(,@exp* ,exp)))
            ((set! ,var ,[exp])
             (unless (symbol? var)
               (error who "Invalid variable in set! ~s" var))
             `(set! ,((Var env) var) ,exp))
            ((and ,exp* ...)
             (case (length exp*)
               ((0) '#t)
               ((1) ((Expr env) (car exp*)))
               (else `(if ,((Expr env) (car exp*))
                          ,((Expr env) `(and ,(cdr exp*) ...))
                          '#f))))
            ((or ,exp* ...)
             (case (length exp*)
               ((0) '#f)
               ((1) ((Expr env) (car exp*)))
               (else
                (let ([tmp (unique-name 't)])
                  `(let ([,tmp ,((Expr env) (car exp*))])
                     (if ,tmp ,tmp
                         ,((Expr env) `(or ,(cdr exp*) ...))))))))
            ((not ,exp* ...)
             (unless (null? (cdr exp*))
               (error who "Too many arguments for not ~s" exp*))
             `(if ,((Expr env) exp*) ... '#f '#t))
            ;; Things with scope
            ((lambda (,fml* ...) ,exp ,exp* ...)
             (unless (all-unique fml*)
               (error who "Invalid formals in Lambda ~s" fml*))
             (unless (andmap symbol? fml*)
               (error who "Invalid Lambda formal in ~s" fml*))
             (let* ([ufml* (map (lambda (x) (unique-name x)) fml*)]
                    [env^ (append (map cons fml* ufml*) env)])
               `(lambda (,@ufml*)
                  ,(if (null? exp*) ((Expr env^) exp)
                       (make-begin
                        `(,((Expr env^) exp)
                          ,((Expr env^) exp*) ...))))))
            ((let ([,var* ,bind*] ...) ,exp ,exp* ...)
             (unless (all-unique var*)
               (error who "Non-unique var found in Let ~s" var*))
             (unless (andmap symbol? var*)
               (error who "Invalid Let variable in ~s" var*))
             (let* ([uvar* (map (lambda (x) (unique-name x)) var*)]
                    [env^ (append (map cons var* uvar*) env)]
                    [bind (map list uvar* (map (Expr env) bind*))])
               `(let ,bind
                  ,(if (null? exp*) ((Expr env^) exp)
                       (make-begin
                        `(,((Expr env^) exp)
                          ,((Expr env^) exp*) ...))))))
            ((letrec ([,var* ,bind*] ...) ,exp ,exp* ...)
             (unless (all-unique var*)
               (error who "Non-unique var found in Letrec ~s" var*))
             (unless (andmap symbol? var*)
               (error who "Invalid Letrec variable in ~s" var*))
             (let* ([uvar* (map (lambda (x) (unique-name x)) var*)]
                    [env^ (append (map cons var* uvar*) env)]
                    [bind (map list uvar* (map (Expr env^) bind*))])
               `(letrec ,bind
                  ,(if (null? exp*) ((Expr env^) exp)
                       (make-begin
                        `(,((Expr env^) exp)
                          ,((Expr env^) exp*) ...))))))
            ;; rator/rand
            ((,rat ,rand* ...) (guard (assq rat primitives))
             (unless (= (length rand*) (cdr (assq rat primitives)))
               (error who "Error: Arity mismatch for ~s, given ~s" rat rand*))
             `(,rat ,((Expr env) rand*) ...))
            ((,[rat] ,[rand*] ...) `(,rat ,@rand*))
            ;; death catcher
            (,x (error who "Invalid Expression ~s" x)))))
      ((Expr '()) x))
    (Program x)))
