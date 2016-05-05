(library (Compiler helpers)
  (export
    do-uncover-conflict
    ;; Unique
    all-unique
    ;;asocs
    map-values make-assoc make-assoc/uvar lengthen/fv lengthen/uvar
    set-boom set-pow get-bind
    ;;booleans
    triv? prim? val-prim? pred-prim? eff-prim?
    simple? bool? binop? relop? non-rp?
    not-null? call? int64!32? ur? imm?
    ;;others
    final-filter split-confs get-uvar-binds vec-offset)
  (import (chezscheme) (Framework match) (Framework helpers))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (carl f exp state)
    (letrec ([apply (lambda (exp id)
                      (if (null? exp)
                          (id state)
                          (apply (cdr exp)
                                 (lambda (s)
                                   (id (cons (f (car exp)) s))))))])
      (apply exp (lambda (x) x))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Association List Generators
  ;;
  ;; (make-assoc ls1 ls2) :: generates an association list with
  ;; members in ls1 and ls2. Ls1 members are treated as the
  ;; first value in (x . y) bind expressions. Primarily used
  ;; when associating uvars with registers and/or frame-vars
  ;;
  ;; (make-assoc/uvar ls1 ls2) :: viruatlly the same as make-assoc
  ;; except is lengthens using uvars
  ;;
  ;; (lengthen/* ls num) :: Used to lengthen ls2 in make-assoc with *
  ;; (shorten ls num) :: Used to shorten ls2 in make-assoc
  ;;
  ;; (set-boom assoc) :: returns a begin expression setting all
  ;; (x . y) to the equivalent (set! x y) expression
  ;;
  ;; (set-pow assoc) :: returns a begin expression setting all
  ;; (x . y) to the equivalent (set! y x) expression
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (all-unique fml*)
    (let loop ([ls fml*] [env '()])
      (if (null? ls) #t
          (and (not (member (car ls) env))
               (loop (cdr ls) (cons (car ls) env))))))
  (define (make-assoc ls1 ls2)
    (let ([l1 (length ls1)]
          [l2 (length ls2)])
      (cond
       ((null? ls1) '())
       ((= l1 l2) (map cons ls1 ls2))
       ((> l1 l2) (map cons ls1 (lengthen/fv ls2 (- l1 l2))))
       (else (map cons ls1 (shorten ls2 (- l2 l1)))))))
  (define (make-assoc/uvar ls1 ls2)
    (let ([l1 (length ls1)]
          [l2 (length ls2)])
      (cond
       ((null? ls1) '())
       ((= l1 l2) (map cons ls1 ls2))
       ((> l1 l2) (map cons ls1 (lengthen/uvar ls2 (- l1 l2))))
       (else (map cons ls1 (shorten ls2 (- l2 l1)))))))
  (define (lengthen/fv ls by)
    (let loop ([st (sub1 by)] [ans '()])
      (if (< st 0) (append ls ans)
          (loop (sub1 st) (cons (index->frame-var st) ans)))))
  (define (lengthen/uvar ls by)
    (let loop ([st 0] [ans '()])
      (if (= st by) (append ls ans)
          (loop (add1 st) (cons (unique-name 'nfv) ans)))))
  (define (shorten ls by)
    (if (zero? by) '() (cons (car ls) (shorten (cdr ls) (sub1 by)))))
  (define (set-boom assoc)
    (if (null? assoc) '()
        (make-begin (map (lambda (x) `(set! ,(car x) ,(cdr x))) assoc))))
  (define (set-pow assoc)
    (if (null? assoc) '()
        (make-begin
         (let loop ([ls assoc] [ans '()])
           (if (null? ls) ans
               (loop (cdr ls) (cons `(set! ,(cdar ls) ,(caar ls)) ans)))))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Predicates
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (call? x) (or (list? x) (label? x) (uvar? x)))
  (define (not-null? x)
    (not (null? x)))
  (define (relop? r) (memq r '(< > <= >= =)))
  (define (binop? b) (memq b '(+ - * logand logor sra)))
  (define (non-rp? exp)
    (or (triv? exp)
        (match exp
          ((mref . ,d) #t)
          ((alloc . ,d) #t)
          ((,b . ,d) (guard (binop? b)) `(,b . ,d))
          (,else #f))))
  (define (triv? t) (or (uvar? t) (int64? t) (label? t)))
  (define (bool? b) (or (eq? 'true b) (eq? 'false b)))
  (define (prim? p)
    (or (label? p) (binop? p) (relop? p) (uvar? p)
        (memq p (list 'cons 'car 'cdr 'set-car! 'set-cdr!
                      'procedure-set! 'procedure-code
                      'procedure-ref 'make-procedure 'procedure?
                      'make-vector 'vector-ref 'vector-length
                      'vector-set! 'eq? 'null? 'boolean?
                      'fixnum? 'pair? 'vector? 'if 'begin))))
  (define (val-prim? p)
    (or (uvar? p) (label? p) (binop? p) (list? p)
        (memq p (list 'cons 'car 'cdr 'vector-ref
                      'vector-length 'make-vector
                      'procedure-code 'procedure-ref 'make-procedure))))
  (define (pred-prim? p)
    (or (relop? p) (memq p (list 'eq? 'null? 'boolean? 'fixnum? 'pair? 'vector? 'procedure?))))
  (define (eff-prim? p)
    (or (uvar? p) (label? p) (eq? p 'set!) (list? p)
        (memq p (list 'set-car! 'set-cdr! 'vector-set! 'procedure-set!))))
  (define (simple? exp)
    (match exp
      ((,r ,x ,y) (guard (and (triv? x) (triv? y))) #t)
      ((if ,[p] ,[c] ,[a]) #t)
      ((begin ,[eff*] ... ,[tail]) #t)
      ((alloc ,[x]) #t)
      ((set! ,[lhs] ,[rhs]) #t)
      ((mset! ,[x] ,[y] ,[z]) #t)
      (,x (guard (triv? x)) #t)
      (,x #f)))
  (define (int64!32? x) (and (not (int32? x)) (int64? x)))
  (define (ur? x) (or (register? x) (uvar? x)))
  (define (imm? c) (or (number? c) (boolean? c) (null? c)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Others
  ;;
  ;; (get-bind uvar assoc) :: returns the bind (if any) of the
  ;; given triv within assoc
  ;;
  ;; (final-filter ls) :: removes all '() in a given, possibly
  ;; nested list. Hackjob. Used in remove-complex-opera*
  ;;
  ;; (split-confs spilled-var conf-graph) ::
  ;; takes a spilled var and the conf-graph within the Body and
  ;; returns two values as lists: Uvar confs (uct) and
  ;; frame-var confs (fct)
  ;;
  ;; (get-uvar-binds uct binds) :: returns all binds of uvars
  ;; in binds; used in get-free-fv to determine which frame-var
  ;; to assign to a spilled-var
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define vec-offset (- disp-vector-length tag-vector))
  (define (get-bind triv assoc) (let ([b (assv triv assoc)]) (if b (cdr b) triv)))
  (define (final-filter ls)
    (cond
     ((null? ls) '())
     ((list? (car ls)) (append (final-filter (car ls)) (final-filter (cdr ls))))
     ((uvar? (car ls)) (cons (car ls) (final-filter (cdr ls))))
     (else (final-filter (cdr ls)))))
  (define (split-confs var ct)
    (let loop ([ls (cdr (assq var ct))] [uvl '()] [fvl '()])
      (match ls
        (() (values uvl fvl))
        ((,a . ,d) (guard (frame-var? a)) (loop d uvl (cons a fvl)))
        ((,a . ,d) (loop d (cons a uvl) fvl)))))
  (define (get-uvar-binds uct binds)
    (map (lambda (x) (let ([b (assq x binds)]) (if b (cadr b) x))) uct))
  (define (Proc-labels label*)
    (match label*
      (,list (guard (list? list))
             (let* ((sl (map (lambda (x) (string->number (extract-suffix x))) label*))
                    (ms (fold-right max 0 sl)))
               (unique-name-count ms)
               list))
      (,else else)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (map-values Proc ls) :: similar to map, except returns
  ;; two lists as the two return values for (values * *)
  ;;
  ;; !!!ONLY WORKS WITH PROCS THAT RETURN TWO VALUES!!!
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (map-values proc ls)
    (cond
     ((null? ls) (values '() '()))
     (else (let-values ([(pa1 pa2) (proc (car ls))]
                        [(pd1 pd2) (map-values proc (cdr ls))])
             (values (cons pa1 pd1) (cons pa2 pd2))))))

  (define warn-if-dead-at-assignment (make-parameter #f))

  (define do-uncover-conflict
    (lambda (tail uvar* who fixed?)
      (define add-conflicts!
        (lambda (ct lhs live*)
          (define add-conflict!
            (lambda (var1 var2)
              (let ([a (assq var1 ct)])
                (set-cdr! a (set-cons var2 (cdr a))))))
          (when (uvar? lhs)
            (for-each
             (lambda (live) (add-conflict! lhs live))
             live*))
          (for-each
           (lambda (live) (when (uvar? live) (add-conflict! live lhs)))
           live*)))
      (define Triv (lambda (x) (if (or (uvar? x) (fixed? x)) `(,x) '())))
      (define Effect*
        (lambda (x live* ct)
          (match x
            [() live*]
            [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
            [,x (error who "invalid Effect* list ~s" x)])))
      (define Effect
        (lambda (x live* ct)
          (match x
            [(nop) live*]
            [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
            [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
            [(set! ,lhs ,rhs)
             (guard (or (uvar? lhs) (fixed? lhs)) (not (memq lhs live*)))
             (when (warn-if-dead-at-assignment)
               (warning who "~s is not live at assignment ~s" lhs
                        `(set! ,lhs ,rhs)))
             (Effect `(set! ,lhs ,rhs) (cons lhs live*) ct)]
            [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
             (let ([live* (difference live* `(,lhs))])
               (when (or (uvar? lhs) (fixed? lhs))
                 (add-conflicts! ct lhs live*))
               (union x-live* y-live* live*))]
            [(set! ,lhs ,var)
             (let ([live* (difference live* `(,lhs))])
               (when (or (uvar? lhs) (fixed? lhs))
                 (add-conflicts! ct lhs (remq var live*)))
               (union (Triv var) live*))]
            [,x (error who "invalid Effect list ~s" x)])))
      (define Pred
        (lambda (x t-live* f-live* ct)
          (match x
            [(true) t-live*]
            [(false) f-live*]
            [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
            [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
            [(,relop ,[Triv -> x-live*] ,[Triv -> y-live*])
             (union t-live* f-live* x-live* y-live*)]
            [,x (error who "invalid Pred ~s" x)])))
      (define Tail
        (lambda (x ct)
          (match x
            [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
            [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
            [(,[Triv -> target-live*] ,live* ...)
             (union target-live*
                    (filter
                     (lambda (x) (or (fixed? x) (uvar? x)))
                     live*))]
            [,x (error who "invalid Tail ~s" x)])))
      (let ([ct (map (lambda (x) (cons x '())) uvar*)])
        (let ([uvar* (filter uvar? (Tail tail ct))])
          (unless (null? uvar*)
            (warning who "found variables ~s live on entry" uvar*)))
        ct)))
  )
