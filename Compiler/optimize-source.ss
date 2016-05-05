;;; Constant Folding

;;; Copy propagation
;;;
;;;  What I'll need to do this for this is take every
;;;  let expression and pass it's bindings as an a-list env
;;; 
;;;  Then replace all occurances of each var in its body-exp
;;;  with the proper binding.
;;;
;;;  This will go in tandem with the following transformation

;;; Useless-code elimination
;;;
;;; This happens after/during the transformation of Copy propagation
;;;
;;; Let will be handled when Copy propagation happens
;;;
;;; It will also need to process begins differently. With each of
;;; it's Effects, I need to make sure that every one of them are
;;; actually side-effect procedure calls (after Copy Propagation)

;;; Dead-code elimination
;;;
;;; Obviously related to "if" expressions and its branches
;;; Something is dead when: it cannot be reached
;;;
;;; Thus a branch is dead whenever the Predicate contained within the if
;;; evaluates to #t or #f.
;;;
;;; The question now is "How/When do I evaluate the predicate?"
;; (library (Compiler optimize-source)
;;   (export optimize-source)
;;   (import (chezscheme)
;;           (Framework helpers)
;;           (Framework match))

;;   (define-who optimize-source
;;     (define Expr
;;       (lambda (exp)
;;         (match exp
;;           ((* (quote ,x) (quote ,y)) (guard (constant? x) (constant? y))
;;            (if (fixnum-range? (* x y))
;;                `(quote ,(* x y))
;;                exp))
;;           ((,op (quote ,x) (quote ,y)) (guard (and (prim? op) (constant? x)
;;                                                    (constant? y)))
;;            `(quote ,(eval `(,op ,x ,y))))
;;           (,x x))))

;;     (define prim?
;;       (lambda (x)
;;         (or (memv x '(+ - * < <= = >= >)))))
;;     (define (constant? x)
;;       (and (and (integer? x) (exact? x))
;;            (fixnum-range? x)))
;;     (lambda (exp)
;;       (Expr exp))))
(library (Compiler optimize-source)
  (export optimize-source)
  (import (chezscheme) (Framework helpers) (Framework match))
  (define-who (optimize-source x)
    (define (prim? x) (memq x '(+ - < <= = >= >)))
    (define (constant? x)
      (or (and (and (integer? x) (exact? x)) (fixnum-range? x))
          (match x ((quote ,x) (constant? x)) (,x #f))))
    (define (Var env)
      (lambda (x)
        (cond
         ((assq x env) =>
          (lambda (x)
            (values ((Var env) (cadr x)) )))
         (else (values x '() #t)))))
    (define (Expr env)
      (lambda (x)
        (match x
          ;((letrec ... ...))
          ;((begin ... ...))
          ((let ([,var* ,[exp* ref* d*]] ...)
             ,[(Expr (append
                      (filter (lambda (x) (or (constant? x) (uvar? x)))
                              (map list var* exp*)) env)) -> exp ref dead?])
           `(let ,(foldr (lambda (x r) (if (memq x ref*))) ))
           (if (andmap (lambda (x) (memq x ref)) var*)
               ))
          ((* (quote ,x) (quote ,y))
           (guard (fixnum-range? (* x y)))
           (values `(quote ,(* x y)) '() #t))
          ((,op (quote ,x) (quote ,y))
           (guard (and (prim? op) (constant? x) (constant? y)))
           (values `(quote ,(eval `(,op ,x ,y))) '() #t))
          ;; rat/rand
          ((,[rat ref dead] ,[rand* ref* dead*] ...)
           (values `(,rat ,rand* ...)
                   (append ref ref*) ;; this isn't right
                   #f))
          ;; dream catcher
          (,[(Var env) -> x var dead?]
           (values x var dead?)))))
    ((Expr '()) x)))
