(library (Compiler everybody-home?)
  (export everybody-home?)
  (import (chezscheme) (Framework helpers) (Framework match))
  (define-who (everybody-home? x)
    (define (all-home? body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]))
    (match x
      [(letrec ([,label* (lambda () ,body*)] ...) ,body) (andmap all-home? `(,body ,@body*))])))
