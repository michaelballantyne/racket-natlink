#lang racket

(require (prefix-in c: "core/core-grammar.rkt"))

(provide grammar or : ! ? + action-mapping value-mapping)

(define-syntax-rule
  (grammar (exports ...)
           [rulename expr] ...)
  (c:grammar '(dgndictation dgnwords) '(exports ...)
             (let ([rulename 'rulename] ...)
               (make-immutable-hash (list (cons rulename (lift expr)) ...)))))

(define (lift atom)
  (cond
    [(symbol? atom) (c:rule-reference atom)]
    [(string? atom) (c:word atom)]
    [else atom]))

(define (or . args)
  (c:alternative (map lift args)))

(define (: . args)
  (c:sequence (map lift args)))

(define (! g f)
  (c:action (lift g) f))

(define (? g)
  (c:optional (lift g)))

(define (+ g)
  (c:repeat (lift g)))

(define-syntax-rule (action-mapping [rule expr] ...)
  (or
   (! rule
      expr)
   ...))

(define-syntax-rule (value-mapping [rule expr] ...)
  (or
   (! rule
      (lambda (ignore) expr))
   ...))

(module+ test
  (grammar (foo)
           [foo (or
                 (! (: "hello" "world")
                    (lambda _ (displayln "it worked! Hello world")))
                 (! "Hello"
                    (lambda _ (displayln "Hello, I guess"))))]))
          