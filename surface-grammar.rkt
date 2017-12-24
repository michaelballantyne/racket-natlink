#lang racket

(require (prefix-in c: "core-grammar.rkt"))

(provide grammar or : ! ? + actions)

(define-syntax-rule
  (grammar (exports ...)
           [rulename expr] ...)
  (c:grammar '(dgndictation) '(exports ...)
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

(define-syntax-rule (actions [rule expr] ...)
  (or
   (! rule
      expr)
   ...))

(module+ test
  (grammar (foo)
           [foo (or
                 (! (: "hello" "world")
                    (lambda _ (displayln "it worked! Hello world")))
                 (! "Hello"
                    (lambda _ (displayln "Hello, I guess"))))]))
          