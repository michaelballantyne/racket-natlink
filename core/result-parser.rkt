#lang racket

(require megaparsack
         data/monad
         data/applicative
         "core-grammar.rkt")

(provide make-result-parser parse-words expr->parser)

(define (word/p str)
  (satisfy/p (lambda (v) (equal? v str))))

(define empty-srcloc (srcloc #f #f #f #f #f))

(define (words->parser-tokens words)
  (map (lambda (word) (syntax-box word empty-srcloc)) words))

(define (parse-words g words)
  (parse g (words->parser-tokens words)))

(module+ test
  (require data/either rackunit)

  (check-match
   (parse-words
    (list/p (word/p "foo") (word/p "bar"))
    (list "foo" "bar"))
   (success '("foo" "bar"))))

(define (expr->parser expr rules)
  (match expr
    [(word str)
     (wrap (word/p str))]
    [(list-reference _)
     (error 'make-result-parser "list-reference not supported")]
    [(rule-reference ref)
     (cond
       [(equal? ref 'dgndictation)
        (wrap (many/p (satisfy/p (lambda (d) #t)) #:min 1))]
       [(equal? ref 'dgnwords)
        (wrap (satisfy/p (lambda (d) #t)))]
       [else
        (expr->parser (hash-ref rules ref) rules)])]
    [(optional e)
     (wrap (many/p (expr->parser e rules) #:min 0 #:max 1))]
    [(repeat e)
     (do [r <- (many/p (expr->parser e rules) #:min 1)]
       (pure (lambda () (map (lambda (f) (f)) r))))]
    [(sequence es)
     (do [r <- (apply list/p (map (lambda (e) (expr->parser e rules)) es))]
       (pure (lambda () (map (lambda (f) (f)) r))))]
    [(alternative es)
     (apply or/p (map (lambda (e) (try/p (expr->parser e rules))) es))]
    [(action e f)
     (do [r <- (expr->parser e rules)]
       (pure (lambda () (f (r)))))]))

(define (wrap p)
  (do [r <- p]
    (pure (lambda () r))))

(define (make-result-parser g)
  (match-define (grammar imports exports rules) g)
  (define rule-grammars
    (for/hash ([export exports])
              (values (symbol->string export) (try/p (expr->parser (hash-ref rules export) rules)))))

  (lambda (words active-rules)
    (define parse-grammar
      (apply or/p
             (for/list ([rule active-rules])
               (hash-ref rule-grammars rule))))
    (parse-words parse-grammar words)))

(module+ test
  (define hello-grammar
    (grammar
        '()
      '(foo)
      (hash
       'foo
       (alternative
        (list
         (action
          (sequence
            (list
             (word "hello")
             (word "world")))
          (lambda _ 'res1))
         (action (word "hello")
                 (lambda _ 'res2)))))))

  (define p (make-result-parser hello-grammar 'foo))

  (check-equal?
   (match (p (list "hello" "world"))
     [(success f) (f)])
   'res1)

  (check-equal?
   (match (p (list "hello"))
     [(success f) (f)])
   'res2)
  )