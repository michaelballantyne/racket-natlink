#lang racket

(require "network.rkt"
         "compiler.rkt")

(init (lambda (res)
        (displayln res)
        "done"))

(define (testl)
  (load-grammar "bar" #"\0\0\0\0\0\0\0\0\4\0\0\0\f\0\0\0\f\0\0\0\1\0\0\0foo\0\2\0\0\0 \0\0\0\20\0\0\0\2\0\0\0world\0\0\0\20\0\0\0\1\0\0\0hello\0\0\0\3\0\0\0(\0\0\0(\0\0\0\1\0\0\0\1\0\0\0\1\0\0\0\3\0\0\0\1\0\0\0\3\0\0\0\2\0\0\0\2\0\0\0\1\0\0\0hello")
  (activate-rule "bar" "foo"))

(define (testul)
  (unload-grammar "bar"))

(define (testc)
  (load-grammar "g"
                (compile-grammar
                 (grammar
                  '()
                  '(foo)
                  (hash
                   'foo
                   (alternative
                    (list
                     (sequence
                       (list
                        (word "hello")
                        (word "world")))
                     (word "hello")))))))
  (activate-rule "g" "foo"))
(testc)
(define (testuc)
  (unload-grammar "g"))