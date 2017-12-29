#lang racket

(require (only-in plai define-type))

(provide grammar word rule-reference list-reference optional repeat sequence alternative action)

(define-type Grammar
  (grammar [imports (listof symbol?)]
           [exports (listof symbol?)]
           [rules (hash/c symbol? Rule-Expr?)]))

(define-type Rule-Expr
  (word [str string?])
  (rule-reference [name symbol?])
  (list-reference [name symbol?])
  (optional [e Rule-Expr?])
  (repeat [e Rule-Expr?])
  (sequence [es (listof Rule-Expr?)])
  (alternative [es (listof Rule-Expr?)])
  (action [e Rule-Expr?] [f any/c]))