#lang racket

(require "../../surface-grammar.rkt"
         "../../actions-osx.rkt")

(provide iterm)

(define iterm
  (action-mapping
   ["git status"
    (text "git status")]
   ["list"
    (text "ls")]
   ["git add"
    (text "git add ")]
   ["git commit"
    (seq (text "git commit -m \"\"") (key left))]
   ["git push"
    (text "git push")]
   ["git diff"
    (text "git diff")]
   ["press tab"
    (key tab)]))