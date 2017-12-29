#lang racket

(require "../../surface-grammar.rkt"
         "../../actions-osx.rkt"
         "../basic-dictation.rkt")

(provide drracket)

(define movement-action
  (value-mapping
   ["down"
    (key down)]
   ["up"
    (key up)]
   ["undo"
    (key cmd-z)]
   ["slap"
    (key enter)]
   ["scratch word"
    (seq (key escape) (key backspace))]
   ["left"
    (key left)]
   ["right"
    (key right)]
   ["backspace"
    (key backspace)]
   ["end"
    (key o-right)]
   ["forward"
    (key ctrl+o-f)]
   ["backward"
    (key o-left)]
   ["wrap open"
    (key o+ctrl-r)]
   ["wrap square"
    (key o+ctrl-s)]
   ["move out"
    (key o-up)]
   ["move in"
    (key o-down)]))

(define movement
  (+
   (or
    (! (: movement-action number)
       (lambda (res)
         (for ([i (range (min 10 (second res)))])
           ((pause 100))
           ((first res)))))
    (! movement-action (lambda (res) (res))))))

(define common-code-words
  (or
   "pair"
   "cond"
   "let"
   "hash"
   "ref"
   "for"
   "define"
   "first"
   "second"
   "lambda"
   "or"
   "list"
   "pause"
   "key"
   "cons"
   "provide"
   "require"
   "string"
   "symbol"
   "set"
   "add"
   "remove"
   "update"
   "compile"
   "compiler"
   "expression"
   "struct"
   "transparent"
   "match"
   "syntax"
   "parse"
   "test"
   "car"
   "CDR"
   "if"
   (! "false" (lambda (res) "#f"))
   (! "true" (lambda (res) "#t"))
   ))

(define code
  (!
   (+
    (or
     (! (: "numeral" number)
        (lambda (res)
          (second res)))
     (! (: "say" dictation)
        (lambda (res)
          (second res)))
     (! (: "spell" letters)
        (lambda (res)
          (second res)))
     (value-mapping
      ["space"
       'space]
      ["quote string"
       'string]
      ["dash"
       'dash]
      ["slash"
       'slash]
      ["question"
       'question]
      ["bang"
       'bang]
      ["open"
       'open]
      ["square"
       'square]
      ["close"
       'close]
      ["slap"
       'slap]
      )
     common-code-words
     word))
   (lambda (words)
     (for/fold ([space-next? #f])
               ([word words])
       ((pause 100))
       (match word
         ['space
          ((key space))
          #f]
         ['dash
          ((key minus))
          #f]
         ['slash
          ((key slash))
          #f]
         ['question
          ((key s-slash))
          #t]
         ['bang
          ((key s-1))
          #t]
         ['close
          ((key right))
          #t]
         ['open
          (if space-next?
              ((text " ()"))
              ((text "()")))
          ((pause 50))
          ((key left))
          #f]
         ['square
          (if space-next?
              ((text " []"))
              ((text "[]")))
          ((pause 50))
          ((key left))
          #f]
         ['string
          (if space-next?
              ((text " \"\""))
              ((text "\"\"")))
          ((pause 50))
          ((key left))
          #f]
         ['slap
          ((seq (key enter)
                (pause 50)
                (key tab)))
          #f]
         [(? number?)
          (when space-next?
            ((key space)))
          ((text (number->string word)))
          #t]
         [w
          (when space-next?
            ((key space)))
          ((text (string-downcase word)))
          #t])))))

(define drracket
  (or
   movement
   (: "code" code)
   (action-mapping
    ["switch tab one"
     (key cmd-1)]
    ["switch tab two"
     (key cmd-2)]
    ["switch tab three"
     (key cmd-3)]
    ["switch tab four"
     (key cmd-4)]
    ["switch tab five"
     (key cmd-5)]
    ["switch tab six"
     (key cmd-6)]
    ["interactions"
     (key ctrl-f6)]
    ["save file"
     (key cmd-s)]
    ["open require path"
     (key s+cmd-o)]
    ["open local require path"
     (seq
      (key s+cmd-o)
      (pause 100)
      (text "\"\"")
      (key left)
      )]
    ["definitions"
     (key ctrl-f6)]
    ["indent file"
     (key cmd-i)]
    ["new file"
     (key cmd-n)]
    ["run code"
     (key cmd-r)]
    ["search help"
     (key f1)]
    )))