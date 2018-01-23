#lang racket

(require "../../surface-grammar.rkt"
         "../../actions-osx.rkt"
         "../basic-dictation.rkt")

(provide chrome)

(define chrome
  (action-mapping
   ["scratch all"
    (seq (key cmd-a) (key backspace))]
   [(: "scratch word" number)
    (lambda (result)
      (for ([i (range (min 10 (second result)))])
        ((key o-backspace))
        ((pause 100))))]
   ["scratch word"
    (key o-backspace)]
   ["new tab"
    (key cmd-t)]
   ["reopen closed tab"
    (key cmd+s-t)]
   [(or "close tab" "close this tab")
    (key cmd-w)]
   ["previous tab"
    (key o+cmd-left)]
   ["next tab"
    (key o+cmd-right)]
   ["go back"
    (key cmd-left)]
   ["go forward"
    (key cmd-right)]
   [(: "page down" number)
    (lambda (result)
      (for ([i (range (min 10 (second result)))])
        ((key space))
        ((pause 100))))]
   [(: "page up" number)
    (lambda (result)
      (for ([i (range (min 10 (second result)))])
        ((key s-space))
        ((pause 100))))]
   ["page up"
    (key s-space)]
   ["page down"
    (key space)]
   ["go to top"
    (key home)]
   ["go to bottom"
    (key end)]
   [(: "down" number)
    (lambda (res)
      (for ([i (range (second res))])
        ((key down))))]
   [(: "up" number)
    (lambda (res)
      (for ([i (range (second res))])
        ((key up))))]
   [(: "url" dictation)
    (lambda (res)
      (define s
        (string-downcase
         (string-replace
          (second res) " " "")))
      ((text s)))]
   [(: "jump" number)
    (lambda (arg)
      ((seq (key cmd+s-space)
            (pause 50)
            (text (number->string (second arg)))
            (pause 50)
            (key enter))))]
   ["search"
    (key cmd-f)]
   ["escape"
    (key escape)]
   ["bigger"
    (key cmd-npplus)]
   ["smaller"
    (key cmd-minus)]
   ["switch to tab one"
    (key cmd-1)]
   ["switch to tab two"
    (key cmd-2)]
   ["switch to tab three"
    (key cmd-3)]
   ["switch to tab four"
    (key cmd-4)]
   ["switch to tab five"
    (key cmd-5)]
   ["switch to tab six"
    (key cmd-6)]
   ))