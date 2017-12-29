#lang racket

(require "../surface-grammar.rkt")

(provide
 dictation
 word
 digits
 number
 letter
 letters)

(define dictation
  (! 'dgndictation
     (lambda (res)
       (string-join res " "))))

(define word
  'dgnwords)

(define digits
  (value-mapping
   ["oh" 0]
   ["zero" 0]
   ["one" 1]
   ["two" 2]
   ["three" 3]
   ["four" 4]
   ["five" 5]
   ["six" 6]
   ["seven" 7]
   ["eight" 8]
   ["nine" 9]))

(define number
  (! (or
      (: digits digits digits)
      (: digits digits)
      (: digits))
     (lambda (info)
       (string->number (apply string-append (map number->string info))))))

(define letter
  (value-mapping
   ["alpha" #\a]
   ["bravo" #\b]
   ["charlie" #\c]
   ["delta" #\d]
   ["echo" #\e]
   ["foxtrot" #\f]
   ["golf" #\g]
   ["hotel" #\h]
   ["india" #\i]
   ["juliett" #\j]
   ["kilo" #\k]
   ["Lima" #\l]
   ["mike" #\m]
   ["november" #\n]
   ["oscar" #\o]
   ["papa" #\p]
   ["quebec" #\q]
   ["romeo" #\r]
   ["sierra" #\s]
   ["tango" #\t]
   ["uniform" #\u]
   ["victor" #\v]
   ["whiskey" #\w]
   ["xray" #\x]
   ["yankee" #\y]
   ["zulu" #\z]
   ["lpar" #\(]
   ["lparen" #\(]
   ["left paren" #\(]
   ["rpar" #\)]
   ["rparen" #\)]
   ["right paren" #\)]
   ["string quote" #\"]
   ["quote" #\']
   ["hash" #\#]
   ["lsquare" #\[]
   ["rsquare" #\]]
   ["bang" #\!]
   ["slash" #\/]
   ["backslash" #\\]
   ["huh" #\?]
   ["star" #\*]
   ["quasiquote" #\`]
   ["comma" #\,]
   ["newline" #\newline]
   ["space" #\space]
   ["colon" #\:]
   ["plus" #\+]
   ["dash" #\-]
   ["dollar sign" #\$]
   ["period" #\.]
   ))

(define letters
  (! (+ letter) (lambda (res) (string-downcase (list->string res)))))
