#lang racket

(require
  "surface-grammar.rkt"
  "network.rkt"
  "compiler.rkt"
  "result-parser.rkt"
  "actions-osx.rkt"
  data/either)

(define digits
  (actions
   ["oh" (lambda _ 0)]
   ["one" (lambda _ 1)]
   ["two" (lambda _ 2)]
   ["three" (lambda _ 3)]
   ["four" (lambda _ 4)]
   ["five" (lambda _ 5)]
   ["six" (lambda _ 6)]
   ["seven" (lambda _ 7)]
   ["eight" (lambda _ 8)]
   ["nine" (lambda _ 9)]))

(define number
  (! (or
      (: digits digits digits)
      (: digits digits)
      (: digits))
     (lambda (info)
       (string->number (apply string-append (map number->string info))))))

(define hello-grammar
  (grammar (foo onoff)
    [onoff (actions
            ["stop listening"
             (lambda _
               (deactivate-rule "hello grammar" "foo"))]
            ["listen to me"
             (lambda _
               (activate-rule "hello grammar" "foo"))])]
    [foo (actions
          ["swick"
           (key cmd-tab)]
          ["slap"
           (key enter)]
          ["full screen"
           (key o-m)]
          ["run code"
           (key cmd-r)]
          ["left half"
           (key o-y)]
          ["go to top"
           (key home)]
          ["go to bottom"
           (key end)]
          ["right half"
           (key o-u)]
          ["bigger"
           (key cmd-npplus)]
          ["smaller"
           (key cmd-minus)]
          ["hide window"
           (key cmd-h)]
          [(: "down" number)
           (lambda (res)
             (for ([i (range (second res))])
               ((key down))))]
          [(: "up" number)
           (lambda (res)
             (for ([i (range (second res))])
               ((key up))))]
          [(: "say" 'dgndictation)
           (lambda (res)
             ((text (string-join (second res) " "))))]
          [(: "url" 'dgndictation)
           (lambda (res)
             (define s
               (string-downcase
                (string-replace
                 (string-join (second res) "") " " "")))
             ((text s)))]
          ["open chrome"
           (open-application "Google Chrome")]
          ["open signal"
           (open-application "Signal")]
          ["new tab"
           (key cmd-t)]
          ["close tab"
           (key cmd-w)]
          ["previous tab"
           (key o+cmd-left)]
          ["next tab"
           (key o+cmd-right)]
          ["go back"
           (key cmd-left)]
          ["go forward"
           (key cmd-right)]
          ["go down"
           (key space)]
          ["scratch all"
           (seq (key cmd-a) (key backspace))]
          ["go up"
           (key s-space)]
          [(: "jump" number)
           (lambda (arg)
             ((seq (key cmd+s-space) (pause 50) (text (number->string (second arg))) (key enter))))]
          ["hello"
           (lambda _
             (displayln "Hello, I guess")
             )])]))

(define result-parser (make-result-parser hello-grammar))

(define (strip-extra-word-info word)
  (first (string-split word "\\")))

(init (lambda (res)
        (displayln res)
        (let ([parsed (result-parser (map strip-extra-word-info res))])
          (match parsed
            [(success action) (action)]
            [(failure info) (displayln info)]))
        (flush-output (current-output-port))
        "done"))

(define (testc)
  (load-grammar "hello grammar"
                (compile-grammar
                 hello-grammar))
  (activate-rule "hello grammar" "onoff")
  (activate-rule "hello grammar" "foo")
  (set-exclusive "hello grammar" #t))

(testc)

(define (testuc)
  (unload-grammar "hello grammar"))