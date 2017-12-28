#lang racket

(require
  "surface-grammar.rkt"
  "network.rkt"
  "compiler.rkt"
  "result-parser.rkt"
  "actions-osx.rkt"
  data/either)

(define dictation
  (! 'dgndictation
     (lambda (res)
       (string-join res " "))))

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
   ["rparen" #\(]
   ["right paren" #\(]
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
   ["dollar" #\$]
   ))

(define letters
  (! (+ letter) (lambda (res) (string-downcase (list->string res)))))

(define ((deactivate-applications) _)
  (for ([app (list "drracket-rule" "chrome-rule" "iterm-rule")])
    (deactivate-rule grammar-name app)))

(define ((activate-application app) _)
  (deactivate-applications)
  (activate-rule grammar-name app))

(define window-management
  (action-mapping
   ["hide window"
    (key cmd-h)]
   ["swick"
    (key cmd-grave)]
   [(or "open google chrome" "open chrome")
    (seq (activate-application "chrome-rule")
         (open-application "Google Chrome"))]
   ["open signal"
    (seq (deactivate-applications)
         (open-application "Signal"))]
   ["open vim"
    (seq (deactivate-applications)
         (open-application "MacVim"))]
   ["open iterm"
    (seq (activate-application "iterm-rule")
         (open-application "iTerm"))]
   ["open virtualbox"
    (seq (deactivate-applications)
         (open-application "VirtualBoxVM"))]
   ["open spotify"
    (seq (deactivate-applications)
         (open-application "Spotify"))]
   ["open finder"
    (seq (deactivate-applications)
         (open-application "Finder"))]
   [(or "open racket" "open doctor racket")
    (seq (activate-application "drracket-rule")
         (open-application "/Applications/Racket v6.11/DrRacket.app"))]
   ["window full screen"
    (key o-m)]
   ["window left half"
    (key o-y)]
   ["window right half"
    (key o-u)]))

(define chrome
  (action-mapping
   ["scratch all"
    (seq (key cmd-a) (key backspace))]
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
   ["page down"
    (key space)]
   ["page up"
    (key s-space)]
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
   [(: "url" 'dgndictation)
    (lambda (res)
      (define s
        (string-downcase
         (string-replace
          (string-join (second res) "") " " "")))
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
   ["tab one"
    (key cmd-1)]
   ["tab two"
    (key cmd-2)]
   ["tab three"
    (key cmd-3)]
   ["tab four"
    (key cmd-4)]
   ["tab five"
    (key cmd-5)]
   ["tab six"
    (key cmd-6)]
   ))

(define movement-action
  (value-mapping
   ["left"
    (key left)]
   ["right"
    (key right)]
   ["backspace"
    (key backspace)]
   ["end"
    (seq (key escape) (key ctrl-f))]
   ["forward"
    (seq (key escape) (key ctrl-f)
         (pause 200)
         (key escape) (key ctrl-f)
         (pause 200)
         (key escape) (key ctrl-b)
         (pause 200))]
   ["backward"
    (seq (key escape) (key ctrl-b))]
   ["out"
    (seq (key escape) (key ctrl-u))]
   ["in"
    (seq (key escape) (key down))]))

(define movement
  (+
   (or
    (! (: movement-action number)
       (lambda (res)
         (for ([i (range (second res))])
           ((pause 10))
           ((first res)))))
    (! movement-action (lambda (res) (res))))))

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
     'dgnwords))
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
          ((text word))
          #t])))))

(define drracket
  (or
   movement
   (: "code" code)
   (action-mapping
    ["interactions"
     (key ctrl-f6)]
    ["undo"
     (key cmd-z)]
    ["save file"
     (key cmd-s)]
    ["open require path"
     (key s+cmd-o)]
    ["definitions"
     (key ctrl-f6)]
    ["indent file"
     (key cmd-i)]
    ["new file"
     (key cmd-n)]
    ["run code"
     (key cmd-r)]
    )))

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

(define hello-grammar
  (grammar (global-rule onoff-rule chrome-rule drracket-rule iterm-rule)
    [onoff-rule
     (action-mapping
      ["stop listening"
       (lambda _
         ((deactivate-applications) '_)
         (deactivate-rule grammar-name "global-rule"))]
      ["listen to me"
       (lambda _
         (activate-rule grammar-name "global-rule"))])]
    [chrome-rule
     chrome]
    [drracket-rule
     drracket]
    [iterm-rule
     iterm]
    [global-rule
     (or
      window-management
      (action-mapping
       ["slap"
        (key enter)]
       [(: "spell" letters)
        (lambda (res)
          ((text (second res))))]
       [(: "say" dictation)
        (lambda (res)
          ((text (second res))))]))]))



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

(define grammar-name "main")

(define (start)
  (load-grammar grammar-name
                (compile-grammar
                 hello-grammar))
  (activate-rule grammar-name "onoff-rule")
  (activate-rule grammar-name "global-rule")
  (set-exclusive grammar-name #t))

(start)

(define (unload)
  (unload-grammar grammar-name))