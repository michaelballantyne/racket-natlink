#lang racket

(require
  "surface-grammar.rkt"
  "core/network.rkt"
  "core/compiler.rkt"
  "core/result-parser.rkt"
  "actions-osx.rkt"
  "grammars/basic-dictation.rkt"
  "grammars/apps/iterm.rkt"
  "grammars/apps/chrome.rkt"
  "grammars/apps/drracket.rkt"
  data/either)

(define default-rules '("global-rule" "onoff-rule"))
(define active-rules #f)

(define ((deactivate-applications) _)
  (for ([app (list "drracket-rule" "chrome-rule" "iterm-rule")])
    (set! active-rules default-rules)
    (deactivate-rule grammar-name app)))

(define ((activate-application app) _)
  (deactivate-applications)
  (set! active-rules (cons app active-rules))
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
    (key o-u)]
   ["close window"
    (key cmd-w)]))


(define hello-grammar
  (grammar (global-rule onoff-rule chrome-rule drracket-rule iterm-rule)
    [onoff-rule
     (action-mapping
      ["stop listening"
       (lambda _
         ((deactivate-applications) '_)
         (deactivate-rule grammar-name "global-rule")
         (set! active-rules '("onoff-rule")))]
      ["listen to me"
       (lambda _
         (set! active-rules default-rules)
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
        (let ([parsed (result-parser (map strip-extra-word-info res) active-rules)])
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
  (set! active-rules default-rules)
  (for/list ([rule default-rules])
    (activate-rule grammar-name rule))
  (set-exclusive grammar-name #t))

(start)

(define (unload)
  (unload-grammar grammar-name))