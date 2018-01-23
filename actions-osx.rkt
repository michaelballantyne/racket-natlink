#lang racket

(require (for-syntax syntax/parse))

(provide key open-application seq pause text increase-volume decrease-volume)

(define (pause ms)
  (lambda _
    (sleep (/ ms 1000))))

(define (seq . actions)
  (lambda args
    (for-each (lambda (a) (apply a args)) actions)))

(define osascript-path (find-executable-path "osascript"))

(define (applescript script)
  (define-values (proc stdout stdin stderr)
    (subprocess
     #f
     #f
     #f
     osascript-path))
  (write-string script stdin)
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (void))

(define decrease-volume
  (lambda (_)
    (applescript "set volume output volume (output volume of (get volume settings) - 6) --100%")))

(define increase-volume
  (lambda (_)
    (applescript "set volume output volume (output volume of (get volume settings) + 6) --100%")))


(define (open-application app)
  (lambda _
    (define-values (proc stdout stdin stderr)
      (subprocess
       #f
       #f
       #f
       (find-executable-path "open")
       "-a"
       app))
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr)
    (void)))

(define (pbcopy text)
  (define-values (proc stdout stdin stderr)
    (subprocess
     #f
     #f
     #f
     (find-executable-path "pbcopy")))
  (write-string text stdin)
  (close-input-port stdout)
  (close-output-port stdin)
  (close-input-port stderr)
  (void))

(define (text str)
  (lambda _
    (pbcopy str)
    ((key cmd-v))))

(define (format-modifier mod)
  (format "~a down" mod))

(define keycodes
  (hash
   "a" 0
   "s" 1
   "d" 2
   "f" 3
   "h" 4
   "g" 5
   "z" 6
   "x" 7
   "c" 8
   "v" 9
   "b" 11
   "q" 12
   "w" 13
   "e" 14
   "r" 15
   "y" 16
   "t" 17
   "1" 18
   "2" 19
   "3" 20
   "4" 21
   "6" 22
   "5" 23
   "equal" 24
   "9" 25
   "7" 26
   "minus" 27
   "8" 28
   "0" 29
   "rbracket" 30
   "o" 31
   "u" 32
   "lbracket" 33
   "i" 34
   "p" 35
   "enter" 36
   "l" 37
   "j" 38
   "quote" 39
   "k" 40
   "semicolon" 41
   "backslash" 42
   "comma" 43
   "slash" 44
   "n" 45
   "m" 46
   "period" 47
   "tab" 48
   "space" 49
   "grave" 50
   "backspace" 51
   "escape" 53
   "capslock" 57
   "f17" 64
   "npdecimal" 65
   "npmultiply" 67
   "npplus" 69
   "npclear" 71
   "volumeup" 72
   "volumedown" 73
   "mute" 74
   "npdivide" 75
   "npenter" 76
   "npminus" 78
   "f18" 79
   "f19" 80
   "keypadequals" 81
   "np0" 82
   "np1" 83
   "np2" 84
   "np3" 85
   "np4" 86
   "np5" 87
   "np6" 88
   "np7" 89
   "f20" 90
   "np8" 91
   "np9" 92
   "jis_yen" 93
   "jis_underscore" 94
   "jis_keypadcomma" 95
   "f5" 96
   "f6" 97
   "f7" 98
   "f3" 99
   "f8" 100
   "f9" 101
   "jis_eisu" 102
   "f11" 103
   "jis_kana" 104
   "f13" 105
   "f16" 106
   "f14" 107
   "f10" 109
   "f12" 111
   "f15" 113
   "help" 114
   "home" 115
   "pgup" 116
   "del" 117
   "f4" 118
   "end" 119
   "f2" 120
   "pgdown" 121
   "f1" 122
   "left" 123
   "right" 124
   "down" 125
   "up" 126
   ))

(define (do-keystroke key modifiers)
  (define keycode (hash-ref keycodes key))
  (define base-keystroke
    (format "key code ~a" keycode))
  (define with-modifiers
    (if (null? modifiers)
        base-keystroke
        (format "~a using {~a}" base-keystroke
                (string-join (map format-modifier modifiers) ", "))))
  (define cmd
    (format
     "tell application \"System Events\"
        try
            ~a
        on error
            key up {{control, shift, option, command}}
        end try
    end tell"
     with-modifiers))

  (applescript
   cmd))

(define (parsed-keystroke str)
  (define split1 (string-split str "-"))

  (define-values (mods key)
    (if (> (length split1) 1)
        (values (first split1) (second split1))
        (values #f (first split1))))

  (define mods-list
    (if mods
        (let ([split2 (string-split mods "+")])
          split2)
        '()))

  (define translated-mods
    (for/list ([m mods-list])
      (match m
        ["ctrl" "control"]
        ["cmd" "command"]
        ["s" "shift"]
        ["o" "option"])))

  (lambda _
    (printf "doing ~a ~a\n" key translated-mods)
    (do-keystroke key translated-mods)))

(define-syntax (key stx)
  (syntax-parse stx
    [(_ str)
     #'(parsed-keystroke (symbol->string 'str))]))
