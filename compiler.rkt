#lang racket

(require (only-in plai define-type))

(provide compile-grammar
         grammar word rule-reference list-reference optional repeat sequence alternative)

; Input language

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
  (alternative [es (listof Rule-Expr?)]))

; IR language

(define-type IR
  (ir [words (listof string?)]
      [lists (listof symbol?)]
      [imports (listof id?)]
      [exports (listof id?)]
      [rules (hash/c id? (listof IR-Rule-Element?))]))

(define composite-label (or/c 'repeat 'optional 'sequence 'alternative))

(define-type IR-Rule-Element
  (ir-word [id id?])
  (ir-rule-reference [id id?])
  (ir-list-reference [id id?])
  (start [type composite-label])
  (end [type composite-label]))


(define (validate-grammar! g)
  (define (assert! bool msg . args)
    (when (not bool)
      (apply error 'validate-grammar! msg args)))

  (match-define (grammar imports exports rules) g)
  
  (assert! (not (empty? exports)) "grammar has no exported rules")

  (for ([rulename exports])
    (assert! (hash-has-key? rules rulename) "exported rule ~a is not defined" rulename))

  (let ([duplicates (set-intersect (list->set imports) (list->set (hash-keys rules)))])
    (assert! (set-empty? duplicates) "rules both imported and defined: ~a" duplicates))
  
  (define available-rules
    (set-union
     (list->set imports)
     (list->set (hash-keys rules))))

  (define (validate-expr! rulename expr seen)
    (match expr
      [(or (struct word _) (struct list-reference _))
       (void)]
      [(rule-reference ref)
       (assert! (not (set-member? seen ref)) "found recursion in rule ~a" rulename)
       (validate-expr! rulename (hash-ref rules ref) (set-add seen ref))]
      [(optional e)
       (validate-expr! rulename e seen)]
      [(repeat e)
       (validate-expr! rulename e seen)]
      [(sequence es)
       (for-each (lambda (r) (validate-expr! rulename r seen)) es)]
      [(alternative es)
       (for-each (lambda (r) (validate-expr! rulename r seen)) es)]))
  
  (for ([(rulename expr) rules])
    (validate-expr! rulename expr (set rulename))))

; Translation from input to IR

(define (flatten-expr expr)
  (define (flatten-composite type es)
    (append (list (start type))
            (apply append (map flatten-expr es))
            (list (end type))))
  
  (match expr
    [(or (struct word _) (struct rule-reference _) (struct list-reference _))
     (list expr)]
    [(optional e)
     (flatten-composite 'optional (list e))]
    [(repeat e)
     (flatten-composite 'repeat (list e))]
    [(sequence es)
     (flatten-composite 'sequence es)]
    [(alternative es)
     (flatten-composite 'alternative es)]))

(module+ test
  (require rackunit)

  (check-equal?
   (flatten-expr
    (alternative
     (list
      (word "foo")
      (sequence
        (list
         (word "hello")
         (optional (word "world")))))))
   (list
    (start 'alternative)
    (word "foo")
    (start 'sequence)
    (word "hello")
    (start 'optional)
    (word "world")
    (end 'optional)
    (end 'sequence)
    (end 'alternative))))

; The target representation factors words, lists, and rules out
; into arrays, and replaces uses of them with their indices in those
; arrays. This helper encapsulates the state required to handle
; one such list.
(struct id [id val] #:transparent)
(define (list-interner)
  (define l '())
  (define map (make-hash))
  (define ctr 1)
  (define (intern! varg)
    ; We'll want strings when we're packing to bytes, so convert here
    (define v (if (symbol? varg)
                  (symbol->string varg)
                  varg))
    (when (not (hash-has-key? map v))
      (hash-set! map v (id ctr v))
      (set! l (cons v l))
      (set! ctr (add1 ctr)))
    (hash-ref map v))
  (define (get) (reverse l))
  (values get intern!))

; Flatten the expression language and intern words, lists, and rules,
;   but don't yet switch from symbolic representation to bytes.
(define (build-ir g)
  (match-define (grammar imports exports rules) g)
  
  (define-values (get-words intern-word!) (list-interner))
  (define-values (get-lists intern-list!) (list-interner))
  (define-values (get-rules intern-rule!) (list-interner))

  (define (build-expr-ir expr)
    (for/list ([atom (flatten-expr expr)])
      (match atom
        [(or (start _) (end _))
         atom]
        [(word name)
         (ir-word (intern-word! name))]
        [(list-reference name)
         (ir-list-reference (intern-list! name))]
        [(rule-reference name)
         (ir-rule-reference (intern-rule! name))])))

  (define imports-ir (map intern-rule! imports))
  (define exports-ir (map intern-rule! exports))
  (define rules-ir
    (for/hash ([(rule-name expr) (in-hash rules)]) 
              (values
               (intern-rule! rule-name)
               (build-expr-ir expr))))

  ; Note that lifting imports-ir, exports-ir, and rules-ir is necessary
  ; so that their interning effects happen before (get-words), etc.
  (ir
   (get-words)
   (get-lists)
   imports-ir
   exports-ir
   rules-ir))


(module+ test
  (check-equal?
   (build-ir
    (grammar '() '(foo)
             (hash 'bar (word "bar")
                   'foo (alternative
                         (list
                          (word "foo")
                          (sequence
                            (list
                             (word "hello")
                             (optional (word "world")))))))))
   (ir
    '("bar" "foo" "hello" "world")
    '()
    '()
    (list (id 1 "foo"))
    (hash (id 2 "bar")
          (list (ir-word (id 1 "bar")))
          (id 1 "foo")
          (list
           (start 'alternative)
           (ir-word (id 2 "foo"))
           (start 'sequence)
           (ir-word (id 3 "hello"))
           (start 'optional)
           (ir-word (id 4 "world"))
           (end 'optional)
           (end 'sequence)
           (end 'alternative))))))

(require binaryio/integer)

(define (WORD v)
  (integer->bytes v 2 #f #f))
(define (DWORD v)
  (integer->bytes v 4 #f #f))

(define (enumerate l)
  (map id (range 1 (+ 1 (length l))) l))

(define (string->dword-padded-bytes str)
  (define padded-length (bitwise-and (+ (string-length str) 4) #xFFFC))
  (bytes-append
   (string->bytes/latin-1 str)
   (make-bytes (- padded-length (string-length str)))))
  
; Int, Listof id
(define (pack-list chunk-type l)
  (define total-length 0)
  
  (define el-bytes
    (for/list ([el l])
      (match-define (id num str) el)
      (define padded-string (string->dword-padded-bytes str))
      (define total-item-length (+ (bytes-length padded-string) 8)) ; includes length and id number
      (set! total-length (+ total-length total-item-length))
      (bytes-append
       (DWORD total-item-length)
       (DWORD num)
       padded-string)))
  
  (apply bytes-append
         ; Chunk header
         (DWORD chunk-type)
         (DWORD total-length)
         el-bytes))

(define type-codes
  (hash 'sequence 1
        'alternative 2
        'repeat 3
        'optional 4))

(define (pack-rules rule-map)
  (define rule-bytes
    (apply bytes-append
           (for/list ([(name expr) rule-map])
             (define expr-bytes
               (apply bytes-append
                      (for/list ([atom expr])
                        (match atom
                          [(start type)
                           (bytes-append
                            (WORD 1)
                            (WORD 0)
                            (DWORD (hash-ref type-codes type)))]
                          [(end type)
                           (bytes-append
                            (WORD 2)
                            (WORD 0)
                            (DWORD (hash-ref type-codes type)))]
                          [(ir-word (id num _))
                           (bytes-append
                            (WORD 3)
                            (WORD 0)
                            (DWORD num))]
                          [(ir-list-reference (id num _))
                           (bytes-append
                            (WORD 6)
                            (WORD 0)
                            (DWORD num))]
                          [(ir-rule-reference (id num _))
                           (bytes-append
                            (WORD 4)
                            (WORD 0)
                            (DWORD num))]))))
             (define rule-length (+ (bytes-length expr-bytes) 8))
             (bytes-append
              (DWORD rule-length)
              (DWORD (id-id name))
              expr-bytes))))
  
  (bytes-append
   (DWORD 3)
   (DWORD (bytes-length rule-bytes))
   rule-bytes))

(define (pack-grammar g-ir)
  (match-define (ir words lists imports exports rules) g-ir)
  (bytes-append
   (DWORD 0) ; type
   (DWORD 0) ; flags
   (if (null? exports) #""
       (pack-list 4 exports))
   (if (null? imports) #""
       (pack-list 5 imports))
   (if (null? lists) #""
       (pack-list 6 (enumerate lists)))
   (if (null? words) #""
       (pack-list 2 (enumerate words)))
   (if (hash-empty? rules) #""
       (pack-rules rules))))

(module+ test
  (check-equal?
   (pack-grammar (build-ir
                  (grammar '() '(foo)
                           (hash 'foo (word "hello")))))
   #"\0\0\0\0\0\0\0\0\4\0\0\0\f\0\0\0\f\0\0\0\1\0\0\0foo\0\2\0\0\0\20\0\0\0\20\0\0\0\1\0\0\0hello\0\0\0\3\0\0\0\20\0\0\0\20\0\0\0\1\0\0\0\3\0\0\0\1\0\0\0"))
(define (compile-grammar g)
  (validate-grammar! g)
  (pack-grammar (build-ir g)))