#lang racket

(require
  web-server/servlet-env
  net/xml-rpc/client
  net/xml-rpc/server
  net/url-string)

(provide
 init
 load-grammar
 unload-grammar
 activate-rule
 deactivate-rule)

(define endpoint
  (xml-rpc-server
   (string->url "http://192.168.56.101:8000/")))

(define-syntax-rule (rpc-client-methods [name sym] ...)
  (begin (define name (endpoint 'sym))
         ...))

(rpc-client-methods
 [load-grammar loadGrammar]
 [unload-grammar unloadGrammar]
 [activate-rule activate]
 [deactivate-rule deactivate])

(define (init handler)
  (thread
   (lambda ()
     (serve/servlet (make-handle-xml-rpc
                     (hasheq 'result handler))
                    #:port 5002
                    #:servlet-path "/RPC2"
                    #:command-line? #t
                    #:listen-ip #f))))

