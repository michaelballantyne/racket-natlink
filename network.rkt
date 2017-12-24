#lang racket

(require
  net/xml-rpc/client
  net/url-string)

(provide
 init
 load-grammar
 unload-grammar
 activate-rule
 deactivate-rule
 set-exclusive)

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
 [deactivate-rule deactivate]
 [set-exclusive setExclusive])

(define (init handler)
  (thread
   (lambda ()
     (define buffer (make-bytes 65536))
     (define socket (udp-open-socket #f #f))
     (udp-bind! socket
                #f
                5003)
     (let loop ()
       (define-values (received-count host port)
         (udp-receive! socket buffer))
       (define res (bytes->string/utf-8 buffer #f 0 received-count))
       (define decoded (string-split res "\0"))
       (handler decoded)
       (loop)))))

