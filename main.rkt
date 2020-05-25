#lang racket

(module reader racket
  (require syntax/strip-context)
  (provide (rename-out [streamIt-read read]
                       [streamIt-read-syntax read-syntax]))
 
  (define (streamIt-read in)
    (syntax->datum
     (streamIt-read-syntax #f in)))
 
  (define (streamIt-read-syntax src in)
    (with-syntax ([str (port->string in)])
      (strip-context
       #'(module anything racket
           (provide data)
           (require brag/support)
           (require br-parser-tools/lex)
           (require "parser.rkt")
           (require "grammar.rkt")
           (require "src_scanner.rkt")
           (require "interpreter_updated1.rkt")
           (require "environment_updated.rkt")
           
           (define data 'str)
           (define a-sample-input-port (open-input-string data))
           (define token-thunk (tokenize a-sample-input-port))
           (define parsed-stx (parse token-thunk))
           ;(syntax->datum parsed-stx)   ;print the AST: debugging
           
           ;creating the global enviroment and structures
           (define global-env (environment (list (frame '() #f))))
           (define objectTable (make-hash))
           (define streamSchedule (Scheduler null null null))
           (define streamObjects (list))
           (define functionList '())
           (define sourceStructure (StreamGraph global-env
                                                streamSchedule
                                                streamObjects
                                                objectTable
                                                functionList
                                                ))
           (makeStructs parsed-stx sourceStructure)
           ;sourceStructure
           ;(Scheduler-initSched (StreamGraph-graphSchedular sourceStructure))
           (interpret (Scheduler-initSched (StreamGraph-graphSchedular sourceStructure))
                      (StreamGraph-globalEnv sourceStructure) sourceStructure streamSchedule #f)
           (interpret (Scheduler-steadySched (StreamGraph-graphSchedular sourceStructure))
                      (StreamGraph-globalEnv sourceStructure) sourceStructure streamSchedule #f)
           ;global-env

           )))))
