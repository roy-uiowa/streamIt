#lang racket
(require graph)
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
           (require graph)
           (require "graph_functions.rkt")
           ;(require "src_scanner.rkt")
           ;(require "interpreter_updated1.rkt")
           ;(require "environment_updated.rkt")

           ;#|
           (require "new_src_scanner.rkt")
           ;(require "new_interpreter.rkt")
           (require "new_environment.rkt")
           ;|#
           (define data 'str)
           (define a-sample-input-port (open-input-string data))
           (define token-thunk (tokenize a-sample-input-port))
           (define parsed-stx (parse token-thunk))
           (syntax->datum parsed-stx)   ;print the AST: debugging
           
           ;creating the global enviroment and structures

           (define GlobalEnv (environment (list (frame '() #f))))
           (define root #f)
           (define objectList (make-hash))
           (define StreamPipeLines null)
           (define StreamFilters null)
           (define StreamSplitJoins null)
           (define sourceStructure (StreamGraph root
                                                objectList
                                                StreamPipeLines
                                                StreamFilters
                                                StreamSplitJoins))
           (makeStructs parsed-stx sourceStructure)
           (buildGraph sourceStructure programGraph)

           #|
           ;(embelishStructs sourceStructure programGraph)
           (define steadySchedule '("FMRadioCore"))
           ;(define steadySchedule '("IntPrinter"))
           ;(define steadySchedule '("IntSource" "IntSource" "IntSource" "IntSource" "IntSource" "IntSource" "IntSource" "IntSource" "IntSource" "MovingAverage"))
           (for ([i 3])
             (for ([object steadySchedule])
               (let ([node (context object)]
                     [nodeName object])
                 (cond
                   [(StreamPipeLine?  node)
                    (let ([node-stx (StreamPipeLine-Body node)]) ;interpret pipeline
                      (interpret node-stx nodeName node (StreamPipeLine-LocalEnv node) programGraph))]
                 
                   [(StreamSplitJoin? node)
                    (let([node-stx (StreamSplitJoin-Body node)])
                      (interpret node-stx nodeName node (StreamSplitJoin-LocalEnv node) programGraph))
                    (print "join output: ")
                    (println (show-channel-data (get-output-channel nodeName programGraph)))
                    ]
                 
                   [(StreamFilter? node)
                    (cond
                      [(and (equal? (StreamFilter-InitFlag node) #f)
                            (not (equal? (StreamFilter-preWork node) #f))) ;interpret prework function
                       (let ([node-stx (StreamFilter-preWork node)])
                         (interpret node-stx nodeName node (StreamFilter-LocalEnv node) programGraph)
                         (add-to-env "pushRate" (StreamFilter-pushRate node) (StreamFilter-LocalEnv node))
                         (add-to-env "popRate" (StreamFilter-popRate node) (StreamFilter-LocalEnv node))
                         (add-to-env "peekRate" (StreamFilter-peekRate node) (StreamFilter-LocalEnv node)))
                       (set-StreamFilter-InitFlag! node #t)])
                    (let ([node-stx (StreamFilter-Work node)]) ; interpret work function
                      (cond
                        [(equal? (StreamFilter-InitFlag node) #f)
                         (set-StreamFilter-InitFlag! node #t)
                         (add-to-env "pushRate" (StreamFilter-pushRate node) (StreamFilter-LocalEnv node))
                         (add-to-env "popRate" (StreamFilter-popRate node) (StreamFilter-LocalEnv node))
                         (add-to-env "peekRate" (StreamFilter-peekRate node) (StreamFilter-LocalEnv node))])
                      (update-env! "pushRate" (StreamFilter-pushRate node) (StreamFilter-LocalEnv node))
                      (update-env! "popRate" (StreamFilter-popRate node) (StreamFilter-LocalEnv node))
                      (update-env! "peekRate" (StreamFilter-peekRate node) (StreamFilter-LocalEnv node))
                      (interpret node-stx nodeName node (StreamFilter-LocalEnv node) programGraph))]))))
|#
             )))))
