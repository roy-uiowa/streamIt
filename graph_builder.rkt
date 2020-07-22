#lang racket

(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require graph)
(require "new_environment.rkt")
(require "graph_functions.rkt")
(require "pipe_evaluator.rkt")
(require "splitjoin_evaluator.rkt")
(require "filter_evaluator.rkt")
(provide buildGraph)
(provide all-defined-out)



(define (buildGraph sourceStruct graph)
  (println "function build graph")
  (define nodeCounter 0)
  (for ([eachObj (hash->list (StreamGraph-objectList sourceStruct))])
    (cond
      [(StreamPipeLine? (cdr eachObj))
       (if (string=? (StreamGraph-RootObject sourceStruct) (StreamPipeLine-Name (cdr eachObj)))
           (let* ([rootPipe (cdr eachObj)]
                  [objName (StreamPipeLine-Name rootPipe)]
                  [nodeName (StreamGraph-nodeCounter sourceStruct)])
             (add-vertex! graph nodeName)
             (context-set! nodeName (environment (list (frame '() #f))))
             (childList-set! nodeName null)
             (objectName-set! nodeName objName)
             (set-StreamGraph-nodeCounter! sourceStruct (+ (StreamGraph-nodeCounter sourceStruct) 1))
             (evaluate-pipe (StreamPipeLine-Body rootPipe) sourceStruct graph nodeName)
             )
           (println "other"))
       (println eachObj)]
      [(StreamSplitJoin? (cdr eachObj))
       (println eachObj)])))

