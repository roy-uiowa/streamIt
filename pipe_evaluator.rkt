#lang racket

(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require graph)
(require "new_environment.rkt")
(require "graph_functions.rkt")
(require "filter_evaluator.rkt")
(provide evaluate-pipe)


(define (evaluate-pipe stx sourceStruct graph nodeName)
  (syntax-parse stx
    ;handling declarations
    [({~literal declaration} stmtType-stx)
     (evaluate-pipe #'stmtType-stx sourceStruct graph nodeName)]
    
    ;handling statements
    [({~literal statement} statement-stx)
     (evaluate-pipe #'statement-stx sourceStruct graph nodeName)]

       
    ;handling block
    [({~literal block} blockStmt-stx ...)
     ;(println "block")
     (for ([eachBlkStmt-stx (syntax->list #'(blockStmt-stx ...))])
       (evaluate-pipe eachBlkStmt-stx sourceStruct graph nodeName))]

        
    ;handle var declaration
    [({~literal varDecl} varDecl-stx ...) ;;; we are consider variable always initialized
     ;(println #'(varDecl-stx ...))
     (syntax-case #'(varDecl-stx ...)()
       [(varType varName varInit-stx)
        (let [(var_name (syntax-e #'varName))
              (var_val (evaluate-pipe #'varInit-stx sourceStruct graph nodeName))]
          (if (eq? (lookup-in-env var_name (context nodeName)) #f)
              (add-to-env var_name var_val (context nodeName))
              (update-env! var_name var_val (context nodeName))))]
       [(varType braket1 arrayLimit-stx braket2 varName)
        (let [(var_name (syntax-e #'varName))
              (array_limit (evaluate-pipe #'arrayLimit-stx graph nodeName))
              ]
                (add-to-env var_name (make-list array_limit #f) (context nodeName))
                (update-env! var_name (make-list array_limit #f) (context nodeName)))]
       [(varType varName) (println "Un-Initialized")])]
    
    ;handling expression
    [({~literal expression} expr-stx)
     (evaluate-pipe #'expr-stx sourceStruct graph nodeName)]
    
    ;handle for loops
    [({~literal forStmt} forInit-stx forCond-stx forInc-stx forBody-stx)
     (add-frame-to-env! (frame '() #f) (context nodeName))
     ;(println (context nodeName))
     (evaluate-pipe #'forInit-stx sourceStruct graph nodeName)
     (while (evaluate-pipe #'forCond-stx sourceStruct graph nodeName)
            (evaluate-pipe #'forBody-stx sourceStruct graph nodeName)
            (evaluate-pipe #'forInc-stx sourceStruct graph nodeName))
     (remove-local-env-frame! (context nodeName))
     ]
    
    ;handling print statement
    [({~literal printStmt} printExpr-stx ...)
     (println "print statement skipped")
     (println (context nodeName))
     ]

    
    ;handle assignment
    [({~literal assignment} assg-stx ...)
     (syntax-case #'(assg-stx ...) ()
       [(varName idx-expr assign-stx)
        (let* [(var_name (syntax-e #'varName))
               (array_idx (evaluate-pipe #'idx-expr sourceStruct graph nodeName))
               (var_val (evaluate-pipe #'assign-stx sourceStruct graph nodeName))
               (array_val (binding-value (lookup-in-env var_name (context nodeName))))
               (new_array (list-set array_val array_idx var_val))]
          (update-env! var_name new_array (context nodeName)))]
       [(varName assign-stx)
        (let [(var_name (syntax-e #'varName))     
              (var_val (evaluate-pipe #'assign-stx sourceStruct graph nodeName))]
          (update-env! var_name var_val (context nodeName)))
        ]
       [(assign-stx)
        ;(println #'assign-stx)
        (evaluate-pipe #'assign-stx sourceStruct graph nodeName)]
       )]
    [({~literal logic_or} lOr-stx)
     ;(println "logic_or")
     (evaluate-pipe #'lOr-stx sourceStruct graph nodeName)]
    [({~literal logic_and} lAnd-stx)
     ;(println "logic_and")
     (evaluate-pipe #'lAnd-stx sourceStruct graph nodeName)]
    [({~literal equality} equality-stx ...)
     ;(println "equality")
     (syntax-case #'(equality-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-pipe #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-pipe #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("!=") (eq? #f (eq? num1 num2))]
            [("==") (eq? num1 num2)]
            ))]
       [(arg)
        (evaluate-pipe #'arg sourceStruct graph nodeName)])]
    
    ;handle comparision instructions
    [({~literal comparison} cmp-stx ...)
     (syntax-case #'(cmp-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-pipe #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-pipe #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("<") (< num1 num2)]
            [(">") (> num1 num2)]
            [("<=") (<= num1 num2)]
            [(">=") (>= num1 num2)]))]
       [(arg)
        (evaluate-pipe #'arg sourceStruct graph nodeName)])]

        
    ; handle addition and subtruction
    [({~literal addition} add-stx ...)
     (syntax-case #'(add-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-pipe #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-pipe #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("+") (+ num1 num2)]
            [("-") (- num1 num2)]))]
       [(arg)
        (evaluate-pipe #'arg sourceStruct graph nodeName)])]
    
    ;handle multiplication and division
    [({~literal multiplication} mult-stx ...)
     ;(println "multiplication")
     (syntax-case #'(mult-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-pipe #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-pipe #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("*") (* num1 num2)]
            [("/") (/ num1 num2)]))]
       [(arg)
        (evaluate-pipe #'arg sourceStruct graph nodeName)])]

    ;handle unary operators: -x, x++, x--
    [({~literal unary} unary-stx ...)
     ;(println "unary")
     (syntax-case #'(unary-stx ...)()
       [(sign arg)
        (if (or (string=? (syntax-e #'arg) "++") (string=? (syntax-e #'arg) "--"))
            (let ([num (evaluate-pipe #'sign sourceStruct graph nodeName)])
              (case (syntax-e #'arg)
                [("++") (+ num 1)]
                [("--") (+ num 1)]
                ))
            (let ([num (evaluate-pipe #'arg sourceStruct graph nodeName)])
              (case (syntax-e #'sign)
                [("-") (- num)])))]
       [(arg)
        (evaluate-pipe #'arg sourceStruct graph nodeName)])]

        
    ;handle function calls
    [({~literal call} call-stx ...)      ; problem here
     ;(println #'(call-stx ...))          ; dispatch issue
     (syntax-case #'(call-stx ...)()
       [(name brkt1 args brkt2)    ;procedure call with arguments
        (let* ([calledNodeName (syntax-e(second(syntax-e #'name)))]
               [calledObject (hash-ref (StreamGraph-objectList sourceStruct) calledNodeName)]
               [listOfArgs null]
               [newNode (StreamGraph-nodeCounter sourceStruct)])
          (for ([eachArg-stx (rest (syntax-e #'args))])
            (set! listOfArgs (append listOfArgs (list (evaluate-pipe eachArg-stx sourceStruct graph nodeName)))))
          ; create a new node
          (add-vertex! graph newNode)
          (objectName-set! newNode calledNodeName)
          (set-StreamGraph-nodeCounter! sourceStruct (+ (StreamGraph-nodeCounter sourceStruct) 1))
          ;and connect edge 
          (if (null? (childList nodeName))
              (begin
                (childList-set! nodeName (list newNode))
                (add-directed-edge! graph nodeName newNode))
              (begin
                (add-directed-edge! graph (last (childList nodeName)) newNode)
                (childList-set! nodeName
                                (append (childList nodeName)
                                        (list newNode)))))
          ;update context 
          (context-set! newNode (environment (list (frame '() #f))))
          (cond
            [(StreamPipeLine? calledObject)
             (println "pipe")
             (extend-env (StreamPipeLine-Parameters calledObject)
                                       listOfArgs
                                       (context newNode))
             (evaluate-pipe (StreamPipeLine-Body calledObject) sourceStruct graph newNode)]
            [(StreamFilter? calledObject)
             (println "filter")
             (extend-env (StreamFilter-Parameters calledObject)
                                       listOfArgs
                                       (context newNode))
             (evaluate-filter (StreamFilter-Body calledObject) sourceStruct graph newNode)]
            [(StreamSplitJoin? calledObject)
             (println "splitjoin")
             (extend-env (StreamSplitJoin-Parameters calledObject)
                                       listOfArgs
                                       (context newNode))
             (println (context newNode))
             (println calledNodeName)
             (evaluate-splitjoin (StreamSplitJoin-Body calledObject) sourceStruct graph newNode)])
          )]
       [(name brkt1 brkt2)    ;procedure call with arguments
        (let* ([calledNodeName (syntax-e(second(syntax-e #'name)))]
               [calledObject (hash-ref (StreamGraph-objectList sourceStruct) calledNodeName)]
               [newNode (StreamGraph-nodeCounter sourceStruct)])
          ; create a new node
          (add-vertex! graph newNode)
          (objectName-set! nodeName calledNodeName)
          (set-StreamGraph-nodeCounter! sourceStruct (+ (StreamGraph-nodeCounter sourceStruct) 1))
          ;and connect edge 
          (if (null? (childList nodeName))
              (begin
                (childList-set! nodeName (list newNode))
                (add-directed-edge! graph nodeName newNode))
              (begin
                (add-directed-edge! graph (last (childList nodeName)) newNode)
                (childList-set! nodeName
                                (append (childList nodeName)
                                        (list newNode)))))
          ;update context 
          (context-set! newNode (environment (list (frame '() #f))))
          (cond
            [(StreamPipeLine? calledObject)
             (evaluate-pipe (StreamPipeLine-Body calledObject) sourceStruct graph newNode)]
            [(StreamFilter? calledObject)
             (evaluate-filter (StreamFilter-Body calledObject) sourceStruct graph newNode)]
            [(StreamSplitJoin? calledObject)
             (evaluate-splitjoin (StreamSplitJoin-Body calledObject) sourceStruct graph newNode)])
          )]
       
       [(name) (evaluate-pipe #'name sourceStruct graph nodeName)])]

       
    [({~literal primary} primary-stx)
     ;(println (syntax-e #'primary-stx))
     (cond
       [(number? (syntax-e #'primary-stx)) (syntax-e #'primary-stx)]
       [(string? (syntax-e #'primary-stx))
        (let [(printStr (syntax-e #'primary-stx))]
          (cond
            [(and (eq? (first (string->list printStr)) #\")
                  (eq? (last (string->list printStr)) #\"))
             (substring printStr 1 (- (string-length printStr) 1))]
            [(if (eq? (lookup-in-env printStr (context nodeName)) #f)
                 (cond
                   [(string=? "push" printStr) printStr]
                   [(string=? "pop" printStr) printStr]   ; handle printing peek/pop
                   [(string=? "peek" printStr) printStr]
                   [(error 'print (~a "Undefined variable:" printStr))])
                 (binding-value (lookup-in-env printStr (context nodeName))))
             ]
            ))])]
    [({~literal addStatement} addStmt-stx)
     ;(println #'addStmt-stx)
     (evaluate-pipe #'addStmt-stx sourceStruct  graph nodeName)]
  ))

(define (evaluate-splitjoin stx sourceStruct graph nodeName)
  (syntax-parse stx
    ;handling declarations
    [({~literal declaration} stmtType-stx)
     (evaluate-splitjoin #'stmtType-stx sourceStruct graph nodeName)]
    
    ;handling statements
    [({~literal statement} statement-stx)
     (evaluate-splitjoin #'statement-stx sourceStruct graph nodeName)]

       
    ;handling block
    [({~literal block} blockStmt-stx ...)
     ;(println "block")
     (for ([eachBlkStmt-stx (syntax->list #'(blockStmt-stx ...))])
       (evaluate-splitjoin eachBlkStmt-stx sourceStruct graph nodeName))]

        
    ;handle var declaration
    [({~literal varDecl} varDecl-stx ...) ;;; we are consider variable always initialized
     ;(println #'(varDecl-stx ...))
     (syntax-case #'(varDecl-stx ...)()
       [(varType varName varInit-stx)
        (let [(var_name (syntax-e #'varName))
              (var_val (evaluate-splitjoin #'varInit-stx sourceStruct graph nodeName))]
          (if (eq? (lookup-in-env var_name (context nodeName)) #f)
              (add-to-env var_name var_val (context nodeName))
              (update-env! var_name var_val (context nodeName))))]
       [(varType braket1 arrayLimit-stx braket2 varName)
        (let [(var_name (syntax-e #'varName))
              (array_limit (evaluate-splitjoin #'arrayLimit-stx graph nodeName))
              ]
                (add-to-env var_name (make-list array_limit #f) (context nodeName))
                (update-env! var_name (make-list array_limit #f) (context nodeName)))]
       [(varType varName) (println "Un-Initialized")])]
    
    ;handling expression
    [({~literal expression} expr-stx)
     (evaluate-splitjoin #'expr-stx sourceStruct graph nodeName)]
    
    ;handle for loops
    [({~literal forStmt} forInit-stx forCond-stx forInc-stx forBody-stx)
     (add-frame-to-env! (frame '() #f) (context nodeName))
     ;(println (context nodeName))
     (evaluate-splitjoin #'forInit-stx sourceStruct graph nodeName)
     (while (evaluate-splitjoin #'forCond-stx sourceStruct graph nodeName)
            (evaluate-splitjoin #'forBody-stx sourceStruct graph nodeName)
            (evaluate-splitjoin #'forInc-stx sourceStruct graph nodeName))
     (remove-local-env-frame! (context nodeName))]
    
    ;handling print statement
    [({~literal printStmt} printExpr-stx ...)
     (println "print statement skipped")
     (println (context nodeName))]
    
    ;handle assignment
    [({~literal assignment} assg-stx ...)
     (syntax-case #'(assg-stx ...) ()
       [(varName idx-expr assign-stx)
        (let* [(var_name (syntax-e #'varName))
               (array_idx (evaluate-splitjoin #'idx-expr sourceStruct graph nodeName))
               (var_val (evaluate-splitjoin #'assign-stx sourceStruct graph nodeName))
               (array_val (binding-value (lookup-in-env var_name (context nodeName))))
               (new_array (list-set array_val array_idx var_val))]
          (update-env! var_name new_array (context nodeName)))]
       [(varName assign-stx)
        (let [(var_name (syntax-e #'varName))     
              (var_val (evaluate-splitjoin #'assign-stx sourceStruct graph nodeName))]
          (update-env! var_name var_val (context nodeName)))
        ]
       [(assign-stx)
        ;(println #'assign-stx)
        (evaluate-splitjoin #'assign-stx sourceStruct graph nodeName)]
       )]
    [({~literal logic_or} lOr-stx)
     ;(println "logic_or")
     (evaluate-splitjoin #'lOr-stx sourceStruct graph nodeName)]
    [({~literal logic_and} lAnd-stx)
     ;(println "logic_and")
     (evaluate-splitjoin #'lAnd-stx sourceStruct graph nodeName)]
    [({~literal equality} equality-stx ...)
     ;(println "equality")
     (syntax-case #'(equality-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-splitjoin #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-splitjoin #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("!=") (eq? #f (eq? num1 num2))]
            [("==") (eq? num1 num2)]
            ))]
       [(arg)
        (evaluate-splitjoin #'arg sourceStruct graph nodeName)])]
    
    ;handle comparision instructions
    [({~literal comparison} cmp-stx ...)
     (syntax-case #'(cmp-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-splitjoin #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-splitjoin #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("<") (< num1 num2)]
            [(">") (> num1 num2)]
            [("<=") (<= num1 num2)]
            [(">=") (>= num1 num2)]))]
       [(arg)
        (evaluate-splitjoin #'arg sourceStruct graph nodeName)])]

        
    ; handle addition and subtruction
    [({~literal addition} add-stx ...)
     (syntax-case #'(add-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-splitjoin #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-splitjoin #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("+") (+ num1 num2)]
            [("-") (- num1 num2)]))]
       [(arg)
        (evaluate-splitjoin #'arg sourceStruct graph nodeName)])]
    
    ;handle multiplication and division
    [({~literal multiplication} mult-stx ...)
     ;(println "multiplication")
     (syntax-case #'(mult-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-splitjoin #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-splitjoin #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("*") (* num1 num2)]
            [("/") (/ num1 num2)]))]
       [(arg)
        (evaluate-splitjoin #'arg sourceStruct graph nodeName)])]

    ;handle unary operators: -x, x++, x--
    [({~literal unary} unary-stx ...)
     ;(println "unary")
     (syntax-case #'(unary-stx ...)()
       [(sign arg)
        (if (or (string=? (syntax-e #'arg) "++") (string=? (syntax-e #'arg) "--"))
            (let ([num (evaluate-splitjoin #'sign sourceStruct graph nodeName)])
              (case (syntax-e #'arg)
                [("++") (+ num 1)]
                [("--") (+ num 1)]
                ))
            (let ([num (evaluate-splitjoin #'arg sourceStruct graph nodeName)])
              (case (syntax-e #'sign)
                [("-") (- num)])))]
       [(arg)
        (evaluate-splitjoin #'arg sourceStruct graph nodeName)])]
    
    [({~literal splitjoinStmt} spltjn-stx spltjnType-stx ...)
     (syntax-case #'(spltjnType-stx ...)()
       [(spltJoinType) (if (string=? (syntax-e (last (syntax-e #'spltJoinType))) "duplicate")
                           (begin
                             ;(println "duplicate")
                             (if (string=? (syntax-e #'spltjn-stx) "split")
                                 (begin
                                   (set-StreamSplitJoin-SplitType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "duplicate")
                                   (set-StreamSplitJoin-SplitRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) 1)) ; no rate setting
                                 (begin
                                   (set-StreamSplitJoin-JoinType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "duplicate")
                                   (set-StreamSplitJoin-JoinRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) 1))))
                           (begin
                             ;(println "roundrobin")
                             (if (string=? (syntax-e #'spltjn-stx) "split")
                                 (begin
                                   (set-StreamSplitJoin-SplitType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "roundrobin")
                                   (set-StreamSplitJoin-SplitRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) 1)) ; no rate setting
                                 (begin
                                   (set-StreamSplitJoin-JoinType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "roundrobin")
                                   (set-StreamSplitJoin-JoinRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) 1)))))] 
       [(spltJoinType rates)
        (let ([splitjoin-rates-stx (rest (syntax->list #'rates))] [sj_rates null])
          (for ([rate splitjoin-rates-stx])
            (set! sj_rates (append sj_rates (list (evaluate-splitjoin rate graph nodeName)))))
          ;(println sj_rates)
          (if (string=? (syntax-e (last (syntax-e #'spltJoinType))) "duplicate")
              (begin
                ;(println #'rates)
                (if (string=? (syntax-e #'spltjn-stx) "split")
                    (begin
                      (set-StreamSplitJoin-SplitType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "duplicate")
                      (set-StreamSplitJoin-SplitRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) sj_rates)) ; split rates
                    (begin
                      (set-StreamSplitJoin-JoinType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "duplicate")
                      (set-StreamSplitJoin-JoinRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) sj_rates)))) ;join rates
              (begin
                      
                (if (string=? (syntax-e #'spltjn-stx) "split")
                    (begin
                      (set-StreamSplitJoin-SplitType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "roundrobin")
                      (set-StreamSplitJoin-JoinRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) sj_rates)) ; rate setting
                    (begin
                      (set-StreamSplitJoin-JoinType! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) "roundrobin")
                      (set-StreamSplitJoin-JoinRate! (hash-ref (StreamGraph-objectList sourceStruct)
                                                                             (objectName nodeName)) sj_rates))))))])]
        
    ;handle function calls
    [({~literal call} call-stx ...)      ; problem here
     ;(println #'(call-stx ...))          ; dispatch issue
     (syntax-case #'(call-stx ...)()
       [(name brkt1 args brkt2)    ;procedure call with arguments
        (let* ([calledNodeName (syntax-e(second(syntax-e #'name)))]
               [calledObject (hash-ref (StreamGraph-objectList sourceStruct) calledNodeName)]
               [listOfArgs null]
               [newNode (StreamGraph-nodeCounter sourceStruct)])
          (for ([eachArg-stx (rest (syntax-e #'args))])
            (set! listOfArgs (append listOfArgs (list (evaluate-splitjoin eachArg-stx sourceStruct graph nodeName)))))
          ; create a new node
          (add-vertex! graph newNode)
          (objectName-set! nodeName calledNodeName)
          (set-StreamGraph-nodeCounter! sourceStruct (+ (StreamGraph-nodeCounter sourceStruct) 1))
          ;and connect edge 
          (childList-set! nodeName
                          (append (childList nodeName)
                                  (list newNode)))
          (add-edge! graph nodeName newNode) ; undirected edge a-->b  b-->a
          ;update context 
          (context-set! newNode (environment (list (frame '() #f))))
          (cond
            [(StreamPipeLine? calledObject)
             (println "pipe")
             (context-set! newNode
                           (extend-env (StreamPipeLine-Parameters calledObject)
                                       listOfArgs
                                       (context newNode)))
             (evaluate-splitjoin (StreamPipeLine-Body calledObject) sourceStruct graph newNode)]
            [(StreamFilter? calledObject)
             (println "filter")
             (context-set! newNode
                           (extend-env (StreamFilter-Parameters calledObject)
                                       listOfArgs
                                       (context newNode)))
             (evaluate-filter (StreamFilter-Body calledObject) sourceStruct graph newNode)]
            [(StreamSplitJoin? calledObject)
             (println "splitjoin")
             (context-set! newNode
                           (extend-env (StreamSplitJoin-Parameters calledObject)
                                       listOfArgs
                                       (context newNode)))
             (evaluate-splitjoin (StreamSplitJoin-Body calledObject) sourceStruct graph newNode)])
          )]
       [(name brkt1 brkt2)    ;procedure call with arguments
        (let* ([calledNodeName (syntax-e(second(syntax-e #'name)))]
               [calledObject (hash-ref (StreamGraph-objectList sourceStruct) calledNodeName)]
               [newNode (StreamGraph-nodeCounter sourceStruct)])
          ; create a new node
          (add-vertex! graph newNode)
          (objectName-set! nodeName calledNodeName)
          (set-StreamGraph-nodeCounter! sourceStruct (+ (StreamGraph-nodeCounter sourceStruct) 1))
          ;and connect edge ; edge also defines the channel
          (childList-set! nodeName
                          (append (childList nodeName)
                                  (list newNode)))
          (add-edge! graph nodeName newNode) ; undirected edge a-->b  b-->a
          ;update context 
          (context-set! newNode (environment (list (frame '() #f))))
          (cond
            [(StreamPipeLine? calledObject)
             (evaluate-splitjoin (StreamPipeLine-Body calledObject) sourceStruct graph newNode)]
            [(StreamFilter? calledObject)
             (evaluate-filter (StreamFilter-Body calledObject) sourceStruct graph newNode)]
            [(StreamSplitJoin? calledObject)
             (evaluate-splitjoin (StreamSplitJoin-Body calledObject) sourceStruct graph newNode)])
          )]      
       [(name) (evaluate-splitjoin #'name sourceStruct graph nodeName)])]

       
    [({~literal primary} primary-stx)
     ;(println (syntax-e #'primary-stx))
     (cond
       [(number? (syntax-e #'primary-stx)) (syntax-e #'primary-stx)]
       [(string? (syntax-e #'primary-stx))
        (let [(printStr (syntax-e #'primary-stx))]
          (cond
            [(and (eq? (first (string->list printStr)) #\")
                  (eq? (last (string->list printStr)) #\"))
             (substring printStr 1 (- (string-length printStr) 1))]
            [(if (eq? (lookup-in-env printStr (context nodeName)) #f)
                 (cond
                   [(string=? "push" printStr) printStr]
                   [(string=? "pop" printStr) printStr]   ; handle printing peek/pop
                   [(string=? "peek" printStr) printStr]
                   [(error 'print (~a "Undefined variable:" printStr))])
                 (binding-value (lookup-in-env printStr (context nodeName))))
             ]
            ))])]
    [({~literal addStatement} addStmt-stx)
     ;(println #'addStmt-stx)
     (evaluate-splitjoin #'addStmt-stx sourceStruct  graph nodeName)]
  ))