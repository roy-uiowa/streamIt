#lang racket
(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require graph)
(provide makeStructs)
(provide buildGraph)
(require "new_environment.rkt")
(require "graph_functions.rkt")

(provide all-defined-out)
; function makestructs reads the source code and makes callable structures

(define (makeStructs stx sourceStructure)
  (syntax-parse stx
    [({~literal program} decStmt-stx ...)   ;; remove AST program tag
     (for ([eachStmt-stx (syntax->list #'(decStmt-stx ...))])
       (makeStructs eachStmt-stx sourceStructure))]
    
    [({~literal declaration} graphObj-stx)  ;; remove AST declaration tag
     (makeStructs #'graphObj-stx sourceStructure)]
    
    
    [({~literal streamObject} streamObjCnl-stx streamObjType-stx streamObjBody-stx)
     ;(print "streamObj found: ")
     (let-values ([(ObjectType) (syntax-e(second (syntax-e #'streamObjType-stx)))]
                  [(input output) (getChannels #'streamObjCnl-stx)]
                  [(name parameters body-stx) (getObjDetails #'streamObjBody-stx)]
                  )
       ;get name and add it to source object list
       (cond
         [(string=? ObjectType "pipeline")
          (let ([newPipe (StreamPipeLine name parameters body-stx)])
            (hash-set! (StreamGraph-objectList sourceStructure) name newPipe) ; we'll handle add statements later once we know all the objects
            (cond
              [(and (eq? input #f) (eq? output #f))
                (set-StreamGraph-RootObject! sourceStructure name)]))]
         [(string=? ObjectType "filter")
          (let ([newFilter (StreamFilter name parameters #f #f null body-stx)])
            (let-values ([(prework work functions) (getFilterDetails body-stx)])
              (set-StreamFilter-PreWork! newFilter prework)
              (set-StreamFilter-Work! newFilter work)
              (set-StreamFilter-FunctionList! newFilter functions)
              (hash-set! (StreamGraph-objectList sourceStructure) name newFilter)))]
         [(string=? ObjectType "splitjoin")
          (let ([newSJ (StreamSplitJoin name parameters body-stx #f #f #f #f)])
            (hash-set! (StreamGraph-objectList sourceStructure) name newSJ))]))]

    
    ;handling block
    [({~literal block} blockStmt-stx ...)
     (for ([eachBlkStmt-stx (syntax->list #'(blockStmt-stx ...))])
       (makeStructs eachBlkStmt-stx sourceStructure ))]
    
    ;handling statements
    [({~literal statement} statement-stx)
     (makeStructs #'statement-stx sourceStructure )
     ]
))

(define (getChannels stx)
  (syntax-parse stx
    [({~literal channel_type} input-stx output-stx)
     (let ([a (if (string=? (syntax-e(second (syntax-e #'input-stx))) "void") #f null)]
           [b (if (string=? (syntax-e(second (syntax-e #'output-stx))) "void") #f null)])
       (values a b))]))

(define (getObjDetails stx)
  (syntax-case stx ()
    [(function Name Params Body)
     (let ([name (syntax-e #'Name)]
           [body #'Body]
           [parameters null])
       (syntax-parse #'Params
         [({~literal parameters} argument-stx ...)
          (for ([eachArg-stx (syntax->list #'(argument-stx ...))])
            (set! parameters (append parameters
                                     (list(syntax-e(third (syntax-e eachArg-stx)))))))])
       (values name parameters body))]
    [(function Name Body)
     (let ([name (syntax-e #'Name)]
           [parameters null]
           [body #'Body])
       (values name parameters body))]))

(define (getFilterDetails stx)
  (let ([prework #f]
        [work #f]
        [functions null])
    (syntax-case stx ()
      [(block init-stx work-stx functions-stx ...)
       (set! prework #'init-stx)
       (set! work #'work-stx)]
         
      [(block work-stx functions-stx ...)
       (set! work #'workBlock-stx)])
       (values prework work functions)))

(define (getRateValues stx) ; return push pop and peek rates
  (let ([rateVarList-stx (rest (syntax-e stx))] [pushRate #f] [popRate #f] [peekRate #f] [currentType ""])
    (for ([eachRate-stx rateVarList-stx])
      (let [(rateData (syntax-e (second (syntax-e eachRate-stx))))]
        (cond
          [(string? rateData) (set! currentType rateData)]
          [(case currentType
             [("push") (set! pushRate rateData)]
             [("peek") (set! peekRate rateData)]
             [("pop") (set! popRate rateData)])])))
    (values pushRate popRate peekRate)))


;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            (evaluate-splitjoin #'forInc-stx sourceStruct graph nodeName)
            )
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


;;;;;;;;;;;;;;;;; filter section
(define (evaluate-filter stx sourceStruct graph nodeName)
  (syntax-parse stx
    ;handling declarations
    [({~literal declaration} stmtType-stx)
     (evaluate-filter #'stmtType-stx sourceStruct graph nodeName)]
    
    ;handling statements
    [({~literal statement} statement-stx)
     (evaluate-filter #'statement-stx sourceStruct graph nodeName)]

       
    ;handling block
    [({~literal block} blockStmt-stx ...)
     ;(println "block")
     (for ([eachBlkStmt-stx (syntax->list #'(blockStmt-stx ...))])
       (evaluate-filter eachBlkStmt-stx sourceStruct graph nodeName))]

        
    ;handle var declaration
    [({~literal varDecl} varDecl-stx ...) ;;; we are consider variable always initialized
     ;(println #'(varDecl-stx ...))
     (syntax-case #'(varDecl-stx ...)()
       [(varType varName varInit-stx)
        (let [(var_name (syntax-e #'varName))
              (var_val (evaluate-filter #'varInit-stx sourceStruct graph nodeName))]
          (if (eq? (lookup-in-env var_name (context nodeName)) #f)
              (add-to-env var_name var_val (context nodeName))
              (update-env! var_name var_val (context nodeName))))]
       [(varType braket1 arrayLimit-stx braket2 varName)
        (let [(var_name (syntax-e #'varName))
              (array_limit (evaluate-filter #'arrayLimit-stx graph nodeName))
              ]
                (add-to-env var_name (make-list array_limit #f) (context nodeName))
                (update-env! var_name (make-list array_limit #f) (context nodeName)))]
       [(varType varName) (println "Un-Initialized")])]
    
    ;handling expression
    [({~literal expression} expr-stx)
     (evaluate-filter #'expr-stx sourceStruct graph nodeName)]
    
    ;handle for loops
    [({~literal forStmt} forInit-stx forCond-stx forInc-stx forBody-stx)
     (add-frame-to-env! (frame '() #f) (context nodeName))
     ;(println (context nodeName))
     (evaluate-filter #'forInit-stx sourceStruct graph nodeName)
     (while (evaluate-filter #'forCond-stx sourceStruct graph nodeName)
            (evaluate-filter #'forBody-stx sourceStruct graph nodeName)
            (evaluate-filter #'forInc-stx sourceStruct graph nodeName)
            )
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
               (array_idx (evaluate-filter #'idx-expr sourceStruct graph nodeName))
               (var_val (evaluate-filter #'assign-stx sourceStruct graph nodeName))
               (array_val (binding-value (lookup-in-env var_name (context nodeName))))
               (new_array (list-set array_val array_idx var_val))]
          (update-env! var_name new_array (context nodeName)))]
       [(varName assign-stx)
        (let [(var_name (syntax-e #'varName))     
              (var_val (evaluate-filter #'assign-stx sourceStruct graph nodeName))]
          (update-env! var_name var_val (context nodeName)))
        ]
       [(assign-stx)
        ;(println #'assign-stx)
        (evaluate-filter #'assign-stx sourceStruct graph nodeName)]
       )]
    [({~literal logic_or} lOr-stx)
     ;(println "logic_or")
     (evaluate-filter #'lOr-stx sourceStruct graph nodeName)]
    [({~literal logic_and} lAnd-stx)
     ;(println "logic_and")
     (evaluate-filter #'lAnd-stx sourceStruct graph nodeName)]
    [({~literal equality} equality-stx ...)
     ;(println "equality")
     (syntax-case #'(equality-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-filter #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-filter #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("!=") (eq? #f (eq? num1 num2))]
            [("==") (eq? num1 num2)]
            ))]
       [(arg)
        (evaluate-filter #'arg sourceStruct graph nodeName)])]
    
    ;handle comparision instructions
    [({~literal comparison} cmp-stx ...)
     (syntax-case #'(cmp-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-filter #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-filter #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("<") (< num1 num2)]
            [(">") (> num1 num2)]
            [("<=") (<= num1 num2)]
            [(">=") (>= num1 num2)]))]
       [(arg)
        (evaluate-filter #'arg sourceStruct graph nodeName)])]

        
    ; handle addition and subtruction
    [({~literal addition} add-stx ...)
     (syntax-case #'(add-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-filter #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-filter #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("+") (+ num1 num2)]
            [("-") (- num1 num2)]))]
       [(arg)
        (evaluate-filter #'arg sourceStruct graph nodeName)])]
    
    ;handle multiplication and division
    [({~literal multiplication} mult-stx ...)
     ;(println "multiplication")
     (syntax-case #'(mult-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (evaluate-filter #'arg1 sourceStruct graph nodeName)]
              [num2 (evaluate-filter #'arg2 sourceStruct graph nodeName)])
          (case (syntax-e #'sign)
            [("*") (* num1 num2)]
            [("/") (/ num1 num2)]))]
       [(arg)
        (evaluate-filter #'arg sourceStruct graph nodeName)])]

    ;handle unary operators: -x, x++, x--
    [({~literal unary} unary-stx ...)
     ;(println "unary")
     (syntax-case #'(unary-stx ...)()
       [(sign arg)
        (if (or (string=? (syntax-e #'arg) "++") (string=? (syntax-e #'arg) "--"))
            (let ([num (evaluate-filter #'sign sourceStruct graph nodeName)])
              (case (syntax-e #'arg)
                [("++") (+ num 1)]
                [("--") (+ num 1)]
                ))
            (let ([num (evaluate-filter #'arg sourceStruct graph nodeName)])
              (case (syntax-e #'sign)
                [("-") (- num)])))]
       [(arg)
        (evaluate-filter #'arg sourceStruct graph nodeName)])]

        
    ;handle function calls
    [({~literal call} call-stx ...)      ; problem here
     ;(println #'(call-stx ...))          ; dispatch issue
     (syntax-case #'(call-stx ...)()
       [(name) (evaluate-filter #'name sourceStruct graph nodeName)])]

       
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
                 (binding-value (lookup-in-env printStr (context nodeName))))]
            ))])]
    [({~literal addStatement} addStmt-stx)
     ;(println #'addStmt-stx)
     (evaluate-filter #'addStmt-stx sourceStruct  graph nodeName)]
  ))



#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;need to define functions for inserting new child in pipline or splitjoin
(define (embelishStructs sourceStruct graph)
  (for ([eachObj (hash->list (StreamGraph-objectList sourceStruct))])
    (cond
      [(StreamPipeLine? (cdr eachObj))
       (let* ([rootPipe (cdr eachObj)]
              [nodeName (StreamPipeLine-Name rootPipe)])
         (add-vertex! graph nodeName)
         (context-set! nodeName rootPipe)
         ;(println (context nodeName))
         (embelishPipilines sourceStruct rootPipe nodeName graph)
         )]
      [(StreamSplitJoin? (cdr eachObj))
       (let* ([rootSJ (cdr eachObj)]
             [nodeName (StreamSplitJoin-Name rootSJ)])
         (add-vertex! graph nodeName)
         (context-set! nodeName rootSJ)
         (embelishSplitJoins sourceStruct rootSJ nodeName graph)
         (get-vertices graph)
         ;(println (get-edges graph))
         )])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (embelishPipilines sourceStruct rootPipe rootNode graph)
  (let ([childList (scanForAddstatement (StreamPipeLine-Body rootPipe))])
    (set-StreamPipeLine-ChildList! rootPipe childList)
    (let ([previousChild #f])
      (for ([eachChild childList])
        (let ([childObj (hash-ref (StreamGraph-objectList sourceStruct) eachChild)]
              [childNode eachChild])
          (add-vertex! graph childNode)
          (context-set! childNode childObj)
          (cond
            [(string=? childNode (first childList)) ; the first child will receive data from input of the root
             (add-directed-edge! graph rootNode childNode)
             (channel-set! rootNode childNode null)]
            [(string=? childNode (last childList))
             (add-directed-edge! graph childNode rootNode)
             (channel-set! childNode rootNode null)])
          (cond
            [(equal? previousChild #f) #t]
            [else 
             (add-directed-edge! graph previousChild childNode)
             (channel-set! previousChild childNode null)])
          (set! previousChild childNode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (embelishSplitJoins sourceStruct rootSJ rootNode graph)
  (let ([childList (scanForAddstatement (StreamSplitJoin-Body rootSJ))])
    (set-StreamSplitJoin-ChildList! rootSJ childList)
      (for ([eachChild childList])
        (let ([childObj (hash-ref (StreamGraph-objectList sourceStruct) eachChild)]
              [childNode eachChild])
          (add-vertex! graph childNode)
          (context-set! childNode childObj)
          (add-edge! graph rootNode childNode)
          (channel-set! rootNode childNode null)
          (channel-set! childNode rootNode null)))))

(define (scanForAddstatement stx)
  (let ([children null])
  (syntax-parse stx
    ;remove block tag
    [({~literal block} decl-stx ...)
     (for ([line-stx (syntax->list #'(decl-stx ...))])
       (let* ([stmt (syntax-e (second (syntax-e (second (syntax-e line-stx)))))]
             [stmtType (syntax-e (first stmt))])
         (cond
           [(symbol=? stmtType 'addStatement)
            (set! children (append children
                                   (list (syntax-e(second (syntax-e (second (syntax-e (second stmt)))))))))])))])
       children))

|#
