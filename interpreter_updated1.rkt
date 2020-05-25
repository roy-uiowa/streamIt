#lang racket
(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require "environment_updated.rkt")

(provide interpret)

;update details: add child abstraction

(define (interpret stx env sourceStruct currentObject previousObject)
  (syntax-parse stx
    [({~literal scheduler} initSchedule-stx steadySchedule-stx)
     (interpret #'initSchedule-stx env sourceStruct currentObject previousObject)
     (interpret #'steadySchedule-stx env sourceStruct currentObject previousObject)
     ]   
    [({~literal initSched} initBlock-stx)
     ;(println #'initBlock-stx)
     (interpret #'initBlock-stx env sourceStruct currentObject previousObject)]
    
    [({~literal steadySched} steadyBlock-stx)
     ;(println "steadySched")
     (interpret #'steadyBlock-stx env sourceStruct currentObject previousObject)]
    [({~literal program} decStmt-stx ...)
     (for ([eachStmt-stx (syntax->list #'(decStmt-stx ...))])
       (interpret eachStmt-stx env sourceStruct currentObject previousObject))]

    ;handling comments
    [({~literal comments} cmnt-stx)
     (println #'cmnt-stx)
     ]
    
    ;handling declarations
    [({~literal declaration} stmtType-stx)
     ;(println "declaration")
     (interpret #'stmtType-stx env sourceStruct currentObject previousObject)]
    
    ; handline initBlock
    [({~literal initBlock} initBlockStmt-stx)
     (if (eq? (StreamObj-initFlag currentObject) #f)
         (begin
           (set-StreamObj-initFlag! currentObject #t)
           (interpret #'initBlockStmt-stx env sourceStruct currentObject previousObject))
         (println "Already Initialized!!"))
     ]
    
    ;handling workBlock
    [({~literal workBlock} rateDecl-stx workBlockStmt-stx)
     (let ([rateVarList-stx (rest (syntax-e #'rateDecl-stx))] [pushRate 0] [popRate 0] [peekRate 0] [currentType ""])
       (for ([eachRate-stx rateVarList-stx])
         (let [(rateData (interpret eachRate-stx env sourceStruct currentObject previousObject))]
           ;(println rateData)
           (cond
             [(string? rateData) (set! currentType rateData)]
             [(case currentType
               [("push") (set! pushRate rateData)]
               [("peek") (set! peekRate rateData)]
               [("pop") (set! popRate rateData)])])
           ))
       ;(println env)
       (if (eq? (lookup-in-env "pushRate" env) #f)   ; set or update push/pop/peek rates
           (add-to-env "pushRate" pushRate env)
           (update-env! "pushRate" pushRate env))
       (if (eq? (lookup-in-env "popRate" env) #f)
           (add-to-env "popRate" popRate env)
           (update-env! "popRate" popRate env))
       (if (eq? (lookup-in-env "peekRate" env) #f)
           (add-to-env "peekRate" peekRate env)
           (update-env! "peekRate" peekRate env))
       ;(println env)
       )
     (interpret #'workBlockStmt-stx env sourceStruct currentObject previousObject)
     ]
    
    ;handling block
    [({~literal block} blockStmt-stx ...)
     ;(println "block")
     (for ([eachBlkStmt-stx (syntax->list #'(blockStmt-stx ...))])
       (interpret eachBlkStmt-stx env sourceStruct currentObject previousObject))]
    ;handling statements
    [({~literal statement} statement-stx)
     ;(println "statement")
     (interpret #'statement-stx env sourceStruct currentObject previousObject)]
    ;handling pring statement
    [({~literal printStmt} printExpr-stx ...)
     (syntax-case #'(printExpr-stx ...)()
       [(string Expr-stx)
        (let ([print-string (interpret #'Expr-stx env sourceStruct currentObject previousObject)])
          (cond
            [(syntax? print-string)
             (println print-string)]
            [else (println print-string)]))]
       [(Expr-stx) (println "call stmt")]
       )]
    
    ;handle for loops
    [({~literal forStmt} forInit-stx forCond-stx forInc-stx forBody-stx)
     (interpret #'forInit-stx env sourceStruct currentObject previousObject)
     (while (interpret #'forCond-stx env sourceStruct currentObject previousObject)
            (interpret #'forBody-stx env sourceStruct currentObject previousObject)
            (interpret #'forInc-stx env sourceStruct currentObject previousObject)
            )]
    
    ;handle var declaration
    [({~literal varDecl} varDecl-stx ...) ;;; we are consider variable always initialized
     ;(println #'(varDecl-stx ...))
     (syntax-case #'(varDecl-stx ...)()
       [(varType varName varInit-stx)
        ;(println "Initialized")
        (let [(var_name (syntax-e #'varName))
              (var_val (interpret #'varInit-stx env sourceStruct currentObject previousObject))]
          (if (eq? (lookup-in-env var_name env) #f)
              (add-to-env var_name var_val env)
              ;(error 'lookup-in-env (~a var_name ": Variable already declared!!"))
              (update-env! var_name var_val env)
              ))]
       [(varType varName) (println "Un-Initialized")])]
    
    ;handle assignment
    [({~literal assignment} varName varAsig-stx)
     ;(println "assignment")
     (define var_name (syntax-e #'varName))     
     (define var_val (interpret #'varAsig-stx env sourceStruct currentObject previousObject))
     (update-env! var_name var_val env)
     ]
    [({~literal assignment} assign-stx)
     ;(println "assignment")
     (interpret #'assign-stx env sourceStruct currentObject previousObject)]
    [({~literal logic_or} lOr-stx)
     ;(println "logic_or")
     (interpret #'lOr-stx env sourceStruct currentObject previousObject)]
    [({~literal logic_and} lAnd-stx)
     ;(println "logic_and")
     (interpret #'lAnd-stx env sourceStruct currentObject previousObject)]
    [({~literal equality} equality-stx ...)
     ;(println "equality")
     (syntax-case #'(equality-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (interpret #'arg1 env sourceStruct currentObject previousObject)]
              [num2 (interpret #'arg2 env sourceStruct currentObject previousObject)])
          (case (syntax-e #'sign)
             [("!=") (eq? #f (eq? num1 num2))]
             [("==") (eq? num1 num2)]
            ))]
       [(arg)
        (interpret #'arg env sourceStruct currentObject previousObject)
        ])]

    ;handle spliter-joiner intructions
    #|
    [({~literal splitjoinStmt} spltjn-stx spltjnType-stx)
     (println #'spltjnType-stx)
     (println (StreamObj-initFlag currentObject))
     (cond
       [(StreamObj-initFlag currentObject) (println "split-join already initialized")]
       [(string=? (syntax-e #'spltjn-stx) "split")
        (begin
          (if (string=? (syntax-e (second (syntax-e #'spltjnType-stx))) "duplicate")
              (set-StreamObj-splitJoinType! currentObject
                                            (append (StreamObj-splitJoinType currentObject)
                                                    (list DUPLICATE)))
              (set-StreamObj-splitJoinType! currentObject
                                            (append (StreamObj-splitJoinType currentObject)
                                                    (list ROUNDROBIN)))))]
       [(string=? (syntax-e #'spltjn-stx) "join")
        (begin
          (set-StreamObj-initFlag! currentObject #t)
          (if (string=? (syntax-e (second (syntax-e #'spltjnType-stx))) "duplicate")
              (set-StreamObj-splitJoinType! currentObject
                                            (append (StreamObj-splitJoinType currentObject)
                                                    (list DUPLICATE)))
              (begin
                (set-StreamObj-splitJoinType! currentObject
                                            (append (StreamObj-splitJoinType currentObject)
                                                    (list ROUNDROBIN)))
                (set-StreamObj-output! currentObject null)
                (for ([child (StreamObj-ChildrenList currentObject)])
                  (println child)
                  ;#|
                  (let ([childObject (functionLookUp child
                                      (StreamGraph-streamObjs sourceStruct))])
                    (println (StreamObj-output childObject))
                    (set-StreamObj-output! currentObject
                                           (append (StreamObj-output currentObject)(StreamObj-output childObject)))
                    (println (StreamObj-output currentObject)))
                  ;|#
                  ))))])
        (println (StreamObj-output currentObject))
        ]
    |#


  ;;;handle add inistructions
    [({~literal addStatement} addStmt-stx)
     ;(println (first (StreamObj-Type currentObject)))
     ;(println (StreamObj-Name currentObject))
     (let ([calledObject (functionLookUp
                            (syntax-e (second (syntax-e(second(syntax-e #'addStmt-stx)))))
                            (StreamGraph-streamObjs sourceStruct))])
       ;(println (StreamObj-input currentObject))
       ;(println (StreamObj-Name calledObject))
       (cond
         [(string=? (StreamObj-Type currentObject) "pipeline")  ;; in pipeline input_channel --->(in) 1st child (out) ---> input_channel 
          (begin
            (if (or (eq? (StreamObj-FirstChild currentObject) null)
                    (string=? (StreamObj-Name calledObject) (StreamObj-FirstChild currentObject))) 
                (begin
                  (println (StreamObj-input currentObject))
                  (set-StreamObj-input! calledObject (StreamObj-input currentObject))
                  
                  ;(println (StreamObj-input calledObject))
                  ;(println "here")
                  (set-StreamObj-FirstChild! currentObject (StreamObj-Name calledObject)))
                (set-StreamObj-input! calledObject (StreamObj-output (StreamObj-PreviousChild currentObject))))
            (set! previousObject (StreamObj-PreviousChild currentObject))   ;;; might be buggy
            (interpret #'addStmt-stx env sourceStruct currentObject previousObject)
            (set-StreamObj-PreviousChild! currentObject calledObject)
            (set-StreamObj-output! currentObject (StreamObj-output calledObject)))]
         [(string=? (StreamObj-Type currentObject) "splitjoin")
          (begin
            (set-StreamObj-ChildrenList! currentObject (append (StreamObj-ChildrenList currentObject)
                                                               (list (StreamObj-Name calledObject))))
            (cond
              [(= (first (StreamObj-splitJoinType currentObject)) DUPLICATE)
               (set-StreamObj-input! calledObject (StreamObj-input currentObject))]
              [(println "roundRobin")])
            (interpret #'addStmt-stx env sourceStruct currentObject)
            (println (StreamObj-output calledObject)))
          ]
         )
       (println (StreamObj-input currentObject))
       (println (StreamObj-output currentObject))
       )
     ;(set-StreamObj-childreName
     #|
     (cond
       [(string=? (first (StreamObj-Type currentObject)) "splitjoin")
        (if (= (second (StreamObj-Type currentObject)) DUPLICATE)
            (let ([calledfunction (functionLookUp
                                   (syntax-e (second (syntax-e(second(syntax-e #'addStmt-stx)))))
                                   (StreamGraph-streamObjs sourceStruct))])
              (set-StreamObj-input! calledfunction (StreamObj-input currentObject)) ;;; INCOMPLETE !!
              )
            (println "nothing")
            )])
     (interpret #'addStmt-stx env sourceStruct currentObject)
     |#
     
     ]

    ;handle comparision instructions
    [({~literal comparison} cmp-stx ...)
     ;(println "comparison")
     (syntax-case #'(cmp-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (interpret #'arg1 env sourceStruct currentObject previousObject)]
              [num2 (interpret #'arg2 env sourceStruct currentObject previousObject)])
          (case (syntax-e #'sign)
             [("<") (< num1 num2)]
             [(">") (> num1 num2)]
             [("<=") (<= num1 num2)]
             [(">=") (>= num1 num2)]
            ))
        ]
       [(arg)
        (interpret #'arg env sourceStruct currentObject previousObject)
        ]
       )]
    
    ; handle addition and subtruction
    [({~literal addition} add-stx ...)
     ;(println #'(add-stx ...))
     (syntax-case #'(add-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (interpret #'arg1 env sourceStruct currentObject previousObject)]
              [num2 (interpret #'arg2 env sourceStruct currentObject previousObject)])
          (case (syntax-e #'sign)
             [("+") (+ num1 num2)]
             [("-") (- num1 num2)]
            ))
        ]
       [(arg)
        (interpret #'arg env sourceStruct currentObject previousObject)]
       )]
    
    ;handle multiplication and division
    [({~literal multiplication} mult-stx ...)
     ;(println "multiplication")
     (syntax-case #'(mult-stx ...)()
       [(arg1 sign arg2)
        (let ([num1 (interpret #'arg1 env sourceStruct currentObject previousObject)]
              [num2 (interpret #'arg2 env sourceStruct currentObject previousObject)])
          (case (syntax-e #'sign)
             [("*") (* num1 num2)]
             [("/") (/ num1 num2)]
            ))
        ]
       [(arg)
        (interpret #'arg env sourceStruct currentObject previousObject)
        ])]

    ;handle unary operators: -x, x++, x--
    [({~literal unary} unary-stx ...)
     ;(println "unary")
     (syntax-case #'(unary-stx ...)()
       [(sign arg)
        (let ([num (interpret #'arg env sourceStruct currentObject previousObject)])
          (case (syntax-e #'sign)
             [("-") (- num)]
            ))
        ]
       [(arg)
        (interpret #'arg env sourceStruct currentObject previousObject)
        ])]

    ;handle function calls
    [({~literal call} call-stx ...)      ; problem here
     ;(println #'(call-stx ...))          ; dispatch issue
     (syntax-case #'(call-stx ...)()
       [(name brkt1 args brkt2)    ;procedure call with arguments
        (begin
          (let ([calledFunctionName (syntax-e(second(syntax-e #'name)))])
            (cond
              [(string=? calledFunctionName "push")
               (let ([pushRate (binding-value (lookup-in-env "pushRate" env))])
                 ;(println pushRate)
               (cond
                 [(> pushRate 0)
                  (begin
                    (update-env! "pushRate" (- pushRate 1) env)
                    ;(println (StreamObj-Name currentObject))
                    (set-StreamObj-output! currentObject
                                         (append (StreamObj-output currentObject)
                                                 (list (interpret (second(syntax-e #'args)) env sourceStruct currentObject previousObject))))
                    ;(println (StreamObj-output currentObject))
                    )]
                 [(error 'push "can't push items more than the limit!!")]))]
              [(string=? calledFunctionName "peek")
               (let ([peekRate (binding-value (lookup-in-env "peekRate" env))])
                 (println peekRate)
                 (cond
                   [(> peekRate 0)
                    (begin
                      (update-env! "peekRate" (- peekRate 1) env)
                      (list-ref (StreamObj-input currentObject)
                                (interpret (second(syntax-e #'args)) env sourceStruct currentObject previousObject)))]
                   [(error 'peek "can't peek items more than the limit!!")]))]
              [(begin
                 (let ([calledfunction (functionLookUp
                                        (syntax-e(second(syntax-e #'name)))
                                        (StreamGraph-streamObjs sourceStruct))]
                       [listOfArgs null])
                   (for ([eachArg-stx (rest (syntax-e #'args))])
                     (set! listOfArgs (append listOfArgs (list (interpret eachArg-stx env sourceStruct currentObject previousObject)))))
                   ;(println listOfArgs)
                   ;(println (StreamObj-parameters calledfunction))
                   ;(println (StreamObj-initFlag calledfunction))
                   (if (eq? (StreamObj-initFlag calledfunction) #f)
                       (begin
                         (set-StreamObj-initFlag! calledfunction #t)
                         (extend-env (StreamObj-parameters calledfunction) listOfArgs (StreamObj-localEnv calledfunction)))
                       (begin
                         (for ([arg listOfArgs])
                           (println arg))))
                   ;(println env)
                   ;(println (lookup-in-env "a" env))
                   (interpret (StreamObj-body calledfunction) (StreamObj-localEnv calledfunction) sourceStruct calledfunction  previousObject)
                   )
                 )]
              )))]
       [(name brkt1 brkt2)         ;procedure call without arguments
        (begin
          (let ([calledFunctionName (syntax-e(second(syntax-e #'name)))])
            (cond
              [(string=? calledFunctionName "pop")
               (let ([popRate (binding-value (lookup-in-env "popRate" env))])
                 ;(println popRate)
                 (cond
                   [(> popRate 0)
                    (begin
                      (update-env! "popRate" (- popRate 1) env)
                      ;(println previousObject)
                      (let ([numberPopped (first(StreamObj-input currentObject))])
                        (set-StreamObj-input! currentObject (rest(StreamObj-input currentObject)))
                        (set-StreamObj-output! previousObject (rest(StreamObj-output previousObject)))
                        ;(println (StreamObj-input currentObject))
                        numberPopped))]
                   [(error 'pop "can't pop items more than the limit!!")]))]
              [(begin
                 (let ([calledfunction (functionLookUp
                                        (syntax-e(second(syntax-e #'name)))
                                        (StreamGraph-streamObjs sourceStruct))])
                   (if (eq? (StreamObj-initFlag calledfunction) #f) ;; should not depend on initflag
                       (begin
                         (extend-env '() '() env)
                         ;(set-StreamObj-initFlag! calledfunction #t)
                         )
                       (println "initialized")
                       )
                   (interpret (StreamObj-body calledfunction) (StreamObj-localEnv calledfunction) sourceStruct calledfunction previousObject)
                   )
                 )])))]
       [(name) (interpret #'name env sourceStruct currentObject previousObject)])]

    
    [({~literal primary} primary-stx)
     ;(println "primary")
     ;(println (syntax-e #'primary-stx))
     (cond
       [(number? (syntax-e #'primary-stx)) (syntax-e #'primary-stx)]
       [(string? (syntax-e #'primary-stx))
        (let [(printStr (syntax-e #'primary-stx))]
                (cond
                  [(and (eq? (first (string->list printStr)) #\")
                        (eq? (last (string->list printStr)) #\"))
                   (substring printStr 1 (- (string-length printStr) 1))]
                  [(if (eq? (lookup-in-env printStr env) #f)
                       (cond
                         [(string=? "push" printStr) printStr]
                         [(string=? "pop" printStr) printStr]   ; handle printing peek/pop
                         [(string=? "peek" printStr) printStr]
                         [(error 'print (~a "Undefined variable:" printStr))])
                       (binding-value (lookup-in-env printStr env)))
                       ]
                  ))])]

    ;handle if-else
    [({~literal ifStmt} branching-stx ...)
     (syntax-case #'(branching-stx ...)()
       [(condStmt-stx ifBlk-stx elseBlk-stx)
        (if (interpret #'condStmt-stx env sourceStruct currentObject previousObject)
            (interpret #'ifBlk-stx env sourceStruct currentObject previousObject)
            (interpret #'elseBlk-stx env sourceStruct currentObject previousObject))
        ]
       [(condStmt-stx ifBlk-stx)
        (cond [(interpret #'condStmt-stx env sourceStruct currentObject previousObject)
               (interpret #'ifBlk-stx env sourceStruct currentObject previousObject)])
        ]
       )]
    [({~literal exprStmt} exprStmt-stx)
     ;(println "in Expression statement")
     (interpret #'exprStmt-stx env sourceStruct currentObject previousObject)]    
    ;variable declaration handling
    ;handling expression
    [({~literal expression} expr-stx)
     ;(println "Expression")
     (interpret #'expr-stx env sourceStruct currentObject previousObject)]
    [({~literal operator} operator-string)
     ;(display (syntax->datum #'operator-string))
     #'operator-string]
    [({~literal literal} value) #'value]))


;; handles function lookup from the enviroment
(define (functionLookUp funcName functionList)
  ;(println (string=? (StreamObj-Name (first functionList)) funcName))
  ;#|
  (cond
    [(if (string=? (StreamObj-Name (first functionList)) funcName)
      (first functionList)
      (functionLookUp funcName (rest functionList)))]
    [else (error 'functionLookUP (~a funcName ":Not defined !!"))])
  ;|#
  )