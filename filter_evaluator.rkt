#lang racket
(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require graph)
(require "new_environment.rkt")
(require "graph_functions.rkt")
(provide all-defined-out)
(provide evaluate-filter)

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