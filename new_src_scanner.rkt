#lang racket
(require syntax/parse)
(require dyoo-while-loop)
(require br/list)
(require graph)
(provide makeStructs)
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
