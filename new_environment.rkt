#lang racket
(provide (all-defined-out))

(struct StreamGraph (RootObject
                     objectList
                     StreamPiplines
                     StreamFilters
                     StreamSplitJoiners) #:mutable #:transparent)

                     
(struct StreamPipeLine (Name
                        Parameters
                        Body) #:mutable #:transparent)

(struct StreamFilter (Name
                      Parameters
                      PreWork
                      Work
                      FunctionList
                      Body) #:mutable #:transparent)

(struct StreamSplitJoin (Name
                         Parameters
                         Body
                         SplitType
                         SplitRate
                         JoinType
                         JoinRate) #:mutable #:transparent)

(struct StreamFilterFunctions (Name
                              Parameters
                              Body) #:mutable #:transparent)

; An environment is a sequence of frames.
(struct environment (frames) #:mutable #:transparent)
; Each frame is a table (possibly empty) of bindings
(struct frame (bindings parent) #:mutable #:transparent)
(struct binding (key value) #:mutable #:transparent)
#|
(define (local-env node)  
  (cond
    [(StreamPipeLine? node) (let ([env (StreamPipeLine-LocalEnv node)]) env)]
    [(StreamSplitJoin? node) (let ([env (StreamSplitJoin-LocalEnv node)]) env)]
    [(StreamPipeLine? node) (let ([env (StreamFilter-LocalEnv node)]) env)]))
(define (update-node-env node env)  
  (cond
    [(StreamPipeLine? node) (set-StreamPipeLine-LocalEnv! (StreamPipeLine-LocalEnv node) env)]
    [(StreamSplitJoin? node) (set-StreamSplitJoin-LocalEnv! (StreamSplitJoin-LocalEnv node) env)]
    [(StreamPipeLine? node) (set-StreamSplitJoin-LocalEnv! (StreamFilter-LocalEnv node) env)]))
|#
(define (lookup-in-env key env)
  (match env
    [(environment frames)
     (lookup-in-frames key frames)]))

(define (lookup-in-frames key frames)
  (match frames
    ['()         #f] ; unbound
    [(cons f fs) (or (lookup-in-frame  key f)
                     (lookup-in-frames key fs))]))

(define (lookup-in-frame key f)
  (match f
    [(frame bindings parent)
     (lookup-in-bindings key bindings)]))

(define (lookup-in-bindings key bindings)
  (match bindings
    ['()         #f] ; unbound
    [(cons b bs) (if (string=? key (binding-key b))
                     b  ; binding with key-value paring
                     (lookup-in-bindings key bs))]))

(define (add-frame-to-env! f env)
  (match env
    [(environment frames)
     (set-environment-frames! env (cons f frames))]))
(define (remove-local-env-frame! env)
  (match env
    [(environment frames)
     (set-environment-frames! env (rest frames))]))

(define (update-env! key value env)
  (let ([b (lookup-in-env key env)])
    (if b
        (set-binding-value! b value)
        (error 'update-env! (~a "no binding for " key)))))

(define (extend-env keys values env)
  (match env
    [(environment (cons top-frame frames))
     (define bs (map binding keys values))
     (define new-f (frame bs top-frame))
     (set-environment-frames! env (cons new-f (cons top-frame frames)))]))



(define (add-to-env key value env)
  (match env
    [(environment (cons currentFrame frames))
     (match currentFrame
       [(frame bindings parent)
        (define bs (binding key value))
        (define newBs (append (list bs) bindings))
        (set-frame-bindings! currentFrame newBs)
        ])]))


(define global-env (environment (list (frame '() #f))))
(add-to-env "x" 3 global-env)
(let ([local-frame (frame '() global-env)])
  (add-frame-to-env! local-frame global-env)
  (add-to-env "y" 5 global-env))
;(remove-local-env-frame! global-env)
;(lookup-in-env '+ global-env) ; #f since plus is unbound
;(extend-env '(+ - * /)    (list + - * /)    global-env)
;(lookup-in-env '+ global-env)
;(lookup-in-env '- global-env)
;(extend-env (list "tkr") (list 5) global-env)
;(add-to-env  "roy"  5 global-env)
#|
(define global-env (environment (list (frame '() #f))))
(define streamSchedule (Scheduler null null))
(define streamObjects (list))
(define sourceStructure (StreamGraph global-env
                                 streamSchedule
                                 streamObjects))

|#
;(set-Scheduler-initSched! (StreamGraph-graphSchedular sourceStructure) "tkr")
#|
(StreamGraph-streamObjs sourceStructure)
(define newObj (StreamObj null null null null null null null))
(set-StreamObj-body! newObj "howdy")
(define newObjList (append (StreamGraph-streamObjs sourceStructure) (list newObj)))
(set-StreamGraph-streamObjs! sourceStructure newObjList)
|#
;(define x (PipeLine #f #f))


;(struct myEnviroment (bindings myfilters))
;(struct filter (bindings))
;(struct myBinding (key value))