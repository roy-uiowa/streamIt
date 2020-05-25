#lang racket
(provide (all-defined-out))
(struct StreamGraph (globalEnv graphSchedular streamObjs objectList functionList) #:mutable #:transparent)
(struct Function  (fName fparameters fbody) #:mutable #:transparent)
;object_name | object_type(filter/pipeline/splitjoin)| input_buffer | output_buffer | ititialized_or_not | argument_list |
(struct StreamObj (Name Type input output initFlag parameters body localEnv ChildrenList FirstChild PreviousChild splitJoinType) #:mutable #:transparent)
;(struct StreamPipeLine (Name Type input output initFlag parameters body localEnv FirstChild PreviousChild splitJoinType) #:mutable #:transparent)
;(struct StreamSplitJoin (Name Type input output initFlag parameters body localEnv ChildrenList splitJoinType) #:mutable #:transparent)
;(struct StreamFilter (Name Type input output initFlag parameters body localEnv) #:mutable #:transparent)

(define DUPLICATE 0)
(define ROUNDROBIN 1)
(struct Scheduler (initFlag initSched steadySched) #:mutable #:transparent)
(struct  PipeLine (sourceName sinkName))

; An environment is a sequence of frames.
(struct environment (frames) #:mutable #:transparent)
; Each frame is a table (possibly empty) of bindings
(struct frame (bindings parent) #:mutable #:transparent)
(struct binding (key value) #:mutable #:transparent)

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