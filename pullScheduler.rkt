#lang racket
(require graph)
(require dyoo-while-loop)
; 0a2 ---> 1b1 ---> 1c2 ---> 3d1 ---> 3e1
; a10      b10      c9  d1 e1
;aaaaa bbbbbbbbbb ccccc ddd e

(define g (unweighted-graph/directed '(
                                       (a b)
                                       (b c)
                                       (c d)
                                       (d e)
                                       )))
(define-vertex-property g pRate #:init 0)
(define-vertex-property g cRate #:init 0)
(pRate-set! 'a 2) ;production rate
(cRate-set! 'a 0) ;consumption rate

(pRate-set! 'b 1)
(cRate-set! 'b 1)

(pRate-set! 'c 2)
(cRate-set! 'c 1)

(pRate-set! 'd 1)
(cRate-set! 'd 3)

(pRate-set! 'e 1)
(cRate-set! 'e 3)
(define (get-parent node graph)
  (map first (filter (lambda (edge)
                       (equal? node (second edge))) (get-edges graph))))
(define (pullSchedule node graph phi)
    (let [ (input_channels (flatten (filter (lambda (edge)
                                    (equal? node (second edge))) (get-edges graph))))]
      (cond
        [(empty? input_channels) (set! phi (append phi  (list node))) phi]
        [else
         (let [(input_node (first input_channels))
               (item_need (cRate node))
               (tmp null)]
           (while (> item_need 0)
                  (set! phi (pullSchedule input_node graph phi))
                  (set! item_need (- item_need (pRate input_node)))
                  (set! phi (append phi (list input_node)))
                  ))
         phi
         ])))


(get-edges g)

(define phi null)
(let ([x (pullSchedule 'c g phi)])
  (println x))
