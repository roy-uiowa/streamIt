#lang racket
(require graph)
(require "new_environment.rkt")
(provide (all-defined-out))

(define (get-parent node graph)
  (map first (filter (lambda (edge)
                            (equal? node (second edge))) (get-edges graph))))

(define (first-kid? node graph)
  (let ([siblings (kids (first (get-parent node graph)) graph)])
    (cond
      [(equal? siblings null) #t]
      [(equal? (last siblings) node) #t]
      [else #f])))

(define (last-kid? node graph)
  (let ([siblings (kids (first (get-parent node graph)) graph)])
    (cond
      [(equal? siblings null) #t]
      [(equal? (first siblings) node) #t]
      [else #f])))

(define (kids n graph)
  (get-neighbors graph n))

(define (get-input-channel node graph)
  (flatten (first (filter (lambda (edge) (equal? node (second edge)))
                   (get-edges graph)))))

(define (get-output-channel node graph)
  (flatten (filter (lambda (edge) (equal? node (first edge)))
                   (get-edges graph))))

(define (show-channel-data edge)
  (let ([data (flatten (channel (first edge) (second edge)))])
        data))

(define (push value node graph)
  (let* ([out-ch (get-output-channel node graph)]
         [data-ch (channel (first out-ch) (second out-ch))])
    (channel-set! (first out-ch)
                  (second out-ch)
                  (append data-ch (list value)))))

(define (pop-channel-data node1 node2 graph)
  (let ([popped_item (first (channel node1 node2))])
    (channel-set! node1 node2 (rest (channel node1 node2)))
    popped_item))

(define (append-to-channel data node1 node2 graph)
  (channel-set! node1 node2 (append (channel node1 node2) (list data))))

(define (pop node graph)
  (let* ([input-ch (get-input-channel node graph)]
         [data-ch (flatten (channel (first input-ch) (second input-ch)))]
         [popped-item (first data-ch)])
    (channel-set! (first input-ch)
                  (second input-ch)
                  (rest data-ch))
    popped-item))

(define (channel-empty? edge)
  (cond
    [(equal? (channel (first edge) (second edge)) null) #t]
    [else #f]))

(define programGraph (unweighted-graph/directed null))
(define-edge-property programGraph channel #:for-each null)
(define-vertex-property programGraph context #:init (environment (list (frame '() #f))))
(define-vertex-property programGraph childList #:init null)
(define-vertex-property programGraph objectName #:init #f)
;(define-vertex-property programGraph input #:init null)
;(define-vertex-property programGraph output #:init null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(define node1 '(A1 B2 C3))
(define node2 '(D4 E5 F6 G6))
(define node3 '(H6 I6))
(define node4 '(S))

(define g (unweighted-graph/undirected null))
(define-edge-property g channel #:for-each 0)
(get-vertices g)

(define (make-graph node graph)
  (define previous #f)
  (for ([n node])
    (add-vertex! graph n)
    (cond
      [(equal? previous #f) #t]
      [else (add-directed-edge! graph previous n)
            (channel-set! previous n null)]
      )
    (println (get-edges g))
    (set! previous n)))

(make-graph node1 g)

(get-vertices g)
(get-edges g)
(push 2 'B2 g)
;(pop 'C3 g)
(channel-empty? (get-output-channel 'B2 g))

(define tree '((A (B C D)) (B (E F)) (F (G H))))
(println tree)

|#