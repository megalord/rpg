#lang racket
 
(require pict3d
         pict3d/universe
         "movement.rkt"
         "state.rkt"
         "parser/obj.rkt")
 
(define (set-time-delta! s t)
  (hash-set! s "dt" (- t (hash-ref s "prev-time")))
  (hash-set! s "prev-time" t))

(define (on-frame s n t)
  (set-time-delta! s t)
  (move-self s)
  s)

(define (camera s)
  (basis 'camera (point-at
                   (hash-ref s "position") 
                   (hash-ref s "direction"))))

(define axes
  (combine
    (with-emitted (emitted "cyan" 2) (arrow origin +x))
    (with-emitted (emitted "magenta" 2) (arrow origin +y))
    (with-emitted (emitted "yellow" 2) (arrow origin +z))))

(define mountain (rotate (parse "mountain.obj") +x+y+z 120))

(define (on-draw s n t)
  (combine 
    mountain
    axes
    lights
    (sunlight (dir 0 0 -1) (emitted "white" 5))
    (camera s)))
 
(define (key-setter v)
  (λ (s n t k)
     (hash-set-in s (list "keys" k) v)
     s))

(big-bang3d init-state
            #:on-frame on-frame
            #:on-draw on-draw
            #:on-key (key-setter #t)
            #:on-release (key-setter #f))
