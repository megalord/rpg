#lang racket

(require pict3d
         racket/gui
         "parser/obj.rkt")

(define frame (new frame% (label "Test")  (width 800)  (height 600)))
(define canvas (new pict3d-canvas%
                    (parent frame)))

(send frame show #t)

(define axes
  (combine
    (with-emitted (emitted "cyan" 2) (arrow origin +x))
    (with-emitted (emitted "magenta" 2) (arrow origin +y))
    (with-emitted (emitted "yellow" 2) (arrow origin +z))))

(define (normalize ns)
  (let ((scale (/ 1 (apply + ns))))
    (map (λ (n) (* n scale)) ns)))

(define plane
  (quad
    (vertex (pos 5 5 0) #:color (rgba 0.08 0.3 0.04))
    (vertex (pos -5 5 0) #:color (rgba 0.08 0.3 0.04))
    (vertex (pos -5 -5 0) #:color (rgba 0.08 0.3 0.04))
    (vertex (pos 5 -5 0) #:color (rgba 0.08 0.3 0.04))))

;(define plane
;  (quad
;    (pos 5 5 0)
;    (pos -5 5 0)
;    (pos -5 -5 0)
;    (pos 5 -5 0)))

(send canvas set-pict3d
  (combine
    axes
    (sunlight (dir 0 0 -1))
    plane
    (basis 'camera (point-at (pos 3 3 3) origin))
    (light (pos -3 -3 3))))
