#lang racket

(require pict3d
         racket/gui
         "parser.rkt")

(define frame (new frame% (label "Test")  (width 800)  (height 600)))
(define canvas (new pict3d-canvas%
                    (parent frame)))

(send frame show #t)

(define axes
  (combine
    (with-emitted (emitted "cyan" 2) (arrow origin +x))
    (with-emitted (emitted "magenta" 2) (arrow origin +y))
    (with-emitted (emitted "yellow" 2) (arrow origin +z))))

(define mountain (parse "mountain.obj"))

(send canvas set-pict3d
  (combine
    axes
    (rotate mountain +x+y+z 120)
    (basis 'camera (point-at (pos 2 2 2) (dir -2 -2 -2)))
    (light (pos -1 -1 1))))
