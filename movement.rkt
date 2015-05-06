#lang racket

(require pict3d
         "state.rkt")

(provide move-self)

; units per 1000 ms
(define time-scale 2/1000)

(define (perpendicular dv0 #:clockwise? clockwise?)
  (match-define (dir dx dy dz) dv0)
  (let ((dv (dir-normalize
              (dir (- dy) dx 0))))
    (if clockwise? dv (dir-negate dv))))

(define (turn direction scale #:clockwise? clockwise?)
  (dir-normalize
    (dir+ direction
          (dir-scale
            (perpendicular direction #:clockwise? clockwise?)
            scale))))

(define (move-self s)
  (let ((dt (hash-ref s "dt"))
        (position (hash-ref s "position"))
        (direction (hash-ref s "direction"))
        (w? (hash-get-in s '("keys" "w")))
        (a? (hash-get-in s '("keys" "a")))
        (s? (hash-get-in s '("keys" "s")))
        (d? (hash-get-in s '("keys" "d")))
        (q? (hash-get-in s '("keys" "q"))) 
        (e? (hash-get-in s '("keys" "e")))) 
    (when (xor w? s?)
      (hash-set! s "position" (pos+ position direction (* time-scale dt (if w? 1 -1)))))
    (when (xor a? d?)
      (hash-set! s "direction" (turn direction (* time-scale dt) #:clockwise? a?)))
    (when (xor q? e?)
      (hash-set! s "position" 
                 (pos+ (hash-ref s "position")
                       (perpendicular direction #:clockwise? q?)
                       (* time-scale dt))))))
