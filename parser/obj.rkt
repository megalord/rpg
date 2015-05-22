#lang racket

(require pict3d
         "file.rkt"
         "mtl.rkt")

(provide parse)

; first element is null so that lists are one-indexed
(define vertices '(()))
(define normals '(()))
(define faces '())

(define (set-obj! name)
  (void))

(define (def-vertex! xyz)
  (set! vertices
    (append vertices
            (list (apply pos xyz)))))

(define (def-normal! xyz)
  (set! normals
    (append normals
            (list (apply dir xyz)))))

; minimum of 3
(define triangle-fan
  (λ vertices
    (let ((fst (car vertices))
          (rst (rest vertices)))
      (combine
        (triangle fst (car rst) (cadr rst))
        (if (= (length rst) 2)
          '()
          (apply triangle-fan (cons fst (rest rst))))))))

(define (create-face points)
  (apply
    (case (length points)
      ((3) triangle)
      ((4) quad)
      (else triangle-fan))
    points))

(define (parse-points points)
  (for/list ((point points))
    (string-split point "/")))

(define curr-material '())

(define (create-vertex point)
  (vertex
    (list-ref vertices (car point))
    #:normal (list-ref normals (caddr point))
    #:color (car curr-material)
    #:material (cadr curr-material)))

(define (def-face! points)
  (set! faces
    (append faces
            (list
              (create-face
                (map
                  (compose create-vertex strings->numbers)
                  (parse-points points)))))))

(define (use-mtl! args)
  (set! curr-material (get-material (car args))))

(define (parser key)
  (case key
    (("v") (compose def-vertex! strings->numbers))
    (("vn") (compose def-normal! strings->numbers))
    (("f") def-face!)
    (("o") set-obj!)
    (("mtllib") (compose parse-mtl! car))
    (("usemtl") use-mtl!)
    (("#" "s") void))) 

(define (parse file)
  (parse-with file parser)
  (apply combine faces))
