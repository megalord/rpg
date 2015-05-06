#lang racket

(require pict3d)

(provide parse)

(define vertices '(#f))
(define normals '())
(define faces '())

(define (set-obj! name)
  (void))

(define (def-vertex! x y z)
  (set! vertices
    (append vertices 
            (list (pos
                    (string->number x)
                    (string->number y)
                    (string->number z))))))

(define (def-normal! x y z)
  (void))

; minimum of 3
(define triangle-fan
  (lambda vertices
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

(define def-face!
  (λ face
     (set! faces
       (append faces
               (list
                 (create-face
                   (for/list ((point face))
                     (let ((v (list-ref (string-split point "/") 0)))
                       (list-ref vertices (string->number v))))))))))

(define (import-mtl! file)
  (void))

(define (use-mtl! name)
  (void))

(define (noop)
  (void))

(define (parse-line line)
  (let ((lst (string-split line)))
    (apply 
      (case (car lst)
        (("v") def-vertex!)
        (("vn") def-normal!)
        (("f") def-face!)
        (("o") set-obj!)
        (("mtllib") import-mtl!)
        (("usemtl") use-mtl!)
        (("#" "s") void))
      (cdr lst))))

;(file->list "state.rkt" (lambda (x) (read-line x)) #:mode 'text)

(define (parse file)
  (file->list file 
              (lambda (inp)
                (let ((line (read-line inp)))
                  (if (eof-object? line)
                    line
                    (parse-line line))))
              #:mode 'text)
  (apply combine faces))
