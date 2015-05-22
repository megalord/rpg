#lang racket

(require pict3d
         "file.rkt")

(provide get-material
         parse-mtl!)

(define materials (make-hash))

(define (get-material name)
  (hash-ref materials name))

(define current-name "")
(define curr-material '(() ()))
(define curr-color '())

(define (create-mtl!)
  (unless (string=? current-name "")
    (hash-set! materials current-name
               (list
                 curr-color
                 (keyword-apply material
                                (reverse (car curr-material))
                                (reverse (cadr curr-material))
                                '())))
    (set! curr-material '(() ()))))

(define (new-mtl! lst)
  (create-mtl!)
  (set! current-name (car lst)))

(define (average numbers)
  (/ (apply + numbers) (length numbers)))

(define (material-adder k)
  (λ (lst)
     (set! curr-material
       (list
         (cons k (car curr-material))
         (cons (average lst) (cadr curr-material))))))

(define (set-diffuse! lst)
  (set! curr-color lst)
  lst)

(define (set-weighted! lst)
  (void))

(define (set-dissolved! lst)
  (set! curr-color
    (apply rgba (append curr-color lst))))

(define (set-illumination! lst)
  (void))


(define (parser key)
  (case key
    (("newmtl") new-mtl!)
    (("Ns") void) ; specular exponent
    (("Ka") (compose (material-adder '#:ambient) strings->numbers))
    (("Kd") (compose (material-adder '#:diffuse) set-diffuse! strings->numbers))
    (("Ks") (compose (material-adder '#:specular) strings->numbers))
    (("Ni") void) ; optical density (index of refraction)
    (("d") (compose set-dissolved! strings->numbers)) ; transparency
    (("illum") void) ; illumination model
    (("#") void)))

(define (parse-mtl! file)
  (parse-with file parser)
  (create-mtl!)
  null)
