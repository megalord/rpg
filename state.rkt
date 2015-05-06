#lang racket

(require pict3d)

(provide init-state
         hash-set-in
         hash-get-in)

(define (hash-set-in ht ks v)
  (if (empty? (cdr ks))
    (hash-set! ht (car ks) v)
    (hash-set-in (hash-ref ht (car ks)) (cdr ks) v)))

(define (hash-get-in ht ks)
  (let ((val (hash-ref ht (car ks))))
    (if (empty? (cdr ks))
      val
      (hash-get-in val (cdr ks)))))

(define init-state
  (let ((ht (make-hash)))
        (hash-set! ht "prev-time" 0)
        (hash-set! ht "position" (pos 1 1 1))
        (hash-set! ht "direction" (dir -1 -1 0))
        (hash-set! ht "keys" (make-hash))
        (hash-set-in ht '("keys" "w") #f)
        (hash-set-in ht '("keys" "a") #f)
        (hash-set-in ht '("keys" "s") #f)
        (hash-set-in ht '("keys" "d") #f)
        (hash-set-in ht '("keys" "q") #f)
        (hash-set-in ht '("keys" "e") #f)
        ht))

