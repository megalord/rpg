#lang racket

(provide parse-with strings->numbers)

(define (strings->numbers lst)
  (map string->number lst))

(define (create-line-parser f)
  (λ (line)
     (unless (string=? line "")
       (let ((lst (string-split line)))
         ((f (car lst))
          (cdr lst))))))

(define (read-lines file f)
  (file->list file
              (λ (inp)
                (let ((line (read-line inp)))
                  (if (eof-object? line)
                    line
                    (f line))))
              #:mode 'text))

(define (parse-with file f)
  (read-lines file (create-line-parser f)))
