(import
  (srfi srfi-1)
  (srfi srfi-13)
  (srfi srfi-14)
  (rnrs lists (6))
  (rnrs io ports (6))
  (rnrs io simple (6))
  (rnrs sorting (6)))

(define (parse-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((left '())
                 (right '()))
        (if (port-eof? (current-input-port))
          (cons left right)
          (let* ((in (string-tokenize
                       (get-line (current-input-port))
                       char-set:digit))
                 (l (string->number (car in)))
                 (r (string->number (cadr in))))
            (loop (cons l left) (cons r right))))))))

(define ls (parse-input "in.txt"))
(define (sort xs) (list-sort < xs))
(define left (sort (car ls)))
(define right (sort (cdr ls)))

(display "Difference: ")
(display (apply + (map (lambda (x) (abs (apply - x))) (zip left right))))
(newline)
(display "Similarity: ")
(display (apply +
           (map (lambda (x)
                (* x (length (filter (lambda (y) (= x y)) right))))
                left)))
(newline)
