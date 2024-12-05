(import
  (srfi srfi-1)
  (srfi srfi-11)
  (srfi srfi-13)
  (srfi srfi-14)
  (srfi srfi-27)
  (rnrs lists (6))
  (rnrs io ports (6))
  (rnrs io simple (6))
  (rnrs sorting (6)))

(define (parse-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lst '()))
        (if (port-eof? (current-input-port))
          lst
          (loop (cons 
                  (map string->number
                       (string-tokenize
                         (get-line (current-input-port))
                         char-set:digit))
                  lst)))))))

(define (sliding lst amt)
  (let loop ((rem lst)
             (windows '()))
    (if (< (- (length rem) amt) 0)
      (reverse windows)
      (loop (cdr rem) (cons (take rem amt) windows)))))

(define (small-change? pair)
  (define diff (abs (- (car pair) (cadr pair))))
  (and (<= diff 3) (<= 1 diff)))

(define (gradual? report)
  (for-all small-change? (sliding report 2)))

(define (monotonic? report)
  (define (helper rem cmp)
    (cond
     ((null? rem) #t)
     ((cmp (caar rem) (cadar rem)) (helper (cdr rem) cmp))
     (else #f)))
  (if (>= 2 (length report))
    #t
    (let ((rem (sliding report 2)))
      (helper rem (if (< (caar rem) (cadar rem)) < >)))))

(define (actually-safe? report)
  (and (gradual? report) (monotonic? report)))

(define (missing ls i)
  (if (= 0 i)
    (cdr ls)
    (append (take ls i) (list-tail ls (+ i 1)))))

(define (dampened-safe? report)
  (let loop ((i 0)
             (rem '())) 
    (if (>= i (length report))
      (exists actually-safe? rem)
      (loop (+ i 1) (cons (missing report i) rem)))))

(define (safe? report)
  (or (actually-safe? report) (dampened-safe? report)))

(define ls (parse-input "in.txt"))

(display "Count: ")
(display (count safe? ls))
(newline)
(display "?: ")
(display "")
(newline)
