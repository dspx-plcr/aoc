(import
  (srfi srfi-1)
  (srfi srfi-11)
  (srfi srfi-13)
  (srfi srfi-14)
  (srfi srfi-27)
  (srfi srfi-43)
  (rnrs arithmetic bitwise (6))
  (rnrs hashtables (6))
  (rnrs lists (6))
  (rnrs io ports (6))
  (rnrs io simple (6))
  (rnrs records syntactic (6))
  (rnrs sorting (6)))

(define ls #f)

(define (read-lines)
  (let loop ((ls' '()))
    (if (port-eof? (current-input-port))
      (set! ls ls')
      (let* ((line (get-line (current-input-port)))
             (nums (map string->number (string-tokenize line char-set:digit))))
        (loop (cons nums ls'))))))

(with-input-from-file "in.txt"
  (lambda ()
    (read-lines)))

(define (list-index lst idx)
  (let loop ((i 0)
	     (rem lst))
    (if (= i idx)
      (car rem)
      (loop (+ 1 i) (cdr rem)))))

(define (it-value base num i)
  (let loop ((j 0)
	     (mod base)
	     (rem num))
    (if (= i j)
      (/ (modulo rem mod) (/ mod base))
      (loop (+ 1 j) (* mod base) (- rem (modulo rem mod))))))

(define (try-ith nums i n ops)
  (let loop ((acc (car nums))
             (j 0)
             (rem (cdr nums)))
    (if (= n j)
      acc
      (loop
        (apply (list-index ops (it-value (length ops) i j))
	       (list acc (car rem)))
        (+ 1 j)
        (cdr rem)))))

(define (find-sum ls ops)
  (define target (car ls))
  (define nums (cdr ls))
  (define n (- (length nums) 1))
  (let loop ((i 0))
    (cond
     ((= i (expt (length ops) n)) 0)
     ((= target (try-ith nums i n ops)) target)
     (else (loop (+ 1 i))))))

(define (go1)
  (fold-left (lambda (a e) (+ a (find-sum e (list + *)))) 0 ls))

(define (concat x y)
  (string->number (string-append (number->string x) (number->string y))))

(define (go2)
  (fold-left (lambda (a e) (+ a (find-sum e (list + * concat)))) 0 ls))

(display "Sum: ")
(display (go1))
(newline)
(display "Sum: ")
(display (go2))
(newline)
