(import
  (srfi srfi-1)
  (srfi srfi-11)
  (srfi srfi-13)
  (srfi srfi-14)
  (srfi srfi-27)
  (rnrs lists (6))
  (rnrs io ports (6))
  (rnrs io simple (6))
  (rnrs records syntactic (6))
  (rnrs sorting (6)))

(define grid
  (with-input-from-file "in.txt"
    (lambda ()
      (let loop ((grid '()))
	(if (port-eof? (current-input-port))
	  (list->vector (reverse grid))
	  (loop (cons
		 (list->vector (string->list (get-line (current-input-port))))
		 grid)))))))

(define dirs '((0 . -1) (1 . -1) (1 . 0) (1 . 1) (0 . 1) (-1 . 1) (-1 . 0) (-1 . -1)))
(define (xmas? i j)
  (lambda (dir)
    (define di (car dir))
    (define dj (cdr dir))
    (define i-bnd (+ i (* 3 di)))
    (define j-bnd (+ j (* 3 dj)))
    (and
      (>= j-bnd 0) (< j-bnd (vector-length grid))
      (>= i-bnd 0) (< i-bnd (vector-length (vector-ref grid j)))
      (eq? (vector-ref (vector-ref grid j) i) #\X)
      (eq? (vector-ref (vector-ref grid (+ j dj)) (+ i di)) #\M)
      (eq? (vector-ref (vector-ref grid (+ j (* 2 dj))) (+ i (* 2 di))) #\A)
      (eq? (vector-ref (vector-ref grid (+ j (* 3 dj))) (+ i (* 3 di))) #\S))))

(define (go1)
  (let loop ((i 0)
	     (j 0)
	     (n 0))
    (if (= j (vector-length grid))
      n
      (if (= i (vector-length (vector-ref grid j)))
	(loop 0 (+ 1 j) n)
	(loop (+ 1 i) j
          (+ n (length (filter (xmas? i j) dirs))))))))

(define (admissable i1 j1 i2 j2)
  (or
    (and (eq? (vector-ref (vector-ref grid j1) i1) #\M)
         (eq? (vector-ref (vector-ref grid j2) i2) #\S))
    (and (eq? (vector-ref (vector-ref grid j1) i1) #\S)
         (eq? (vector-ref (vector-ref grid j2) i2) #\M))))
(define (x-mas? i j)
  (and
    (< 0 j) (< (+ j 1) (vector-length grid))
    (< 0 i) (< (+ i 1) (vector-length (vector-ref grid j)))
    (eq? (vector-ref (vector-ref grid j) i) #\A)
    (admissable (- i 1) (- j 1) (+ i 1) (+ j 1))
    (admissable (- i 1) (+ j 1) (+ i 1) (- j 1))))

(define (go2)
  (let loop ((i 0)
	     (j 0)
	     (n 0))
    (if (= j (vector-length grid))
      n
      (if (= i (vector-length (vector-ref grid j)))
	(loop 0 (+ 1 j) n)
	(loop (+ 1 i) j
          (+ n (if (x-mas? i j) 1 0)))))))

(display "Count: ")
(display (go2))
(newline)
