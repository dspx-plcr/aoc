(import
  (srfi srfi-1)
  (srfi srfi-11)
  (srfi srfi-13)
  (srfi srfi-14)
  (srfi srfi-27)
  (srfi srfi-43)
  (rnrs hashtables (6))
  (rnrs lists (6))
  (rnrs io ports (6))
  (rnrs io simple (6))
  (rnrs records syntactic (6))
  (rnrs sorting (6)))

(define grid #f)

(define (read-grid)
  (let loop ((g '()))
        (if (port-eof? (current-input-port))
          (set! grid (list->vector (reverse g)))
          (loop (cons
                 (list->vector (string->list (get-line (current-input-port))))
                 g)))))

(with-input-from-file "in.txt"
  (lambda ()
    (read-grid)))

(define (find-guard)
  (define (v i j) (vector-ref (vector-ref grid j) i))
  (let loop ((i 0)
             (j 0))
    (cond
      ((= j (vector-length grid)) #f)
      ((= i (vector-length (vector-ref grid j))) (loop 0 (+ 1 j)))
      ((eqv? (v i j) #\^) (cons (cons i j) (cons 0 -1)))
      ((eqv? (v i j) #\>) (cons (cons i j) (cons 1 0)))
      ((eqv? (v i j) #\v) (cons (cons i j) (cons 0 1)))
      ((eqv? (v i j) #\<) (cons (cons i j) (cons -1 0)))
      (else (loop (+ 1 i) j)))))

(define (out-of-bounds? pos)
  (or (< (cdr pos) 0)
      (< (car pos) 0)
      (>= (cdr pos) (vector-length grid))
      (>= (car pos) (vector-length (vector-ref grid (cdr pos))))))

(define (ahead pos dir)
  (let ((i (+ (car pos) (car dir)))
        (j (+ (cdr pos) (cdr dir))))
    (if (out-of-bounds? (cons i j))
      #\.
      (vector-ref (vector-ref grid j) i))))

(define (move pos dir)
  (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))

(define (rotate-right dir)
   (cond
     ((and (= 0 (car dir)) (= 1 (cdr dir))) (cons -1 0))
     ((and (= -1 (car dir)) (= 0 (cdr dir))) (cons 0 -1))
     ((and (= 0 (car dir)) (= -1 (cdr dir))) (cons 1 0))
     ((and (= 1 (car dir)) (= 0 (cdr dir))) (cons 0 1))))

(define (go1)
  (define start (find-guard))
  (define prev (make-hashtable equal-hash equal?))
  (let loop ((pos (car start))
             (dir (cdr start)))
    (cond
      ((out-of-bounds? pos) (vector-length (hashtable-keys prev)))
      ((eqv? #\# (ahead pos dir)) (loop pos (rotate-right dir)))
      (else (begin
	     (hashtable-set! prev pos 1)
	     (loop (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir)))
		   dir))))))

(define (loop? pos dir)
  (define prev (make-hashtable equal-hash equal?))
  (let loop ((pos pos)
             (dir dir))
    (cond
      ((out-of-bounds? pos) #f)
      ((eqv? #\# (ahead pos dir)) (loop pos (rotate-right dir)))
      ((hashtable-contains? prev (cons pos dir)) #t)
      (else
        (begin
          (hashtable-set! prev (cons pos dir) #t)
          (loop (move pos dir) dir))))))

(define (go2)
  (define start (find-guard))
  (define prev (make-hashtable equal-hash equal?))
  (let loop ((cnt 0)
             (pos (car start))
             (dir (cdr start)))
    (cond
      ((out-of-bounds? pos) cnt)
      ((eqv? #\# (ahead pos dir)) (loop cnt pos (rotate-right dir)))
      ((hashtable-contains? prev (move pos dir)) (loop cnt (move pos dir) dir))
      (else (let ((next (move pos dir)))
              (hashtable-set! prev (move pos dir) #t)
	      (if (out-of-bounds? next)
	        (loop cnt next dir)
	        (begin
                  (vector-set! (vector-ref grid (cdr next)) (car next) #\#)
                  (let ((l? (loop? pos dir)))
                    (vector-set! (vector-ref grid (cdr next)) (car next) #\.)
                    (loop (+ cnt (if l? 1 0)) next dir)))))))))

(display "Count: ")
(display (go1))
(newline)
(display "Count: ")
(display (go2))
(newline)
