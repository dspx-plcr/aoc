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

(define-record-type state
  (fields
   (immutable buf)
   (mutable pos)
   (mutable acc)
   (mutable mul?)
   (mutable parse-success)
   (mutable parse-fail)))

(define (try parser state)
  (call/cc (lambda (k)
             (state-parse-fail-set! state k)
             (parser state))))

(define (parse-char c)
  (lambda (state)
    (if (eq? (string-ref (state-buf state) (state-pos state)) c)
      ((state-parse-success state))
      ((state-parse-fail state)))))

(define (parse-number state)
  (define start (state-pos state))
  (define end (string-skip (state-buf state) char-set:digit start))
  (if (= end start) 
    ((state-parse-fail state))
    ((state-parse-success state)
     (cons (- end start)
           (string->number (string-copy (state-buf state) start end))))))

(define (parse-mul state)
  (define buf (state-buf state))
  (define start (state-pos state))
  (define success (state-parse-success state))
  (define fail (state-parse-fail state))
  (define (reset-state)
    (state-pos-set! state start)
    (state-parse-success-set! state success)
    (state-parse-fail-set! state fail))
  (define (new-fail)
    (reset-state)
    (fail))
  (define (new-success res)
    (reset-state)
    (success res))
  (define x 0)
  (define y 0)
  (define (num-success x-or-y)
    (lambda (res)
      (state-pos-set! state (+ (state-pos state) (car res)))
      (case x-or-y
	((x) (set! x (cdr res)))
	((y) (set! y (cdr res))))))
  (define (char-success)
    (state-pos-set! state (+ (state-pos state) 1)))
  (state-parse-fail-set! state new-fail)

  (let ((p (state-pos state)))
    (if (or (<= (string-length buf) (+ p 4))
            (not (string= buf "mul(" p (+ p 4))))
      ((state-parse-fail state))))
  (state-pos-set! state (+ 4 (state-pos state)))
  
  (state-parse-success-set! state (num-success 'x))
  (parse-number state)

  (state-parse-success-set! state char-success)
  ((parse-char #\,) state)

  (state-parse-success-set! state (num-success 'y))
  (parse-number state)

  (state-parse-success-set! state char-success)
  ((parse-char #\)) state)
  (if (state-mul? state)
    (new-success (cons (- (state-pos state) start) (* x y)))
    (new-success (cons (- (state-pos state) start) 0))))
      
(define (parse-do state)
  (let ((buf (state-buf state))
	(p (state-pos state)))
    (if (or (<= (string-length buf) (+ p 4))
            (not (string= buf "do()" p (+ p 4))))
      ((state-parse-fail state))))
  ((state-parse-success state) (cons 4 #t)))
  
(define (parse-don't state)
  (let ((buf (state-buf state))
	(p (state-pos state)))
    (if (or (<= (string-length buf) (+ p 7))
            (not (string= buf "don't()" p (+ p 7))))
      ((state-parse-fail state))))
  ((state-parse-success state) (cons 7 #f)))

(define (go)
  (define restart #f)
  (define state
    (make-state
      (call-with-input-file "in.txt" get-string-all)
      0 0 #t #f #f))

  (define (mul-success res)
    (state-pos-set! state (+ (state-pos state) (car res)))
    (state-acc-set! state (+ (state-acc state) (cdr res)))
    (restart))
  (define (switch-success res)
    (state-pos-set! state (+ (state-pos state) (car res)))
    (state-mul?-set! state (cdr res))
    (restart))
  (call/cc (lambda (k)
             (set! restart k)))
  (if (>= (state-pos state) (string-length (state-buf state)))
    (state-acc state)
    (begin
      (state-parse-success-set! state mul-success)
      (try parse-mul state)
      (state-parse-success-set! state switch-success)
      (try parse-do state)
      (try parse-don't state)
      (state-pos-set! state (+ 1 (state-pos state)))
      (restart))))

(display "Sum: ")
(display (go))
(newline)