#lang racket
(require racket/match)

(define (printf a) (write a) (newline) (flush-output))

(define (list-index lst v)
  (let loop ([l lst] [n 0])
    (cond
      [(empty? l) #f]
      [(eq? v (car l)) n]
      [else (loop (cdr l) (+ 1 n))])))

(define env (make-hash))

;; Frame: (pc base fun)
;; Fun: (code constant-list argument-list argument-count)

;; Builtin functions for various operations

(hash-set! env '+ `(((add 0 1 2)
                    (return))
                   () () 2))

(hash-set! env '* `(((multiply 0 1 2)
                    (return))
                   () () 2))

(hash-set! env '= `(((eq 0 1 2)
                     (return))
                    () () 2))

(define (current-function stack)
  (caddar stack))

(define (base stack)
  (cadar stack))

(define (pc stack)
  (caar stack))

(define (move-pc stack amount)
  (cons (list (+ amount (pc stack))
              (cadar stack)
              (caddar stack))
        (cdr stack)))

(define (increment-pc stack)
  (move-pc stack 1))

(define (nth-constant stack n)
  (list-ref (cadr (current-function stack)) n))

(define (next-instruction stack)
  (list-ref (car (current-function stack)) (pc stack)))

(define (register-set registers stack index value)
  (if (<= (length registers) (+ index (cadar stack)))
      (register-set (append registers '(())) stack index value)
      (append (take registers (+ index (cadar stack)))
              (list value)
              (list-tail registers (+ 1 index (cadar stack))))))

(define (register-get registers stack index)
  (if (<= (length registers) (+ index (cadar stack)))
      '()
      (list-ref registers (+ index (cadar stack)))))

(define (run registers stack)
  ;; (write registers)
  ;; (newline)
  ;; (write stack)
  ;; (newline)
  ;; (printf (next-instruction stack))
  ;; (newline)
  ;; (newline)
  ;; (flush-output)
  (if (empty? stack)
      ;; If we've nothing left to do, return register 01
      (car registers)
      (match (next-instruction stack)
        [(list 'eq register-a register-b register-c)
         (run (register-set registers stack
                            register-a
                            (if (eq? (register-get registers stack register-b)
                                     (register-get registers stack register-c))
                                1 0))
              (increment-pc stack))]
        [(list 'jump distance)
         (run registers (move-pc stack (+ 1 distance)))]
        [(list 'jump-zero register distance)
         (run registers (if (= 0 (register-get registers stack register))
                            (move-pc stack (+ 1 distance))
                            (increment-pc stack)))]
        [(list 'move-immediate register value)
         (run (register-set registers stack register value)
              (increment-pc stack))]
        [(list 'move-constant register constant)
         (run (register-set registers stack register (nth-constant stack constant))
              (increment-pc stack))]
        [(list 'add register-a register-b register-c)
         (run (register-set registers stack
                            register-a
                            (+ (register-get registers stack register-b)
                               (register-get registers stack register-c)))
              (increment-pc stack))]
        [(list 'multiply register-a register-b register-c)
         (run (register-set registers stack
                            register-a
                            (* (register-get registers stack register-b)
                               (register-get registers stack register-c)))
              (increment-pc stack))]
        [(list 'copy register-a register-b)
         (run (register-set registers stack
                            register-b
                            (register-get registers stack register-a))
              (increment-pc stack))]
        [(list 'call register)
         (run registers
              (cons (list 0 (+ register (base stack))
                          (hash-ref env (register-get registers stack register)))
                    (increment-pc stack)))]
        [(list 'return )
         (run registers (cdr stack))]
        [(list 'halt) (exit)])))

;; Compiler

(define primops '(if))

(define (register-find registers atom)
  (cadar (filter (lambda (i) (eq? atom (car i))) registers)))

(define (compile-atom atom state)
  (match state
    [(list constants registers instructions)
     (if (member atom constants)
         (let ([cidx (list-index constants atom)]
               [ridx (length registers)])
           (list constants
                 (cons (list atom ridx) registers)
                 (cons (list 'move-constant ridx cidx) instructions)))
         (let ([idx0 (register-find registers atom)]
               [idx1 (length registers)])
           (list constants
                 (cons (list atom idx1) registers)
                 (cons (list 'copy idx0 idx1) instructions))))]))

(define (compile-form form state)
  (cond
    [(and (pair? form) (eq? 'if (car form)))
     (let* ([base-state (compile-form (cadr form) state)]
            ;; Carry the constant table forward, but rewind instructions and
            ;; registers to before the predicate call
            [true-state
             (compile-form (caddr form) (cons (car base-state) (cdr state)))]
            [false-state
             (compile-form (cadddr form) (cons (car true-state) (cdr state)))]
            [true-instruction-count (- (length (caddr true-state))
                                       (length (caddr state)))]
            [false-instruction-count (- (length (caddr false-state))
                                        (length (caddr state)))])
       (list (car false-state)
             (cadr state)
             (append (take (caddr false-state) false-instruction-count)
                     `((jump ,false-instruction-count))
                     (take (caddr true-state) true-instruction-count)
                     `((jump-zero ,(length (cadr state)) ,(+ 1 true-instruction-count)))
                     (caddr base-state))))]
    [(pair? form)
     (let ([cidx (length (cadr state))]
           [state1 (foldl compile-form state form)])
       (match state1
         [(list constants registers instructions)
          (list constants
                (list-tail registers (- cidx 1))
                (cons (list 'call cidx) instructions))]))]
    [else (compile-atom form state)]))


(define (find-constants form state)
  (match state
    [(list args env constants)
     (cond
       [(pair? form) (foldl find-constants state form)]
       ;; Don't include primitives
       [(member form primops) (list args env constants)]
       ;; Don't include a value in the constant list twice
       [(member form constants) (list args env constants)]
       ;; Don't include a value in the constant list if it's an argument
       [(member form args) (list args env constants)]
       [else (list args env (cons form constants))])]))

(define (treefold fun acc form)
  (cond [(pair? form) (foldl (lambda (a f) (treefold fun a f)) acc form)]
        [else (cons (fun form) acc)]))

(define (walk-form form fn)
  (cond [(pair? form) (map walk-form 

(define (find-free-bindings form state)
  (match state
    [(list args env bindings)
     (cond [(pair? form) (foldl find-free-bindings state form)]
           [(member form args) (list args env bindings)]
           [(member form env
        [(member form args) '()]
        [(member form 

(define (lift-lambdas form)
  (match form
    [(list 'lambda args body)
     (let ([sym (gensym)]
           [free (find-free-bindings args body)])
       (compile-toplevel
        (list 'define sym (list 'lambda (append free args) body)))
       (list 'think sym free))]
    [(list _ ...) (map lift-lambdas form)]
    [_ form]))

(define (compile-lambda args body)
  (let* ([body (lift-lambdas body)]
         [constants (caddr (find-constants body (list args (hash-keys env) '())))]
         ;; Add a "nil" at the head of the registers to signify the current
         ;; function, which is always at register 0
         [registers (cons '(() 0) (map list args (range 1 (+ 1 (length args)))))])
    (match (compile-form body (list constants registers '()))
      [(list _ registers1 instructions)
       `(,(reverse (append `((return) (copy ,(+ 1 (length args)) 0)) instructions))
         ,constants () ,(length args))])))

(define (compile-definition form)
  (match form
    [(list 'lambda args body) (compile-lambda args body)]))

(define (compile-toplevel sexp)
  (match sexp
    [(list 'define name form)
     ;; Set the name to nil pre-compilation to enable recursion
     (hash-set! env name '())
     (hash-set! env name (compile-definition form))]))

(define (read-sexps port sexps)
  (let ([sexp (read port)])
    (if (eof-object? sexp)
        sexps
        (read-sexps port (cons sexp sexps)))))

(define (main file)
  (map compile-toplevel (read-sexps (open-input-file file) '()))
  (printf env)
  (run '(main) `((0 0 ,(hash-ref env 'main)))))

(main "foo.avn")
