
(import (scheme base) (scheme write) (scheme case-lambda))

(cond-expand
 (guile (import (srfi srfi-1)))
 (else
  (import (scheme list))))

(define (displayln fst . stuff)
  (define (stringify arg)
    (if (string? arg)
        arg
        (let ((port (open-output-string)))
          (write arg port)
          (get-output-string port))))
  (display (stringify fst))
  (if (pair? stuff)
      (apply displayln stuff)
      (newline)))


(define (range-lreducer start end step)
  (lambda (fn init)
    (let loop ((init init)
               (start start))
      (if (>= start end)
          init
          (loop (fn start init) (+ start step))))))

(define (notc v)
  v)

(define (display-lreducer lred)
  (notc (lred (lambda (item acc)
                (displayln "item id " acc " is " item)
                (+ 1 acc))
              0)))

(define (range start end step)
  (lambda (kons knull)
    (if (>= start end)
        knull
        (kons start (range (+ start step) end step)))))

(define (display-reducer lc)
  (display-lreducer (reducer->lreducer lc)))

;; (define (display-reducer lc)
;;   (displayln (lc cons '())))

;; (display-lreducer (range-lreducer 0 10 1))

;; (display-reducer (range 100 110 1))

(define (zero)
  (lambda (kons knull) knull))

(define (one v)
  (lambda (kons knull) (kons v knull)))

(define (reducer-cons v lc)
  (lambda (kons knull)
    (kons v lc)))

(define (many v . rest)
  (reducer-cons v (if (null? rest)
                      (zero)
                      (apply many rest))))

(define (reduce->list lc)
  (define (kons e rest)
    (cons e (if (null? rest)
                rest
                (rest kons '()))))
  (lc kons '()))

(displayln (reduce->list (zero)))
(displayln (reduce->list (one 1)))
(displayln (reduce->list (many 1 2 3)))

(define (reducer->lreducer lc)
  (lambda (fn init)
    (define (rec lc acc)
      (define v (lc cons '()))
      (if (pair? v)
          (rec (cdr v) (fn (car v) acc))
          acc))
    (rec lc init)))

(display-lreducer (reducer->lreducer (zero)))
(let ()
  (define m (many 1 2 3 4 5 6 7))
  ;; (define m (many 1))
  (define lr (reducer->lreducer m))
  (display-lreducer lr)
  ;; (displayln (lr cons '()))
  )

(define (zero-lr)
  (lambda (fn init) init))

(define (one-lr v)
  (lambda (fn init) (fn v init)))

(define (two-lr v1 v2)
  (lambda (fn init)
    (fn v2 (fn v1 init))))

(define (many-lr v1 . rest)
  (lambda (fn init)
    (define acc (fn v1 init))
    (if (null? rest)
        acc
        ((apply many-lr rest) fn acc))))

;; (define (rewrap-cps r-cp kons knull cont)
;;   (r

(define (rewrap-cps r kons knull)
  (call/cc (lambda (first-return)
             (r (lambda (e acc set-acc)
                  (acc (kons e (lambda ()
                                 (call/cc (lambda (return)
                                            (set-acc return)))))))
                first-return
                (lambda (acc)
                  (acc knull))))))

(define (do-rewrap mini-stream)
  (define (rec p kons knull)
    ;; p is null or (cons e (lambda () ...))
    (if (null? p)
        knull
        (notc (kons (car p) (lambda (kons knull)
                              (notc (rec ((cdr p)) kons knull)))))))
  (lambda (kons knull)
    (notc (rec mini-stream kons knull))))

(define (rewrap r-cp)
  (define rewrapped (rewrap-cps r-cp cons '()))
  (do-rewrap rewrapped))

(define (one-lr-cp v)
  (lambda (fn init cont)
    (cont (fn v init))))

(define (zero-lr-cp)
  (lambda (fn init cont)
    (cont init)))

(define (cons-lr-cp v rest)
  (lambda (fn init cont)
    (fn v init (lambda (acc)
                 (rest fn acc cont)))))

(define (lr-cp->list lr-cp)
  (define return (lambda (acc) acc))
  (define (body e acc cont)
    (cont (cons e acc)))
  (reverse (lr-cp body '() return)))

(define wrp-input (cons-lr-cp 3 (cons-lr-cp 2 (cons-lr-cp 1 (zero-lr-cp)))))
;; (define wrp-input (cons-lr-cp 1 (zero-lr-cp)))
(define rewrapped
  (lambda (kons knull)
    (rewrap-cps wrp-input kons knull)))

(displayln (lr-cp->list (zero-lr-cp)))
(displayln (lr-cp->list (cons-lr-cp 3 (cons-lr-cp 2 (cons-lr-cp 1 (zero-lr-cp))))))
(displayln (car (rewrapped cons '())))
;; (displayln (car ((cdr (rewrapped cons '())))))
;; (displayln (car ((cdr ((cdr (rewrapped cons '())))))))
;; (displayln ((cdr ((cdr ((cdr (rewrapped cons '()))))))))

;; (displayln (reduce->list (rewrap wrp-input)))
(let ((lc (rewrap wrp-input)))
  (define ll (lc cons '()))
  (displayln ll))

(display-reducer (rewrap wrp-input))

;; (displayln

(define (r-to-r-cp r)
  (lambda (fn init cont)
    (define (body e acc)
      (call/cc (lambda (return)
                 (fn e acc return))))
    (cont (r body init))))

(define (lreducer->reducer r)
  (rewrap (r-to-r-cp r)))

(displayln ((lreducer->reducer (zero-lr)) cons '()))
(displayln ((lreducer->reducer (one-lr 'a)) cons '()))
(display-reducer (lreducer->reducer (many-lr 'a 'b 'c)))
;; (displayln (car ((lreducer->reducer (one-lr 'a)) cons '())))

;; (displayln "asdasdasd——————-")

;; ;; (display-lreducer (many-lr 1))
;; (displayln ((lreducer->reducer (many-lr 1)) cons '()))

;; (display-reducer (lreducer->reducer (one-lr 1)))


(displayln "\n\n")
