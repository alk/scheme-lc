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

(define (wrap-mapc lc)
  (case-lambda ((body) (wrap-mapc (raw-mapc lc body)))
               ((fn init cont)
                (lc fn init cont))))

(define (lc:two v1 v2)
  (wrap-mapc
   (lambda (fn init cont)
     (fn init v1 (lambda (acc)
                   (fn acc v2 cont))))))

(define (lc:one v)
  (wrap-mapc (lambda (fn init cont)
               (fn init v cont))))

(define (lc:zero)
  (wrap-mapc (lambda (fn init cont) (cont init))))

(define (raw-mapc lc body)
  (lambda (fn init cont)
    (lc (lambda (init v cont)
          (define child-lc (body v))
          (child-lc fn init cont))
        init cont)))

(define (lc:range start end . maybe-step)
  (define step (if (pair? maybe-step)
                   (car maybe-step)
                   1))
  (define (loop start fn init cont)
    (if (>= start end)
        (cont init)
        (fn init start (lambda (init)
                         (loop (+ start step) fn init cont)))))
  (wrap-mapc (lambda (fn init cont)
               (loop start fn init cont))))

(define (lc-fold fn init lc)
  (lc fn init (lambda (acc) acc)))

(define (fold-to-displayln lc)
  (define (body acc item cont)
    (displayln "item id " acc " is " item)
    (cont (+ 1 acc)))
  (lc-fold body 0 lc))

(let ()
  (fold-to-displayln (lc:range 0 10)) ; works
  )

(displayln "---")

;; (let ()
;;   (fold-to-displayln ((lc:range 0 10)
;;                       (lambda (a)
;;                         (lc:one a)))) ; a 100+a 200+a ...
;;   )
(let ()
  (fold-to-displayln ((lc:range 0 10)
                      (lambda (a)
                        (lc:range a 1000 100)))) ; a 100+a 200+a ...
  )

(define (pythagorean n)
  ((lc:range 1 n)
   (lambda (a)
     ((lc:range 1 a)
      (lambda (b)
        (define sq (+ (square a) (square b)))
        (define max-c (- n a b))
        (define-values (min-c _) (exact-integer-sqrt sq))
        ((lc:range min-c max-c)
         (lambda (c)
           (if (and (<= (+ a b c) n)
                    (= (+ (square a) (square b)) (square c)))
               (lc:one (vector a b c))
               (lc:zero)))))))))

(define (pythagorean-2 n)
  ((lc:range 1 n)
   (lambda (a)
     ((lc:range (+ 1 a) n)
      (lambda (c)
        (define-values (b rem) (exact-integer-sqrt (- (square c) (square a))))
        (if (and (zero? rem)
                 (> a b))
            (lc:one (vector a b c))
            (lc:zero)))))))

(define (pythagorean-native n)
  (define res '())
  (define (try-c a b)
    (define-values (c rem) (exact-integer-sqrt (+ (square a) (square b))))
    (if (and (zero? rem)
             (< c n))
        (set! res (cons (vector a b c) res))))

  (define (loop-a a)
    (define (loop-b b)
      (if (>= b a)
          (loop-a (+ 1 a))
          (begin
            (try-c a b)
            (loop-b (+ 1 b)))))
    (if (>= a n)
        (lc:list (reverse res))
        (loop-b 1)))

  (loop-a 1))

(define (pythagorean-native-count n)
  (define res 0)
  (define (try-c a b)
    (define-values (c rem) (exact-integer-sqrt (+ (square a) (square b))))
    (if (and (zero? rem)
             (< c n))
        (set! res (+ 1 res))))

  (define (loop-a a)
    (define (loop-b b)
      (if (>= b a)
          (loop-a (+ 1 a))
          (begin
            (try-c a b)
            (loop-b (+ 1 b)))))
    (if (>= a n)
        res
        (loop-b 1)))

  (loop-a 1))


;; (define (lc:list lst . more)
;;   (define fst (lambda (fn init)
;;                 (fold (lambda (e acc) (fn acc e)) init lst)))
;;   (if (null? more)
;;       (wrap-mapc fst)
;;       (lc:concat fst (apply lc:list more))))

(define (lc->count lc)
  (lc:one (lc (lambda (acc _ cont) (cont (+ 1 acc))) 0)))

(define (lc:concat lc1 lc2 . more)
  (define (concat2 lc1 lc2)
    (lambda (fn init cont)
      (lc1 fn init (lambda (acc)
                     (lc2 fn acc cont)))))
  (define (concatn lc1 lc2 more)
    (concat2 lc1 (if (null? more)
                     lc2
                     (concatn lc2 (car more) (cdr more)))))
  (wrap-mapc (concatn lc1 lc2 more)))

(define (lc->first lc)
  (lc->agg (lc:truncate 1 lc) (lambda (acc _ cont) (cont acc))))

(define (lc->nth n lc)
  (lc->first (lc:nthtail n lc)))

(define (lc:nth n lc)
  (lc:one (lc->nth n lc)))

(define (lc:truncate n lc)
  (wrap-mapc (lambda (fn init return)
               (lc (lambda (acc v cont)
                     (define-values (n acc0) (car+cdr acc))
                     (if (zero? n)
                         (return acc0)
                         (fn acc0 v (lambda (acc1)
                                      (cont (cons (- n 1) acc1))))))
                   (cons n init)
                   return))))

(define (lc:nthtail n lc)
  (define tag (list))
  (wrap-mapc
   (lambda (fn init cont)
     (define (wf acc v cont)
       (if (and (pair? acc)
                (eq? (cdr acc) tag))
           (let ((c (car acc)))
             (if (zero? c)
                 (fn init v cont)
                 (cont (cons (- c 1) tag))))
           (fn acc v cont)))
     (lc wf (cons n tag) cont))))

;; lc->agg is just reduce. I.e. fold with no initial value.
(define (lc->agg lc fn)
  (define %empty (list))
  (lc (lambda (acc item cont)
        (if (eq? acc %empty)
            (cont item)
            (fn acc item cont)))
      %empty (lambda (acc) acc)))

(define (lc:agg lc fn)
  (lc:one (lc->agg lc fn)))

(define (count-agg acc _) (+ 1 acc))

(let ()
  ;; (fold-to-displayln (lc:list (list 1 2 3 4)))
  (fold-to-displayln (lc:truncate 10 (pythagorean-2 1000)))
  (fold-to-displayln (lc:truncate 10 (lc:nthtail 1 (pythagorean-2 1000))))
  (fold-to-displayln (lc:nth 0 (pythagorean-2 1000)))
  ;; (fold-to-displayln (lc:one (pythagorean-native-count 2000)))
  (displayln 'asd)
  )
