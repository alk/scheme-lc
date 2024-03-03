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

(define (wrap l)
  (case-lambda ((body) (lc:mapc l body))
               ((null-cont pair-cont) (l null-cont pair-cont))))

(define (lc:one v1)
  (wrap
   (lambda (null-cont pair-cont)
    (pair-cont v1 (lambda (null-cont _) (null-cont))))))

(define (lc:cons hd lc)
  (wrap
   (lambda (null-cont pair-cont)
     (pair-cont hd lc))))

(define (lc:two v1 v2)
  (lc:cons v1 (lc:one v2)))

(define %zero (wrap (lambda (null-cont _) (null-cont))))

(define (lc:zero) %zero)

(define (lc->fold fn init lc)
  (lc (lambda ()
        init)
      (lambda (hd tl)
        (lc->fold fn (fn init hd) tl))))

(define (lc->reduce fn lc)
  (lc (lambda ()
        (error "reduce must have at least one element"))
      (lambda (hd tl)
        (lc->fold fn hd tl))))

(define (lc->list lc)
  (reverse (lc->fold xcons '() lc)))

(define (lc:list lst)
  (define (mk lst)
    (if (null? lst)
        (lc:zero)
        (lambda (_ pair-cont)
          (pair-cont (car lst) (mk (cdr lst))))))
  (wrap (mk lst)))

(define (lc->vector lc)
  (apply vector (lc->list lc)))

(define (fold-to-displayln lc)
  (define (body acc item)
    (displayln "item id " acc " is " item)
    (+ 1 acc))
  (lc->fold body 0 lc))

(define (%range start end step)
  (wrap
   (lambda (null-cont pair-cont)
    (if (>= start end)
        (null-cont)
        (pair-cont start (%range (+ start step) end step))))))

(define (lc:range start end . maybe-step)
  (if (null? maybe-step)
      (%range start end 1)
      (%range start end (car maybe-step))))

(define (lc:iterate fn init)
  (wrap (lambda (_ pair-cont)
          (pair-cont init (lc:iterate fn (fn init))))))

(define (concat2 lc1 lc2)
  (lambda (null-cont pair-cont)
    (lc1 (lambda ()
           (lc2 null-cont pair-cont))
         (lambda (hd tl)
           (pair-cont hd (concat2 tl lc2))))))

(define (lc:concat lc1 lc2 . more)
  (define (concatn lc1 lc2 more)
    (concat2 lc1 (if (null? more)
                     lc2
                     (concatn lc2 (car more) (cdr more)))))
  (wrap (concatn lc1 lc2 more)))

(define (lc:mapc lc fn)
  (lambda (null-cont pair-cont)
    (lc null-cont
        (lambda (hd tl)
          (define tl-mapper (lc:mapc tl fn))
          (define expanded-hd (fn hd))
          (define concater (concat2 expanded-hd tl-mapper))
          (concater null-cont pair-cont)))))

(define (lc:filter lc fn)
  (lambda (null-cont pair-cont)
    (lc null-cont
        (lambda (hd tl)
          (if (fn hd)
              (pair-cont hd (lc:filter tl fn))
              ((lc:filter tl fn) null-cont pair-cont))))))

(define (lc:truncate n lc)
  (define (mk n lc)
    (if (positive? n)
        (lambda (null-cont pair-cont)
          (lc null-cont
              (lambda (hd tl)
                (pair-cont hd (mk (- n 1) tl)))))
        (lc:zero)))
  (wrap (mk n lc)))

(define (lc:nthtail n lc)
  (define (mk n lc)
    (lambda (null-cont pair-cont)
      (if (positive? n)
          (lc null-cont
              (lambda (hd tl)
                ((mk (- n 1) tl) null-cont pair-cont)))
          (lc null-cont pair-cont))))
  (wrap (mk n lc)))

;; or lc:map variable-arg
(define (lc:zip kons lc1 lc2)
  (lambda (null-cont pair-cont)
    (lc1 null-cont
        (lambda (hd1 tl1)
          (lc2 null-cont
               (lambda (hd2 tl2)
                 (pair-cont (kons hd1 hd2) (lc:zip kons tl1 tl2))))))))

(define (lc:cycle . values)
  (define (cycle l)
    (lambda (null-cont pair-cont)
      ((lc:concat l (cycle l)) null-cont pair-cont)))
  (wrap (cycle (lc:list values))))

(define lc:replicate lc:cycle)

(define (lc:each-nth n lc)
  (define (each n lc)
    (lambda (null-cont pair-cont)
      ((lc:nthtail (- n 1) lc) null-cont (lambda (hd tl)
                                           (pair-cont hd (each n tl))))))
  (wrap (each n lc)))

;; takes n values from lc and returns first list of those n values and
;; second remaining tail (through `values')
(define (lc:split-at n lc)
  (define (loop n acc lc)
    (define (ret tl)
      (values (reverse acc) tl))
    (lc (lambda ()
          (ret (lc:zero)))
        (lambda (hd tl)
          (if (positive? n)
              (loop (- n 1) (cons hd acc) tl)
              (ret tl)))))
  (loop n '() lc))

;; like ruby's each_slice, but passing slice to vektor fn to map (up to n) values to 1
(define (lc:slice n vektor lc)
  (define (slice n vektor lc)
    (lambda (null-cont pair-cont)
      (define-values (xs ys) (lc:split-at n lc))
      (if (null? xs)
          (null-cont)
          (pair-cont (apply vektor xs)
                     (slice n vektor ys)))))
  (unless (positive? n)
    (error "slice size must be positive" n))
  (wrap (slice n vektor lc)))

(define (lc:take-while fn lc)
  (define (mk fn lc)
    (lambda (null-cont pair-cont)
      (lc null-cont
          (lambda (hd tl)
            (if (fn hd)
                (pair-cont hd (mk fn tl))
                (null-cont))))))
  (wrap (mk fn lc)))

(fold-to-displayln (lc:range 0 10))

(fold-to-displayln (lc:two 'a 'b))

;; (fold-to-displayln (lc:mapc (lc:range 0 5) (lambda (i) (lc:one (* i i)))))
(fold-to-displayln ((lc:range 0 5) (lambda (i) (lc:one (* i i)))))

(define (pythagorean-2 n)
  ((lc:range 1 n)
   (lambda (a)
     ((lc:range (+ 1 a) n)
      (lambda (c)
        (define-values (b rem) (exact-integer-sqrt (- (square c) (square a))))
        (if (and (zero? rem)
                 (>= a b))
            (lc:one (vector a b c))
            (lc:zero)))))))

(define (inf-pythagorean)
  ((lc:iterate (lambda (i) (+ 1 i)) 1)
   (lambda (c)
     ;; max-a is slightly more than c/(sqrt 2). Idea being we seek
     ;; (square a) less than half of (square c)
     (define max-a (truncate-quotient (* 100 c) 142))
     ((lc:range 1 max-a)
      (lambda (a)
        (define-values (b rem) (exact-integer-sqrt (- (square c) (square a))))
        (if (and (zero? rem)
                 (>= b a))
            (lc:one (vector a b c))
            (lc:zero)))))))


(fold-to-displayln (lc:truncate 10 (pythagorean-2 1000)))

(fold-to-displayln (lc:truncate 10 (lc:nthtail 2 (pythagorean-2 1000))))

(fold-to-displayln (lc:truncate 10 (lc:nthtail 1000 (inf-pythagorean))))

(fold-to-displayln (lc:truncate 10 (lc:filter
                                    (inf-pythagorean)
                                    (lambda (vec)
                                      (zero? (truncate-remainder (vector-ref vec 2) 11))))))

;; (fold-to-displayln (lc:slice 10 list (inf-pythagorean)))

;; (fold-to-displayln (apply lc:cycle (lc->list (lc:range 0 10))))

(displayln "nothing")
