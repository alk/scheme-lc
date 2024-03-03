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

(define (one->lc v1)
  (lambda (null-cont pair-cont)
    (pair-cont v1 (lambda (null-cont _) (null-cont)))))

(define (lc:cons hd)
  (lambda (lc)
    (lambda (null-cont pair-cont)
      (pair-cont hd lc))))

(define (lc %lc . mappers)
  (if (null? mappers)
      %lc
      (apply lc ((car mappers) %lc) (cdr mappers))))

(define (two->lc v1 v2)
  (lc (one->lc v2)
      (lc:cons v1)))

(define %zero (lambda (null-cont _) (null-cont)))

(define (zero->lc) %zero)

(define (lc->fold fn init)
  (define (rec fn init lc)
    (lc (lambda ()
          init)
        (lambda (hd tl)
          (rec fn (fn init hd) tl))))
  (lambda (lc)
    (rec fn init lc)))

(define (lc->reduce fn)
  (lambda (lc)
    (lc (lambda ()
          (error "reduce must have at least one element"))
        (lambda (hd tl)
          ((lc->fold fn hd) tl)))))

(define (lc->list)
  (lambda (lc)
    (reverse ((lc->fold xcons '()) lc))))

(define (list->lc lst)
  (define (mk lst)
    (if (null? lst)
        (zero->lc)
        (lambda (_ pair-cont)
          (pair-cont (car lst) (mk (cdr lst))))))
  (mk lst))

(define (lc->vector)
  (lambda (lc)
    (apply vector ((lc->list) lc))))

(define (fold-to-displayln lc)
  (define (body acc item)
    (displayln "item id " acc " is " item)
    (+ 1 acc))
  ((lc->fold body 0) lc))

(define (%range->lc start end step)
  (lambda (null-cont pair-cont)
    (if (>= start end)
        (null-cont)
        (pair-cont start (%range->lc (+ start step) end step)))))

(define (range->lc start end . maybe-step)
  (if (null? maybe-step)
      (%range->lc start end 1)
      (%range->lc start end (car maybe-step))))

(define (iterate->lc fn init)
  (lambda (_ pair-cont)
    (pair-cont init (iterate->lc fn (fn init)))))

(define (concat2 lc1 lc2)
  (lambda (null-cont pair-cont)
    (lc1 (lambda ()
           (lc2 null-cont pair-cont))
         (lambda (hd tl)
           (pair-cont hd (concat2 tl lc2))))))

(define (lc:concat lc2 . more)
  (define (concatn lc1 lc2 more)
    (concat2 lc1 (if (null? more)
                     lc2
                     (concatn lc2 (car more) (cdr more)))))
  (lambda (lc1)
    (concatn lc1 lc2 more)))

(define (lc:mapc fn)
  (define (rec fn lc)
    (lambda (null-cont pair-cont)
      (lc null-cont
          (lambda (hd tl)
            (define tl-mapper (rec fn tl))
            (define expanded-hd (fn hd))
            (define concater (concat2 expanded-hd tl-mapper))
            (concater null-cont pair-cont)))))
  (lambda (lc)
    (rec fn lc)))

(define (lc:filter fn)
  (define (rec fn lc)
    (lambda (null-cont pair-cont)
      (lc null-cont
          (lambda (hd tl)
            (if (fn hd)
                (pair-cont hd (rec fn tl))
                ((rec fn tl) null-cont pair-cont))))))
  (lambda (lc)
    (rec fn lc)))

(define (lc:truncate n)
  (define (rec n lc)
    (if (positive? n)
        (lambda (null-cont pair-cont)
          (lc null-cont
              (lambda (hd tl)
                (pair-cont hd (rec (- n 1) tl)))))
        %zero))
  (lambda (lc)
    (rec n lc)))

(define (lc:nthtail n)
  (define (rec n lc)
    (lambda (null-cont pair-cont)
      (if (positive? n)
          (lc null-cont
              (lambda (hd tl)
                ((rec (- n 1) tl) null-cont pair-cont)))
          (lc null-cont pair-cont))))
  (lambda (lc)
    (rec n lc)))

;; or lc:map variable-arg
(define (lc:zip kons lc2)
  (define (rec kons lc2 lc1)
    (lambda (null-cont pair-cont)
      (lc1 null-cont
           (lambda (hd1 tl1)
             (lc2 null-cont
                  (lambda (hd2 tl2)
                    (pair-cont (kons hd1 hd2) (rec kons tl2 tl1))))))))
  (lambda (lc1)
    (rec kons lc2 lc1)))

(define (cycle->lc . values)
  (define (cycle l)
    (lambda (null-cont pair-cont)
      (((lc:concat (cycle l)) lc) null-cont pair-cont)))
  (cycle (list->lc values)))

(define replicate->lc cycle->lc)

(define (lc:each-nth n)
  (define (each n lc)
    (lambda (null-cont pair-cont)
      (((lc:nthtail (- n 1)) lc) null-cont (lambda (hd tl)
                                             (pair-cont hd (each n tl))))))
  (lambda (lc)
    (each n lc)))

;; takes n values from lc and returns first list of those n values and
;; second remaining tail (through `values')
(define (lc:split-at n body)
  (define (loop n acc lc)
    (define (ret tl)
      (body (reverse acc) tl))
    (lc (lambda ()
          (ret (zero->lc)))
        (lambda (hd tl)
          (if (positive? n)
              (loop (- n 1) (cons hd acc) tl)
              (ret tl)))))
  (lambda (lc)
    (loop n '() lc)))

;; like ruby's each_slice, but passing slice to vektor fn to map (up to n) values to 1
(define (lc:slice n vektor)
  (define (slice n vektor lc)
    (lambda (null-cont pair-cont)
      (define-values (xs ys) (lc:split-at n lc))
      (if (null? xs)
          (null-cont)
          (pair-cont (apply vektor xs)
                     (slice n vektor ys)))))
  (unless (positive? n)
    (error "slice size must be positive" n))
  (lambda (lc)
    (slice n vektor lc)))

(define (lc:take-while fn lc)
  (define (rec fn lc)
    (lambda (null-cont pair-cont)
      (lc null-cont
          (lambda (hd tl)
            (if (fn hd)
                (pair-cont hd (rec fn tl))
                (null-cont))))))
  (lambda (lc)
    (rec fn lc)))

(define (lc:map1 fn)
  (define (rec fn lc)
    (lambda (null-cont pair-cont)
      (lc null-cont
          (lambda (hd tl)
            (pair-cont (fn hd) (rec fn tl))))))
  (lambda (lc)
    (rec fn lc)))

(define (gen-iterate->lc fn init)
  (define marker (list))
  (define (rec init)
    (if (eq? init marker)
        (zero->lc)
        (lambda (null-cont pair-cont)
          (pair-cont init (rec (fn init marker))))))
  (rec (fn init marker)))

;; (define (with-generate->lc body)
;;   (define unwrap (list))
;;   (define (fn init done)
;;     (call/cc (lambda (return)
;;                (init done return))))
;;   (gen-iterate->lc fn

;; (define (rewrap-cps r fn knull cont)
;;   (r (lambda (e acc cont)
;;        (fn e (lambda (acc)
;;                (cont acc))
;;            acc))
;;      cont
;;      (lambda (acc)
;;        (acc knull))))


;; (begin
;;   (kons e1 knull (lambda (k1)
;;                    (kons e2 k1 (lambda (k2)
;;                                  (kons e3 k2 cont)))))
;;   (kons e3 (kons e2 (kons e1 knull)))
;;   (with-generate->lc (lambda (fn null)
;;                        (fn 1 (lambda () (fn 2 (lambda () (fn 3 null)))))))
;;   (fn 1 (lambda () (fn 2 (lambda () (fn 3 null)))))

;;   (lambda (cont)
;;     (define l1 (lambda (cont)
;;                  (define l2 (lambda (cont)
;;                               (fn 3 null cont)))
;;                  (fn 2 l2 cont)))
;;     (fn 1 l1))

;;   )

;; (define (reducible->lc r)
;;   ;; r: \fn acc -> (fn (fn acc e1) e2 ...)
;;   (define (rec cont)
;;     (lambda (null-cont pair-cont)
;;       (call/cc (lambda (got)
;;                  (r (lambda (acc e)
;;                       (call/cc (lambda (next)
;;                                  (acc e next))))
;;                     (lambda (e resume)
;;                       (got (pair-cont e (rec resume)))))
                 

(fold-to-displayln (range->lc 0 10))

(fold-to-displayln (two->lc 'a 'b))

(fold-to-displayln (lc (range->lc 0 5)
                       (lc:mapc (lambda (i) (one->lc (* i i))))))

(define (pythagorean-2 n)
  (lc (range->lc 1 n)
      (lc:mapc
       (lambda (a)
         (lc (range->lc (+ 1 a) n)
             (lc:mapc
              (lambda (c)
                (define-values (b rem) (exact-integer-sqrt (- (square c) (square a))))
                (if (and (zero? rem)
                         (>= a b))
                    (one->lc (vector a b c))
                    (zero->lc)))))))))

(define (inf-pythagorean)
  (lc (iterate->lc (lambda (i) (+ 1 i)) 1)
      (lc:mapc
       (lambda (c)
         ;; max-a is slightly more than c/(sqrt 2). Idea being we seek
         ;; (square a) less than half of (square c)
         (define max-a (truncate-quotient (* 100 c) 142))
         (lc (range->lc 1 max-a)
             (lc:mapc
              (lambda (a)
                (define-values (b rem) (exact-integer-sqrt (- (square c) (square a))))
                (if (and (zero? rem)
                         (>= b a))
                    (one->lc (vector a b c))
                    (zero->lc)))))))))


(fold-to-displayln (lc (pythagorean-2 1000)
                       (lc:truncate 10)))

(fold-to-displayln (lc (pythagorean-2 1000)
                       (lc:nthtail 2)
                       (lc:truncate 10)))

(fold-to-displayln (lc (inf-pythagorean)
                       (lc:nthtail 1000)
                       (lc:truncate 10)))

(fold-to-displayln (lc (inf-pythagorean)
                       (lc:filter (lambda (vec)
                                    (zero? (truncate-remainder (vector-ref vec 2) 11))))
                       (lc:truncate 10)))

;; (fold-to-displayln (lc:slice 10 list (inf-pythagorean)))

;; (fold-to-displayln (apply lc:cycle (lc->list (lc:range 0 10))))

(displayln "nothing")
