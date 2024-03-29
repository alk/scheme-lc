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

;; in this attempt we're modeling our generalized sequences, we call
;; them LCs, as folding functions. Each LC receives function and
;; accumulator and computes (fn (elem-at n) (fn (elem-at (- n 1))
;; ... acc)). I.e. applying fn element and accumulator for each
;; element in turn and the result becoming next accumulator.

;; Zero (aka empty) LC just returns acc.
(define %zero
  (lambda (fn acc) acc))

(define (zero->lc) %zero)

;; Unused.
(define (%cons-lazy hd lc-thunk)
  (lambda (fn acc)
    (define v (fn hd acc))
    ((lc-thunk) fn v)))

;; Creates LC with one element.
(define (one->lc v)
  (lambda (fn acc)
    (fn v acc)))

;; Creates LC with given list of values
(define (->lc . values)
  (list->lc values))

;; Same as above with list passed explicitly
(define (list->lc lst)
  (lambda (fn init)
    (define (rec acc rest)
      (if (null? rest)
          acc
          (rec (fn (car rest) acc) (cdr rest))))
    (rec init lst)))

;; Creates LC with list of numbers starting from start, increasing by
;; maybe-step or 1, and continuing until it hits end (end is not
;; included).
(define (range->lc start end . maybe-step)
  (define (range start end step fn init)
    (if (>= start end)
        init
        (range (+ start step) end step fn (fn start init))))
  (lambda (fn init)
    (range start end (if (null? maybe-step) 1 (car maybe-step))
           fn init)))

;; creates LS with elements (iter-init (iter-fn iter-init) (iter-fn (iter-fn iter-init)) ...)
;;
;; Note it never stops, so LC consumption logic might need means to do
;; non-local exit.
(define (iterate->lc iter-fn iter-init)
  (lambda (fn init)
    (let loop ((acc init)
               (iter-acc iter-init))
      (define next-acc (fn iter-acc acc))
      (define next-tter-acc (iter-fn iter-acc))
      (loop next-acc next-tter-acc))))

;; Helper to display LC's elements.
(define (lc->displayln lc)
  (define (body item acc)
    (displayln "item id " acc " is " item)
    (+ 1 acc))
  (lc body 0))


;; ---- lazy lists: (cons e1 (lambda () (cons e2 (lambda () ... '()))))
;;
;; Basically similar to streams but without promises so maybe a little
;; cheaper. This is convenient for some intermediate operations
;; otherwise difficult with plain LC. Most notably, trunction and
;; zipping multiple sequences together.

;; converts LC (where LC drives iteration) to lazy list (where
;; iteration is driven externally). As noted above, we use those lazy
;; lists in places where truncation of sequences output is needed.
;;
;; The implementation is a bit of head-scratcher. But here are couple
;; hints. First, we use continuations (that return out of list CDR
;; thunks) as accumulators. Second thing to keep in mind is we track 2
;; continuations. One mentioned above, second is used to "return" back
;; to LC's iteration. Each time LC's fn is called we capture this
;; (second type of) continuation. And each time thunk is expanded we
;; capture first kind.
;;
;; I am not sure why I choose to introduce r-cp thingy and what was
;; the idea behind the name.
;;
;; This is the original function I wrote, unsure why I left it so
;; over-complicated. Below is clearer rewrite
;; (define (%lc->lazy-list lc)
;;   (define r-cp (lambda (fn init cont)
;;                  (define (body e acc)
;;                    (call/cc (lambda (return)
;;                               (fn e acc return))))
;;                  (cont (lc body init))))
;;   (call/cc (lambda (first-return)
;;              (r-cp (lambda (e acc set-acc)
;;                      (acc (cons e (lambda ()
;;                                     (call/cc (lambda (return)
;;                                                (set-acc return)))))))
;;                    first-return
;;                    (lambda (acc)
;;                      (acc '()))))))

(define (%lc->lazy-list lc)
  ;; this is the folding function we pass to LC
  (define (body e thunk-return)
    (call/cc (lambda (body-return)
               (define thunk (lambda ()
                               (call/cc (lambda (new-thunk-return)
                                          (body-return new-thunk-return)))))
               (thunk-return (cons e thunk)))))

  (call/cc (lambda (first-return)
             (define last-acc (lc body first-return))
             (last-acc '()))))

(define (lazy-list-trunc n lst)
  (if (> n 0)
      (if (null? lst)
          lst
          (cons (car lst) (lambda ()
                            (lazy-list-trunc (- n 1) ((cdr lst))))))
      '()))

(define (lazy-list-skip n lst)
  (if (and (> n 0)
           (pair? lst))
      (lazy-list-skip (- n 1) ((cdr lst)))
      lst))

(define (ll-map1 fn lst)
  (if (null? lst)
      lst
      (cons (fn (car lst)) (lambda ()
                             (ll-map1 fn ((cdr lst)))))))

;; helper for lc:map below. Just like map except more-lst is
;; (explicitly passed) regular list of lazy-lists.
(define (lazy-list-map fn lst more-lst)
  (call/cc
   (lambda (return)
     (define-values (args rests)
       (unzip2 (map (lambda (lst)
                      (when (null? lst)
                        (return '())) ; one of lists too short
                      (list (car lst) ((cdr lst))))
                    more-lst)))
     (if (null? lst)
         lst
         (cons (apply fn (car lst) args)
               (lambda ()
                 (lazy-list-map fn ((cdr lst)) rests)))))))

;; folding is how we convert back from lazy-lists to LCs.

(define (lazy-list-fold fn acc lst)
  (if (null? lst)
      acc
      (lazy-list-fold fn (fn (car lst) acc) ((cdr lst)))))

(define (lazy-list->lc lst)
  (lambda (fn init)
    (lazy-list-fold fn init lst)))

;; ------ end of lazy lists

;; lc function takes given LC and transforms it through list of LC->LC
;; functions. So, note, p0 and rest are one arg lambdas. Lets call the
;; LC transformers below. Well, last element could and usually does
;; produce non-LC value (i.e. converting to regular list, vector, or
;; printing etc).
;;
;; Note, usually name lc transformers by having "lc:"
;; prefix. I.e. lc:map, lc:filter etc.
(define (lc lc0 p0 . rest)
  (define (rec lc rest)
    (if (null? rest)
        lc
        (rec ((car rest) lc) (cdr rest))))
  (rec (p0 lc0) rest))

;; creates LC tansformer (i.e. lambda that takes LC) that filters it's
;; elements.
(define (lc:filter pred)
  (lambda (lc)
    (lambda (fn init)
      (lc (lambda (e acc)
            (if (pred e)
                (fn e acc)
                acc))
          init))))

;; creates LC transformer that maps elements.
(define (lc:map1 map-fn)
  (lambda (lc)
    (lambda (fn init)
      (lc (lambda (e acc)
            (fn (map-fn e) acc))
          init))))

;; creates LC trasformer that "zips" it's LC with a given list of LC's
;; and then maps this with N arg function. I.e. like lisp's map. See
;; map1 for simpler and faster one arg version.
(define (lc:map fn . lcs)
  (lambda (lc)
    (define ll (%lc->lazy-list lc))
    (define lls (map %lc->lazy-list lcs))
    (lazy-list->lc (lazy-list-map fn ll lls))))

;; creates LC transformer that zips it's LC with a given `lc2'
;; transformer. Resultant transformer produces values like this
;;
;; (e1 . e2) (i.e. (cons e1 e2), i.e. improper lists)
(define (lc:zip2 lc2)
  (lambda (lc1)
    (define ll1 (%lc->lazy-list lc1))
    (define ll2 (%lc->lazy-list lc2))
    (define (rec ll1 ll2)
      (if (and (pair? ll1)
               (pair? ll2))
          (cons (cons (car ll1)
                      (car ll2))
                (lambda ()
                  (rec ((cdr ll1)) ((cdr ll2)))))
          '()))
    (lazy-list->lc (rec ll1 ll2))))

(define (lc->first empty-val)
  (lambda (lc)
    (define ll (%lc->lazy-list lc))
    (if (null? ll)
        empty-val
        (car ll))))

(define (lc:truncate n)
  (lambda (lc)
    (define ll (%lc->lazy-list lc))
    (lazy-list->lc (lazy-list-trunc n ll))))

;; ;; HERE
;; (define (lc:truncate n)
;;   (if (<= n 0)
;;       (lambda (lc) %zero)
;;       (lambda (lc)
;;         (lambda (fn acc)
;;           (call/cc (lambda (return)
;;                      (define (body e acc)
;;                        (define real-acc (car acc))
;;                        (define next-i (- (cdr acc) 1))
;;                        (define next-acc (fn e real-acc))
;;                        (if (zero? next-i)
;;                            (return next-acc)
;;                            (car next-acc next-i)))
;;                      (car (lc body (cons acc n)))))))))

(define (lc:skip n)
  (lambda (lc)
    (define ll (%lc->lazy-list lc))
    (lazy-list->lc (lazy-list-skip n ll))))

(define (lc:concat lc2 . more)
  (lambda (lc1)
    (lambda (fn init)
      (define first (lc1 fn init))
      (define sec (lc2 fn first))
      (fold (lambda (lc acc)
              (lc fn acc))
            sec
            more))))

;; maybe slice ?

;; mapconcat. aka bind of our lc monad (one->lc is return). This means
;; body must take one value and return list comprehension.
(define (lc:mapc body)
  (lambda (lc)
    (lambda (fn init)
      (lc (lambda (v init)
            (define child-lc (body v))
            (child-lc fn init))
          init))))

;; UNTESTED
(define (lc:mapc-with-break body)
  (lambda (lc)
    (lambda (fn init)
      (call/cc (lambda (return)
                 (lc (lambda (v init)
                       (define (break) (return init))
                       (define child-lc (body v break))
                       (child-lc fn init))
                     init))))))

;; ----------------

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

(lc (->lc 11 22 33 44)
    (lc:concat (iterate->lc (lambda (i) (+ 1 i)) -13)) ; (->lc 1 2 3)
    ;; (lc:map1 (lambda (i) (cons i (+ 42 i))))
    (lc:map (lambda (i b) (cons b i))
            (iterate->lc (lambda (i) 42) 0))
    ;; (lc:zip2 (iterate->lc (lambda (i) 42) 'first))
    (lc:truncate 11)
    lc->displayln)

(lc (pythagorean-2 1000)
    (lc:skip 1)
    (lc:truncate 10)
    ;; (lc:zip2 (iterate->lc (lambda (i) (+ 1 i)) -13))
    lc->displayln)

(lc (inf-pythagorean)
    (lc:filter (lambda (vec)
                 (zero? (truncate-remainder (vector-ref vec 2) 101))))
    (lc:truncate 100)
    lc->displayln)
