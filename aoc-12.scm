(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-171))

(define biginput (string-trim-right (read-file-as-string "./aoc-12-input.txt")))
(define smallinput "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(define all-regions (make-hash-table))

(define (make-region) (make-hash-table))

(define (region-row region y)
  (hashv-ref region y '()))

(define (add-to-region! region c)
  (let ((cx (coord-x c)) (cy (coord-y c)))
    (hashv-set! region cy (append (region-row region cy) (list cx)))))

(define (touches-region? c region)
  (let ((cx (coord-x c)) (cy (coord-y c)))
    (or (any (λ (x) (= x cx)) (region-row region (1- cy)))
        (any (λ (x) (= x cx)) (region-row region (1+ cy)))
        (any (λ (x) (= 1 (abs (- x cx)))) (region-row region cy)))))

(define (find-region kind coord)
  (let ((rlist (hashv-ref all-regions kind '())))
    (or (find (λ (r) (touches-region? coord r)) rlist)
        (let ((new-region (make-region)))
          (hashv-set! all-regions kind (cons new-region rlist))
          new-region))))

(define (connected-regions? a b)
  (any (λ (c) (touches-region? c a))
       (concatenate (hash-map->list (λ (y xs)
                                      (map (λ (x) (make-coord x y)) xs))
                                    b))))

(define (join-2regions a b)
  (hash-for-each (λ (y xs)
                   (for-each (λ (x) (add-to-region! a (make-coord x y))) xs))
                 b)
  a)

(define (merge-regions lst)
  (if (nil? lst) '()
      (let* ((head (car lst))
             (new-list (list head)))
        (let loop ((rest (cdr lst)))
          (when (not (nil? rest))
            (if (connected-regions? head (car rest))
                (join-2regions head (car rest))
                (set! new-list (append new-list (list (car rest)))))
            (loop (cdr rest))))
        (if (= (length lst) (length new-list))
            (append (list (car lst)) (merge-regions (cdr lst)))
            (merge-regions new-list)))))

(define (merge-all-regions)
  (hash-for-each (λ (kind regions)
                   (when (not (nil? (cdr regions)))
                     (hashv-set! all-regions kind (merge-regions regions))))
                 all-regions))

(define (parse-regions input)
  (define farm (string->nllist input))
  (let loop ((c (make-coord 0 0)))
    (when (not (nil? c))
      (add-to-region! (find-region (grid-ref farm c) c) c)
      (loop (grid-next-coord farm c))))
  (merge-all-regions))

(parse-regions biginput)



(define (region-area region)
  (apply + (hash-map->list (λ (y xs) (length xs)) region)))

(define (region-has? region x y)
  (memv x (region-row region y)))

(define (region-perimeter region)
  (apply + (hash-map->list (λ (y xs)
                             (apply + (map (λ (x)
                                             (let ((ntop? (region-has? region x (1- y)))
                                                   (nleft? (region-has? region (1- x) y)))
                                               (cond ((and ntop? nleft?) 0)
                                                     ((or ntop? nleft?) 2)
                                                     (else 4))))
                                           xs)))
                           region)))

(define (fence-price region)
  (* (region-area region) (region-perimeter region)))

(define total-price (apply + (hash-map->list (λ (k regions) (apply + (map fence-price regions)))
                                             all-regions)))
(format #t "Part 1: ~a\n" total-price)

(define (region-sides region)
  (apply + (hash-map->list (λ (y xs)
                             (apply + (map (λ (x)
                                             (let ((n? (region-has? region x (1- y)))
                                                   (w? (region-has? region (1- x) y))
                                                   (e? (region-has? region (1+ x) y))
                                                   (s? (region-has? region x (1+ y)))
                                                   (nw? (region-has? region (1- x) (1- y)))
                                                   (ne? (region-has? region (1+ x) (1- y)))
                                                   (sw? (region-has? region (1- x) (1+ y)))
                                                   (se? (region-has? region (1+ x) (1+ y))))
                                               (length (filter identity (list
                                                                         (and (not w?) (or nw? (not n?))) ; west fence
                                                                         (and (not n?) (or nw? (not w?))) ; north fence
                                                                         (and (not e?) (or ne? (not n?))) ; east fence
                                                                         (and (not s?) (or sw? (not w?))))))))
                                           xs)))
                           region)))

(define (fence-price-discount region)
  (* (region-area region) (region-sides region)))


(define total-price-discount (apply + (hash-map->list (λ (k regions) (apply + (map fence-price-discount regions)))
                                                      all-regions)))
(format #t "Part 2: ~a\n" total-price-discount)

