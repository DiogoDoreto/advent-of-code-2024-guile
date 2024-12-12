(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-171))

(define biginput (string-trim-right (read-file-as-string "./aoc-10-input.txt")))
(define smallinput "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(define topo-map (map (λ (row) (map char->number row))
                      (string->nllist biginput)))

(define (is-trailhead? coord) (= (grid-ref topo-map coord) 0))
(define (is-end-trail? coord) (= (grid-ref topo-map coord) 9))
(define (walkable? a b) (= 1 (- (grid-ref topo-map b)
                                (grid-ref topo-map a))))

(define (possible-paths coord)
  (filter (λ (c) (walkable? coord c))
          (filter (λ (c) (grid-has-coord? topo-map c))
                  (list (coord-dx 1 coord)
                        (coord-dx -1 coord)
                        (coord-dy 1 coord)
                        (coord-dy -1 coord)))))

(define (trail-ends coord)
  (if (is-end-trail? coord) (list coord)
      (apply lset-union equal? (map trail-ends (possible-paths coord)))))

(define (trail-rating coord)
  (if (is-end-trail? coord) 1
      (apply + (map trail-rating (possible-paths coord)))))

(define (generate-coords grid)
  (define coord (make-coord 0 0))
  (lambda ()
    (if (nil? coord) '()
        (let ((value coord))
          (set! coord (grid-next-coord grid coord))
          value))))

(define total-score (generator-transduce
                     (compose (ttake-while (negate nil?))
                              (tfilter is-trailhead?)
                              (tmap trail-ends)
                              (tmap length))
                     +
                     (generate-coords topo-map)))

(define total-rating (generator-transduce
                      (compose (ttake-while (negate nil?))
                               (tfilter is-trailhead?)
                               (tmap trail-rating))
                      +
                      (generate-coords topo-map)))

(format #t "Part 1: ~a\n" total-score)
(format #t "Part 2: ~a\n" total-rating)
