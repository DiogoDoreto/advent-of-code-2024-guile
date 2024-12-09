(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (ice-9 pretty-print)
             (srfi srfi-1))

(define biginput (read-file-as-string "./aoc-08-input.txt"))
(define smallinput "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")


(define city-map (string->nllist biginput))
(define freq-hash-list (make-hash-table))

(define antinodes (make-hash-table))
(define (antinode-add coord)
  (when (grid-has-coord? city-map coord)
    (hash-set! antinodes coord #t)))

(define* (parse-frequencies #:optional (coord (make-coord 0 0)))
  (when (not (nil? coord))
    (let ((freq (grid-ref city-map coord)))
      (when (not (eqv? freq #\.))
        (hashv-set! freq-hash-list freq (cons coord (hashv-ref freq-hash-list freq '()))))
      (parse-frequencies (grid-next-coord city-map coord)))))

(parse-frequencies)

(define (calc-antinodes c1 c2)
  (let* ((d1 (coord-diff c1 c2))
         (d2 (coord-inverse d1)))
    (antinode-add (coord-add c1 d2))
    (antinode-add (coord-add c2 d1))))

(hash-for-each (lambda (freq coord-lst)
                 (combine-pairs-for-each calc-antinodes coord-lst))
               freq-hash-list)

(format #t "Part 1: ~a\n" (hash-count (const #t) antinodes))
