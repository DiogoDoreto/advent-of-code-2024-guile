(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-41))

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

(define (combine-pairs-stream lst)
  (let ((head (car lst)) (tail (cdr lst)))
    (if (nil? tail) stream-null
        (stream-append (stream-map (lambda (b) (list head b))
                                   (list->stream tail))
                       (combine-pairs-stream tail)))))

(define (coord-in-city? coord)
  (grid-has-coord? city-map coord))

(define (generate-antinodes-delta coord delta)
  (stream-take-while coord-in-city?
                     (stream-iterate (λ (c) (coord-add c delta))
                                     coord)))

(define (generate-antinodes pair)
  (let* ((c1 (car pair))
         (c2 (cadr pair))
         (d1 (coord-diff c1 c2))
         (d2 (coord-inverse d1)))
    (stream-append (generate-antinodes-delta c1 d1)
                   (generate-antinodes-delta c2 d2))))

(hash-for-each (lambda (freq coord-lst)
                 (stream-for-each antinode-add
                                  (stream-concat (stream-map generate-antinodes
                                                             (combine-pairs-stream coord-lst)))))
               freq-hash-list)

(format #t "Part 2: ~a\n" (hash-count (const #t) antinodes))
