(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-171))

(define biginput (string-trim-right (read-file-as-string "./aoc-13-input.txt")))
(define smallinput "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(define (cost-to-prize a b prize)
  (let ((ax (coord-x a))
        (ay (coord-y a))
        (bx (coord-x b))
        (by (coord-y b))
        (px (coord-x prize))
        (py (coord-y prize)))
    (let ((a-count (/ (- (* px by) (* py bx))
                      (- (* ax by) (* ay bx))))
          (b-count (/ (- (* ax py) (* px ay))
                      (- (* ax by) (* ay bx)))))
      (and (integer? a-count)
           (integer? b-count)
           (+ b-count (* 3 a-count))))))

(define (cost-to-prize-corrected a b prize)
  (cost-to-prize a b (coord-add prize (make-coord 10000000000000
                                                  10000000000000))))

(format #t "Part 2: ~a\n"
        (string-transduce (compose (tpartition (λ (c) (char=? c #\newline)))
                                   (tmap list->string)
                                   (tappend-map (λ (s) (string-tokenize s char-set:digit)))
                                   (tmap string->number)
                                   (tsegment 2)
                                   (tmap (λ (pair) (apply make-coord pair)))
                                   (tsegment 3)
                                   (tmap (λ (values) (apply cost-to-prize-corrected values)))
                                   (tfilter identity))
                          + biginput))
