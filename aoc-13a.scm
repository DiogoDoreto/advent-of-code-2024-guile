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

(define (solve ax bx px)
  (lambda (a-count)
    (define-values (b-count b-rem) (euclidean/ (- px (* ax a-count)) bx))
    (if (= 0 b-rem) b-count #f)))

(define* (decrement from #:optional (to 0))
  (let ((next from))
    (lambda ()
      (if (< next to) (eof-object)
          (let ((current next))
            (set! next (1- next))
            current)))))

(define (results-for-x a b prize)
  (let* ((ax (coord-x a))
         (bx (coord-x b))
         (px (coord-x prize))
         (solve-b-count (solve ax bx px)))
    (generator-transduce (compose (tmap (λ (a-count) (cons a-count (solve-b-count a-count))))
                                  (tfilter (λ (v) (cdr v))))
                         rcons (decrement (euclidean-quotient px ax)))))

(define (play-cost values)
  (+ (* 3 (car values)) (cdr values)))

(define (cost-to-prize a b prize)
  (define (valid? guess)
    (let ((a-count (car guess)) (b-count (cdr guess)))
      (equal? prize (coord-add (coord-multiply a a-count)
                               (coord-multiply b b-count)))))
  (let* ((guesses (results-for-x a b prize))
         (results (filter valid? guesses))
         (costs (map play-cost results)))
    (if (nil? costs) #f
        (apply min costs))))

(format #t "Part 1: ~a\n"
        (string-transduce (compose (tpartition (λ (c) (char=? c #\newline)))
                                   (tmap list->string)
                                   (tappend-map (λ (s) (string-tokenize s char-set:digit)))
                                   (tmap string->number)
                                   (tsegment 2)
                                   (tmap (λ (pair) (apply make-coord pair)))
                                   (tsegment 3)
                                   (tmap (λ (values) (apply cost-to-prize values)))
                                   (tfilter identity))
                          + biginput))
