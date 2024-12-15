(load "./aoc-lib.scm")
(use-modules (aoc-lib))

(define biginput (read-file-as-string "./aoc-15-input.txt"))
(define smallinput "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(define (double-nl)
  (let ((prev '()))
    (lambda (current)
      (if (and (char=? current #\newline) (eqv? current prev)) #t
          (begin (set! prev current)
                 #f)))))

(define input-parts (string-split biginput (double-nl)))
(define wh-map (string->nllist (car input-parts)))

(define (print-map)
  (for-each (λ (row) (format #t "~a\n" (list->string row))) wh-map))

(define (move-coord coord direction)
  (case direction
    ((#\^) (coord-dy -1 coord))
    ((#\v) (coord-dy 1 coord))
    ((#\<) (coord-dx -1 coord))
    ((#\>) (coord-dx 1 coord))))

(define (try-move coord direction)
  (let* ((next-coord (move-coord coord direction))
         (next-char (grid-ref wh-map next-coord)))
    (if (or (not (grid-has-coord? wh-map next-coord))
            (char=? #\# next-char)
            (and (not (char=? #\. next-char))
                 (not (try-move next-coord direction))))
        #f
        (begin (grid-set! wh-map next-coord (grid-ref wh-map coord))
               (grid-set! wh-map coord #\.)
               next-coord))))

(let loop ((robot (grid-find-coord wh-map (λ (c) (char=? c #\@))))
           (moves (filter (λ (c) (char-set-contains? (char-set #\^ #\> #\v #\<) c))
                          (string->list (cadr input-parts)))))
  (when (not (nil? moves))
    (loop (or (try-move robot (car moves)) robot)
          (cdr moves))))

(define (sum-gps)
  (let loop ((c (make-coord 0 0))
             (sum 0))
    (if (nil? c) sum
        (loop (grid-next-coord wh-map c)
              (+ sum (if (char=? #\O (grid-ref wh-map c))
                         (+ (coord-x c)
                            (* 100 (coord-y c)))
                         0))))))

(format #t "Part 1: ~a\n" (sum-gps))
