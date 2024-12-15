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

(define (make-wide-char c)
  (case c
    ((#\#) (list #\# #\#))
    ((#\.) (list #\. #\.))
    ((#\@) (list #\@ #\.))
    ((#\O) (list #\[ #\]))))

(define (make-wide-map input)
  (map (λ (row) (apply append (map make-wide-char row)))
       input))

(define (double-nl)
  (let ((prev '()))
    (lambda (current)
      (if (and (char=? current #\newline) (eqv? current prev)) #t
          (begin (set! prev current)
                 #f)))))

(define input-parts (string-split biginput (double-nl)))
(define wh-map (make-wide-map (string->nllist (car input-parts))))

(define (print-map)
  (for-each (λ (row) (format #t "~a\n" (list->string row))) wh-map))

(define (move-coord coord direction)
  (case direction
    ((#\^) (coord-dy -1 coord))
    ((#\v) (coord-dy 1 coord))
    ((#\<) (coord-dx -1 coord))
    ((#\>) (coord-dx 1 coord))))

(define (vertical? direction)
  (or (char=? #\^ direction)
      (char=? #\v direction)))

(define (is-box? c)
  (or (char=? #\[ c)
      (char=? #\] c)))

(define (box-other-half coord char)
  (coord-dx (if (char=? #\[ char) 1 -1) coord))

(define (can-move? coord direction check-second?)
  (let* ((next-coord (move-coord coord direction))
         (next-char (grid-ref wh-map next-coord))
         (curr-char (grid-ref wh-map coord)))
    (if (or (not (grid-has-coord? wh-map next-coord))
            (char=? #\# next-char)
            (and (not (char=? #\. next-char))
                 (not (can-move? next-coord direction #t))))
        #f
        (if (and (vertical? direction)
                 check-second?
                 (is-box? curr-char))
            (can-move? (box-other-half coord curr-char) direction #f)
            #t))))

(define (move-to coord direction move-second?)
  (let* ((next-coord (move-coord coord direction))
         (curr-char (grid-ref wh-map coord)))
    (when (and (vertical? direction)
               move-second?
               (is-box? curr-char))
      (move-to (box-other-half coord curr-char) direction #f))
    (when (not (char=? #\. (grid-ref wh-map next-coord)))
      (move-to next-coord direction #t))
    (grid-set! wh-map next-coord curr-char)
    (grid-set! wh-map coord #\.)
    next-coord))

(define (try-move coord direction)
  (and (can-move? coord direction #t)
       (move-to coord direction #t)))

(define robot (grid-find-coord wh-map (λ (c) (char=? c #\@))))
(define moves (filter (λ (c) (char-set-contains? (char-set #\^ #\> #\v #\<) c))
                      (string->list (cadr input-parts))))

(define (move-robot)
  (and=> (try-move robot (car moves))
         (λ (pos) (set! robot pos)))
  (set! moves (cdr moves)))

(while (not (nil? moves))
  (move-robot))

(define (sum-gps)
  (let loop ((c (make-coord 0 0))
             (sum 0))
    (if (nil? c) sum
        (loop (grid-next-coord wh-map c)
              (+ sum (if (char=? #\[ (grid-ref wh-map c))
                         (+ (coord-x c)
                            (* 100 (coord-y c)))
                         0))))))

(format #t "Part 2: ~a\n" (sum-gps))
