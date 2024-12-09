(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1))

(define biginput (read-file-as-string "./aoc-06-input.txt"))
(define smallinput "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(define guard-coord '())
(define guard-direction '())
(define guard-initial-coord '())
(define guard-initial-direction '())
(define lab-map '())

(define UP #\^)
(define RIGHT #\>)
(define DOWN #\v)
(define LEFT #\<)
(define guard-states (char-set UP RIGHT DOWN LEFT))

(define VISITED #\X)
(define FREE #\.)
(define BLOCKED #\#)

(define (mark-visited! coord)
  (grid-set! lab-map coord VISITED))

(define (mark-blocked! coord)
  (grid-set! lab-map coord BLOCKED))

(define (reset-guard)
  (set! guard-coord guard-initial-coord)
  (set! guard-direction guard-initial-direction))

(define (parse-map text)
  (set! lab-map (string->nllist text))
  (define (find-guard coord)
    (let ((char (grid-ref lab-map coord)))
      (if (char-set-contains? guard-states char)
          (set! guard-initial-coord coord)
          (find-guard (grid-next-coord lab-map coord)))))
  (find-guard (make-coord 0 0))
  (set! guard-initial-direction (grid-ref lab-map guard-initial-coord))
  (reset-guard)
  (mark-visited! guard-coord))

(parse-map biginput)

(define (guard-rotate)
  (set! guard-direction (case guard-direction
                          ((#\^) RIGHT)
                          ((#\>) DOWN)
                          ((#\v) LEFT)
                          ((#\<) UP))))

(define* (guard-move #:key mark-visited?)
  (let* ((next (case guard-direction
                 ((#\^) (coord-dy -1 guard-coord))
                 ((#\v) (coord-dy 1 guard-coord))
                 ((#\<) (coord-dx -1 guard-coord))
                 ((#\>) (coord-dx 1 guard-coord))))
         (char (and (grid-has-coord? lab-map next) (grid-ref lab-map next))))
    (cond ((not (grid-has-coord? lab-map next)) #f)
          ((eqv? char BLOCKED) (guard-rotate) #t)
          (else (when mark-visited? (mark-visited! next))
                (set! guard-coord next)
                #t))))

(while (guard-move #:mark-visited? #t))

(define (count-visited)
  (define (iter coord count)
    (if (nil? coord) count
        (iter (grid-next-coord lab-map coord)
              (if (eqv? (grid-ref lab-map coord) VISITED)
                  (1+ count)
                  count))))
  (iter (make-coord 0 0) 0))

(format #t "Part 1: ~a\n" (count-visited))

(define (print-map)
  (for-each (lambda (row)
              (format #t "~a\n" (list->string row)))
            lab-map))

(define (find-next-visited-slot from-coord)
  (let ((next (grid-next-coord lab-map from-coord)))
    (if (nil? next) '()
        (if (and (eqv? VISITED (grid-ref lab-map next))
                 (not (equal? next guard-initial-coord)))
            next
            (find-next-visited-slot next)))))

(define (map-has-loop? extra-block-coord)
  (mark-blocked! extra-block-coord)
  (reset-guard)
  (define (iter turns last-dir)
    (cond
     ((not (guard-move #:mark-visited? #f)) #f) ;; guard left map, no loop
     ((eqv? last-dir guard-direction) (iter turns last-dir)) ;; no change, keep walking
     ((find (λ (t) (equal? t (cons guard-direction guard-coord))) turns) #t) ;; found loop
     (else (iter (cons (cons guard-direction guard-coord) turns) guard-direction))))
  (define result (iter '() guard-direction))
  (mark-visited! extra-block-coord)
  result)

(define* (count-loops #:optional (count 0) (last-block (make-coord -1 0)))
  (let* ((next-block (find-next-visited-slot last-block))
         (loop? (and (not (nil? next-block)) (map-has-loop? next-block)))
         (next-count (if loop? (1+ count) count)))
    (if (nil? next-block)
        next-count
        (count-loops next-count next-block))))

(format #t "Part 2: ~a\n" (count-loops))
