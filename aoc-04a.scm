(use-modules (ice-9 textual-ports)
             (ice-9 exceptions))

(define biginput (get-string-all (open "./aoc-04-input.txt" O_RDONLY)))
(define input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(define (string->nllist text)
  "split a string by newlines and then each line into a list of chars"
  (map string->list (string-split text #\newline)))

(define (grid-ref grid coord)
  (guard (ex (else '()))
    (let ((x (car coord)) (y (cdr coord)))
      (list-ref (list-ref grid y) x))))

(define (grid-next-coord grid coord)
  (let* ((x (car coord))
         (y (cdr coord))
         (row (list-ref grid y)))
    (cond ((< x (1- (length row))) (cons (1+ x) y))
          ((< y (1- (length grid))) (cons 0 (1+ y)))
          (else '()))))

(define (next-x-coord grid coord)
  (if (or (nil? coord) (eqv? #\X (grid-ref grid coord))) coord
      (next-x-coord grid (grid-next-coord grid coord))))

(define xmas (string->list "XMAS"))

(define (match-xmas grid start delta)
  (define (step coord) (cons (+ (car coord) (car delta))
                             (+ (cdr coord) (cdr delta))))
  (define (iter coord chars)
    (cond ((nil? chars) #t)
          ((not (eqv? (car chars) (grid-ref grid coord))) #f)
          (else (iter (step coord) (cdr chars)))))
  (iter start xmas))

(define (count-xmas-around grid coord)
  (apply + (map (lambda (delta)
                  (if (match-xmas grid coord delta) 1 0))
                (list '(-1 . -1) '(0 . -1) '(1 . -1)
                      '(-1 .  0)           '(1 .  0)
                      '(-1 .  1) '(0 .  1) '(1 .  1)))))

(define (count-xmas textinput)
  (let ((haystack (string->nllist textinput)))
    (define (iter count coord)
      (let ((x-coord (next-x-coord haystack coord)))
        (if (nil? x-coord) count
            (iter (+ count (count-xmas-around haystack x-coord)) (grid-next-coord haystack x-coord)))))
    (iter 0 (cons 0 0))))

(count-xmas biginput)
