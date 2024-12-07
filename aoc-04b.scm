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

(define (next-char-coord grid coord char)
  (if (or (nil? coord) (eqv? char (grid-ref grid coord))) coord
      (next-char-coord grid (grid-next-coord grid coord) char)))

(define (match-word tailrow word)
  (cond ((nil? word) #t)
        ((nil? tailrow) #f)
        (else (let ((c (car tailrow))
                    (m (car word)))
                (if (and (not (eqv? m #\.)) (not (eqv? c m)))
                    #f
                    (match-word (cdr tailrow) (cdr word)))))))

(define (match-x-mas grid start x-mas)
  (let ((x (car start)) (y (cdr start)))
    (define (iter tailgrid tailword)
      (cond ((nil? tailword) #t)
            ((or (nil? tailgrid) (nil? (car tailgrid))) #f)
            ((not (match-word (list-tail (car tailgrid) x) (car tailword))) #f)
            (else (iter (cdr tailgrid) (cdr tailword)))))
    (iter (list-tail grid y) x-mas)))

(define x-mas-list (list (string->nllist "M.S\n.A.\nM.S")
                         (string->nllist "M.M\n.A.\nS.S")
                         (string->nllist "S.M\n.A.\nS.M")
                         (string->nllist "S.S\n.A.\nM.M")))

(define (count-x-mas-in-coord grid coord)
  (apply + (map (lambda (x-mas)
                  (if (match-x-mas grid coord x-mas) 1 0))
                x-mas-list)))

(define (count-x-mas textinput)
  (let ((haystack (string->nllist textinput)))
    (define (iter count coord)
      (if (nil? coord) count
          (iter (+ count (count-x-mas-in-coord haystack coord)) (grid-next-coord haystack coord))))
    (iter 0 (cons 0 0))))

(count-x-mas biginput)
