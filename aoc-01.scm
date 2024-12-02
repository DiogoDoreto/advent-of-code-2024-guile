(use-modules (srfi srfi-1)
             (ice-9 textual-ports))

(define biginput (get-string-all (open "./aoc-01-input.txt" O_RDONLY)))

(define smallinput "3   4
4   3
2   5
1   3
3   9
3   3")

(define (get_lines text)
  (filter (lambda (line) (< 0 (string-length line)))
          (string-split text #\newline)))

(define (get_numbers line)
  (map string->number (string-tokenize line char-set:digit)))

(define (-abs lst) (abs (- (car lst) (cadr lst))))

(define (run input)
  (let* ((lines (filter (lambda (line) (< 0 (string-length line)))
                        (string-split input #\newline)))
         (num_lines (map get_numbers lines))
         (left_numbers (sort (map car num_lines) <))
         (right_numbers (sort (map cadr num_lines) <))
         (distances (map -abs (zip left_numbers right_numbers))))
    (apply + distances)))

(run smallinput)
(run biginput)
