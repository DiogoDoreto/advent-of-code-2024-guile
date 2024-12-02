(use-modules (srfi srfi-1)
             (ice-9 textual-ports))

(define biginput (get-string-all (open "./aoc-01-input.txt" O_RDONLY)))

(define smallinput "3   4
4   3
2   5
1   3
3   9
3   3")

(define input biginput)

(define (get_numbers line)
  (map string->number (string-tokenize line char-set:digit)))

(define num_lines (map get_numbers
                       (filter (lambda (line) (< 0 (string-length line)))
                               (string-split input #\newline))))

(define left_numbers (sort (map car num_lines) <))
(define right_numbers (sort (map cadr num_lines) <))

(define (sorted-count-items n lst)
  (define (iter c l)
    (if (or (nil? l) (not (= n (car l))))
        c
        (iter (1+ c) (cdr l))))
  (iter 0 (member n lst)))

(define s-score-table (make-hash-table 5))
(define (s-score-calc n)
  (let* ((c (sorted-count-items n right_numbers)))
    (hashq-set! s-score-table n (* n c))))

(define (s-score n)
  (or (hashq-ref s-score-table n) (s-score-calc n)))

(display (apply + (map s-score left_numbers)))
