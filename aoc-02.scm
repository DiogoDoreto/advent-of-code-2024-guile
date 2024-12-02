(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 format))

(define biginput (get-string-all (open "./aoc-02-input.txt" O_RDONLY)))

(define smallinput "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(define input biginput)

(define (get_numbers line)
  (map string->number (string-tokenize line char-set:digit)))

(define num_lines (map get_numbers
                       (filter (lambda (line) (< 0 (string-length line)))
                               (string-split input #\newline))))

(define (safe_diff? a b)
  (let ((d (abs (- a b))))
    (and (> d 0) (< d 4))))

(define (safe? seq)
  (let ((op_check (if (< (car seq) (cadr seq)) < >)))
    (define (iter head tail)
      (cond ((nil? tail) #t)
            ((not (op_check head (car tail))) #f)
            ((not (safe_diff? head (car tail))) #f)
            (else (iter (car tail) (cdr tail)))))
    (iter (car seq) (cdr seq))))


(define (count_safe_lines safe_fn seq)
  (define (iter count lst)
    (if (nil? lst) count
        (let ((next_count (if (safe_fn (car lst)) (1+ count) count)))
          (iter next_count (cdr lst)))))
  (iter 0 seq))

(format #t "Part 1: ~a\n" (count_safe_lines safe? num_lines))

(define (ignore_index seq i)
  (define-values (before after) (split-at seq i))
  (append before (cdr after)))

(define (kinda_safe? seq)
  (or (safe? seq)
      (let ((len (length seq)))
        (define (iter i)
          (cond ((= i len) #f)
                ((safe? (ignore_index seq i)) #t)
                (else (iter (1+ i)))))
        (iter 0))))

(format #t "Part 2: ~a" (count_safe_lines kinda_safe? num_lines))
