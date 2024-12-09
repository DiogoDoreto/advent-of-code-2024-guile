(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1))

(define biginput (read-file-as-string "./aoc-07-input.txt"))
(define smallinput "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(define (parse-text text)
  (map parse-numbers (string-split text #\newline)))

(define (solvable? ops total head . rest)
  (if (nil? rest)
      (= total head)
      (any (lambda (op) (apply solvable? ops total (op head (car rest)) (cdr rest)))
           ops)))

(define (sum-solvables ops input)
  (apply +
         (map car
              (filter (lambda (line) (and (not (nil? line)) (apply solvable? ops line)))
                      (parse-text input)))))

(format #t "Part 1: ~a\n" (sum-solvables (list + *) biginput))

(define (|| a b)
  (string->number (string-concatenate (list (number->string a)
                                            (number->string b)))))

(format #t "Part 2: ~a\n" (sum-solvables (list + * ||) biginput))
