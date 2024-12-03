(use-modules (ice-9 peg)
             (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 format))

(define biginput (get-string-all (open "./aoc-03-input.txt" O_RDONLY)))

(define smallinput "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define input biginput)

(define-peg-string-patterns
  "value <- (operation/cmd/ignore)* !.
ignore < .
cmd <- DO / DONT
DO <- 'do()'
DONT <- 'don' ['] 't()'
operation <- op PL num CM num PR
PL < '('
PR < ')'
CM < ','
op <-- 'mul'
num <-- [0-9]+")
(define op-list (peg:tree (match-pattern value input)))

(define (parse-num num value)
  (string->number value))
(define (parse-op op a b)
  (* (apply parse-num a) (apply parse-num b)))
(define enabled #t)
(define (parse-cmd cmd)
  (if (string? cmd)
      (let () (set! enabled (if (equal? cmd "do()") #t #f))
           0)
      (if enabled (apply parse-op cmd) 0)))

(define result (fold (λ (op prev) (+ prev (parse-cmd op))) 0 op-list))

(format #t "Part 2: ~a" result)
