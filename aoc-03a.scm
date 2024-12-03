(use-modules (ice-9 peg)
             (srfi srfi-1)
             (ice-9 textual-ports)
             (ice-9 format))

(define biginput (get-string-all (open "./aoc-03-input.txt" O_RDONLY)))

(define smallinput "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(define input biginput)

(define-peg-string-patterns
  "value <- (operation/ignore)* !.
ignore < .
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

(define result (fold (λ (op prev) (+ prev (apply parse-op op))) 0 op-list))

(format #t "Part 1: ~a" result)
