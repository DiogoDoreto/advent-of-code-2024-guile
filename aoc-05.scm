(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define biginput (get-string-all (open "./aoc-05-input.txt" O_RDONLY)))
(define input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(define (line-kind line)
  (let ((index (string-index line (char-set #\| #\,))))
    (case (and index (string-ref line index))
      ((#\|) 'rule)
      ((#\,) 'pages))))

(define (parse-nums text)
  (map string->number (string-tokenize text char-set:digit)))

(define rules '())
(define pages '())
(define (parse-input input)
  (for-each (lambda (line)
              (case (line-kind line)
                ((rule) (set! rules (append rules (list (parse-nums line)))))
                ((pages) (set! pages (append pages (list (parse-nums line)))))))
            (string-split input #\newline)))

(parse-input biginput)

(define (follows-rule? rule rev)
  (define (iter current-page . previous-pages)
    (cond ((nil? previous-pages) #t)
          ((and (= current-page (car rule)) (= (cadr rule) (car previous-pages))) #f)
          ((and (= current-page (car rule)) (not (apply iter (cons current-page (cdr previous-pages))))) #f)
          (else (apply iter previous-pages))))
  (apply iter rev))

(define (pages-valid? lst)
  (let ((rev (reverse lst)))
    (every (lambda (rule) (follows-rule? rule rev)) rules)))

(define (middle-value lst)
  (list-ref lst (/ (- (length lst) 1) 2)))

(format #t "Part 1: ~a\n"
        (apply + (map middle-value (filter pages-valid? pages))))

(define (find-prefixes-for-page postfix-page)
  (define (iter prefixes rules)
    (if (nil? rules) prefixes
        (let* ((rule (car rules))
               (r-pre (car rule))
               (r-post (cadr rule)))
          (iter (append prefixes (if (= r-post postfix-page) (list r-pre) '())) (cdr rules)))))
  (iter '() rules))

(define (included-in lst)
  (lambda (n) (any (lambda (k) (= n k)) lst)))

(define (fix-pages lst)
  (define (iter current-page . next-pages)
    (if (nil? next-pages) (list current-page)
        (let* ((prefix-nums (find-prefixes-for-page current-page))
               (index-problem (list-index (included-in prefix-nums) next-pages)))
          (if (not index-problem) (cons current-page (apply iter next-pages))
              (let ((problem-page (list-ref next-pages index-problem))
                    (before-problem (list-head next-pages index-problem))
                    (after-problem (list-tail next-pages (1+ index-problem))))
                (apply iter problem-page current-page (append before-problem after-problem)))))))
  (apply iter lst))

(format #t "Part 2: ~a\n"
        (apply + (map middle-value (map fix-pages (filter (lambda (lst) (not (pages-valid? lst))) pages)))))
