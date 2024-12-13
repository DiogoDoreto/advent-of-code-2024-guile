(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-171))

(define biginput (string-trim-right (read-file-as-string "./aoc-11-input.txt")))
(define smallinput "125 17")

(define initial-stone-list (map string->number (string-tokenize biginput char-set:digit)))

(define (blink-stone n)
  (if (= n 0) (list 1)
      (let* ((s (number->string n))
             (len (string-length s)))
        (if (even? len) (list (string->number (string-take s (/ len 2)))
                              (string->number (string-drop s (/ len 2))))
            (list (* n 2024))))))

(define sc-hash (make-hash-table))

(define (sc-hash-get n s)
  (let ((inner-hash (hashv-ref sc-hash s)))
    (if inner-hash (hashv-ref inner-hash n)
        (begin (hashv-set! sc-hash s (make-hash-table))
               #f))))

(define (sc-hash-put! n s v)
  (let ((inner-hash (hashv-ref sc-hash s)))
    (hashv-set! inner-hash n v)
    v))

(define (blink-stone-count n s)
  (or (sc-hash-get n s)
      (sc-hash-put! n s (blink-list-count (1- n) (blink-stone s)))))

(define (blink-list-count n lst)
  (if (= n 0) (length lst)
      (list-transduce (compose (tmap (λ (s) (blink-stone-count n s))))
                      + lst)))


(format #t "Part 1: ~a\n" (blink-list-count 25 initial-stone-list))
(format #t "Part 2: ~a\n" (blink-list-count 75 initial-stone-list))
