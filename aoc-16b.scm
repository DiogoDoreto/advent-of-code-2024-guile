(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (ice-9 format))

(define input (string->nllist (string-trim-right (read-file-as-string "./aoc-16-input.txt"))))

(define (A*mod start guess-score list-neighbors)
  (define open-set (pqueue-add (make-priority-queue) start 0))
  (define g-score (make-hash-table))
  (define f-score (make-hash-table))
  (define source (make-hash-table))

  (hash-set! g-score start 0)
  (hash-set! f-score start (guess-score start))

  (define (add-to-source from to cost)
    (define-values (cur-cost lst) (car+cdr (hash-ref source to (list (inf)))))
    (when (<= cost cur-cost)
      (hash-set! source to (cons cost (cons from lst)))))

  (define (loop)
    (if (nil? open-set) source
        (let* ((current (pqueue-head open-set))
               (current-g-score (hash-ref g-score current)))
          (set! open-set (pqueue-drop open-set))
          (for-each (lambda (neighbor-score)
                      (let* ((neighbor (car neighbor-score))
                             (tentative-g-score (+ current-g-score (cdr neighbor-score)))
                             (existing-g-score (hash-ref g-score neighbor (inf))))
                        (when (<= tentative-g-score existing-g-score)
                          (add-to-source current neighbor tentative-g-score))
                        (when (< tentative-g-score existing-g-score)
                          (hash-set! g-score neighbor tentative-g-score)
                          (hash-set! f-score neighbor (+ tentative-g-score
                                                         (guess-score neighbor)))
                          (when (not (pqueue-has? open-set neighbor))
                            (set! open-set (pqueue-add open-set neighbor (+ tentative-g-score
                                                                            (guess-score neighbor))))))))
                    (list-neighbors current))
          (loop))))
  (loop))

(define start (cons 'east (grid-find-coord input (lambda (c) (char=? c #\S)))))
(define goal (grid-find-coord input (lambda (c) (char=? c #\E))))

(define direction car)
(define coord cdr)

(define (is-goal? node)
  (equal? (coord node) goal))

(define (guess-score from)
  (let* ((dir (direction from))
         (diff (coord-diff (coord from) goal))
         (dx (coord-x diff))
         (dy (coord-y diff)))
    (+ (abs dx) (abs dy)
       (if (or (not (= 0 dx)) (not (= 0 dy))) 1000 0)
       (if (or (and (positive? dx) (eq? dir 'west))
               (and (negative? dx) (eq? dir 'east))
               (and (positive? dy) (eq? dir 'north))
               (and (negative? dy) (eq? dir 'south)))
           1000 0))))

(define (wall? c)
  (char=? (grid-ref input c) #\#))

(define (reverse? d1 d2)
  (or (and (eq? d1 'north) (eq? d2 'south))
      (and (eq? d1 'south) (eq? d2 'north))
      (and (eq? d1 'west) (eq? d2 'east))
      (and (eq? d1 'east) (eq? d2 'west))))

(define (list-neighbors node)
  (let ((dir (direction node))
        (c (coord node)))
    (let loop ((ds (list 'east 'west 'south 'north))
               (ns (list (coord-dx 1 c)
                         (coord-dx -1 c)
                         (coord-dy 1 c)
                         (coord-dy -1 c)))
               (res '()))
      (if (nil? ds) res
          (loop (cdr ds) (cdr ns)
                (if (or (wall? (car ns))
                        (reverse? (car ds) dir))
                    res
                    (cons (cons (cons (car ds) (car ns)) (+ 1 (if (eq? (car ds) dir) 0 1000)))
                          res)))))))

(define sources (A*mod start guess-score list-neighbors))
(define best-steps-set (make-hash-table))

(define (count-steps from)
  (hash-set! best-steps-set (coord from) #t)
  (let ((options (cdr (hash-ref sources from (list 0)))))
    ;; (for-each (λ (c) (hash-set! best-steps-set (coord c) #t)) options)
    (for-each count-steps options)))

(define (count-steps-goal)
  (let* ((final-steps (list
                       (hash-ref sources (cons 'north goal) (list (inf)))
                       (hash-ref sources (cons 'east goal) (list (inf)))
                       (hash-ref sources (cons 'south goal) (list (inf)))
                       (hash-ref sources (cons 'west goal) (list (inf)))))
         (min-cost (apply min (map car final-steps)))
         (best-finals (map cdr (filter (λ (s) (= min-cost (car s))) final-steps))))
    (hash-set! best-steps-set goal #t)
    (for-each count-steps (concatenate best-finals))
    (hash-count (const #t) best-steps-set)))

;; (define (print-map)
;;   (for-each (λ (row) (format #t "~a\n" (list->string row))) input))

;; (hash-for-each (λ (k v) (grid-set! input k #\O)) best-steps-set)
;; (print-map)

(format #t "Part 2: ~a\n" (count-steps-goal))
