(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (ice-9 format))

(define input (string->nllist (string-trim-right (read-file-as-string "./aoc-16-input.txt"))))

(define (A* start is-goal? guess-score list-neighbors)
  (define open-set (pqueue-add (make-priority-queue) start 0))
  (define g-score (make-hash-table))
  (define f-score (make-hash-table))

  (hash-set! g-score start 0)
  (hash-set! f-score start (guess-score start))

  (define (loop)
    (when (not (nil? open-set))
      (let* ((current (pqueue-head open-set))
             (current-g-score (hash-ref g-score current)))
        (if (is-goal? current) current-g-score
            (begin
              (set! open-set (pqueue-drop open-set))
              (for-each (lambda (neighbor-score)
                          (let ((neighbor (car neighbor-score))
                                (tentative-g-score (+ current-g-score (cdr neighbor-score))))
                            (when (< tentative-g-score (hash-ref g-score neighbor (inf)))
                              (hash-set! g-score neighbor tentative-g-score)
                              (hash-set! f-score neighbor (+ tentative-g-score
                                                             (guess-score neighbor)))
                              (when (not (pqueue-has? open-set neighbor))
                                (set! open-set (pqueue-add open-set neighbor (+ tentative-g-score
                                                                                (guess-score neighbor))))))))
                        (list-neighbors current))
              (loop))))))
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

(format #t "Part 1: ~a\n" (A* start is-goal? guess-score list-neighbors))
