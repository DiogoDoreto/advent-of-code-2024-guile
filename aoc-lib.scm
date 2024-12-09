(define-module (aoc-lib))

(use-modules (ice-9 textual-ports)
             (ice-9 exceptions))

(define-public (read-file-as-string path)
  (get-string-all (open path O_RDONLY)))

(define-public (parse-numbers text)
  "Return a list of numbers from string"
  (map string->number (string-tokenize text char-set:digit)))

(define-public (string->nllist text)
  "split a string by newlines and then each line into a list of chars"
  (map string->list (string-split text #\newline)))

(define-public make-coord cons)
(define-public coord-x car)
(define-public coord-y cdr)

(define-public (coord-dx dx coord)
  (make-coord (+ (coord-x coord) dx)
              (coord-y coord)))

(define-public (coord-dy dy coord)
  (make-coord (coord-x coord)
              (+ (coord-y coord) dy)))

(define-public (grid-ref grid coord)
  "Returns the value in COORD or nil"
  (if (grid-has-coord? grid coord)
      (list-ref (list-ref grid (coord-y coord)) (coord-x coord))
      '()))

(define-public (grid-next-coord grid coord)
  "Returns the next coord auto-wrapping to next row or nil"
  (let* ((x (coord-x coord))
         (y (coord-y coord))
         (row (list-ref grid y)))
    (cond ((< x (1- (length row))) (make-coord (1+ x) y))
          ((< y (1- (length grid))) (make-coord 0 (1+ y)))
          (else '()))))

(define-public (grid-has-coord? grid coord)
  (let ((x (coord-x coord)) (y (coord-y coord)))
    (and (>= x 0)
         (>= y 0)
         (< y (length grid))
         (< x (length (list-ref grid y))))))

(define-public (grid-set! grid coord value)
  (let ((row (list-ref grid (coord-y coord))))
    (list-set! row (coord-x coord) value)))
