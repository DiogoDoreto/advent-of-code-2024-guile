(define-module (aoc-lib))

(use-modules (ice-9 textual-ports)
             (ice-9 exceptions)
             (srfi srfi-171))

(define-public (read-file-as-string path)
  (get-string-all (open path O_RDONLY)))

(define-public char-set:number (char-set-adjoin char-set:digit #\-))

(define-public (parse-numbers text)
  "Return a list of numbers from string"
  (map string->number (string-tokenize text char-set:digit)))

(define-public (string->nllist text)
  "split a string by newlines and then each line into a list of chars"
  (map string->list (string-split (string-trim-right text) #\newline)))

(define-public (char->number c)
  (- (char->integer c) (char->integer #\0)))

(define-public make-coord cons)
(define-public coord-x car)
(define-public coord-y cdr)

(define-public (coord-dx dx coord)
  (make-coord (+ (coord-x coord) dx)
              (coord-y coord)))

(define-public (coord-dy dy coord)
  (make-coord (coord-x coord)
              (+ (coord-y coord) dy)))

(define-public (coord-op op c1 c2)
  (let ((c1x (coord-x c1))
        (c1y (coord-y c1))
        (c2x (coord-x c2))
        (c2y (coord-y c2)))
    (make-coord (op c1x c2x) (op c1y c2y))))

(define-public (coord-diff c1 c2)
  (coord-op - c2 c1))

(define-public (coord-add c1 c2)
  (coord-op + c1 c2))

(define-public (coord-mod c1 c2)
  (coord-op modulo c1 c2))

(define-public (coord-multiply coord n)
  (let ((x (coord-x coord))
        (y (coord-y coord)))
    (make-coord (* x n) (* y n))))

(define-public (coord-inverse coord)
  (coord-multiply coord -1))

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

(define-public (grid-find-coord grid pred)
  (let loop ((coord (make-coord 0 0)))
    (if (or (nil? coord) (pred (grid-ref grid coord))) coord
        (loop (grid-next-coord grid coord)))))

(define-public (combine-pairs-for-each proc lst)
  (let ((head (car lst)) (tail (cdr lst)))
    (when (not (nil? tail))
      (for-each (lambda (b) (proc head b)) tail)
      (combine-pairs-for-each proc tail))))

(define-public (pretty-print-hash-table ht)
  (hash-for-each (lambda (key value) (format #t "~a: ~a\n" key value))
                 ht))

(define* (member-right x lst #:optional (proc =))
  (if (nil? lst) #f
      (or (member-right x (cdr lst) proc)
          (and (proc x (car lst)) lst))))
(export member-right)

(define *eof-object* (read (open-input-string "")))
(define-public (eof-object)
  "useful for terminating generators"
  *eof-object*)

(define-public tchar->numbers (compose
                               (tpartition (λ (c) (char-set-contains? char-set:number c)))
                               (tfilter (λ (lst) (char-set-contains? char-set:number (car lst))))
                               (tmap list->string)
                               (tmap string->number)))

(define-public (make-priority-queue) '())

(define-public (pqueue-add q value cost)
  (if (or (nil? q) (< cost (caar q)))
      (cons (cons cost value) q)
      (cons (car q) (pqueue-add (cdr q) value cost))))

(define-public (pqueue-head q)
  (cdar q))

(define-public (pqueue-drop q)
  (cdr q))

(define-public (pqueue-has? q value)
  (and (not (nil? q))
       (or (equal? (pqueue-head q) value)
           (pqueue-has? (cdr q) value))))
