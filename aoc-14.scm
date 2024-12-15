(load "./aoc-lib.scm")
(use-modules (aoc-lib)
             (srfi srfi-1)
             (srfi srfi-171)
             (srfi srfi-171 gnu))

(define (biginput)
  (open-input-file "./aoc-14-input.txt"))

(define (smallinput)
  (open-input-string "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"))

(define map-size
  (make-coord 101 103))
;; (make-coord 11 7))

(define (walk steps pos vel)
  (coord-mod (coord-add pos
                        (coord-multiply vel steps))
             map-size))

(define (quadrant coord)
  (define-values (x y) (car+cdr coord))
  (define-values (mx my) (car+cdr map-size))
  (let ((qx (euclidean-quotient mx 2))
        (qy (euclidean-quotient my 2)))
    (if (or (= x qx) (= y qy))
        #f
        (if (< y qy)
            (if (< x qx) 'Q1 'Q2)
            (if (< x qx) 'Q3 'Q4)))))

(define group-count
  (case-lambda
    (() (make-hash-table))
    ((hs) (hash-map->list cons hs))
    ((hs key)
     (when key
       (hashv-set! hs key (1+ (hashv-ref hs key 0))))
     hs)))

(define tparse-input (compose
                      tchar->numbers
                      (tsegment 2)
                      (tmap (λ (numbers) (apply make-coord numbers)))
                      (tsegment 2)))

(define (solve-part-1)
  (port-transduce (compose tparse-input
                           (tmap (λ (coords) (apply walk 100 coords)))
                           (tmap quadrant)
                           (tbatch group-count)
                           (tappend-map identity)
                           (tmap cdr))
                  * read-char (biginput)))

(format #t "Part 1: ~a\n" (solve-part-1))

(define* (walk-robot r #:optional (steps 1))
  (cons (apply walk steps r) (cdr r)))

(define (generate-part-2-data)
  (with-output-to-file "./aoc-14-output.js"
    (lambda ()
      (display "const robots = [\n")
      (let loop ((i 0) (robots (port-transduce (compose tparse-input
                                                        (tmap (λ (r) (walk-robot r 7000))))
                                               rcons read-char (biginput))))
        (when (< i 1000)
          (when (> i 0) (display ",\n"))
          (display "  [")
          (display
           (string-join (map (λ (r) (format #f "[~a,~a]" (caar r) (cdar r)))
                             robots)
                        ","))
          (display "]")
          (loop (+ i 1) (map walk-robot robots))))
      (format #t "\n]"))))

;; (generate-part-2-data)
