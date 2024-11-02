#lang racket

(define (index-of item lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) -1)
          ((equal? (car lst) item) i)
          (else (loop (cdr lst) (+ i 1))))))

(define (sublist lst start end)
  (if (or (>= start end) (>= start (length lst)))
      '()
      (cons (list-ref lst start) (sublist lst (+ start 1) end))))

(define (swap lst i j)
  (let ((elem-i (list-ref lst i))
        (elem-j (list-ref lst j)))
    (append (sublist lst 0 i)          ; Elements before index i
            (list elem-j)              ; Element at index j
            (sublist lst (+ i 1) j)    ; Elements between i and j
            (list elem-i)              ; Element at index i
            (sublist lst (+ j 1) (length lst))))) ; Elements after index j


(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

(define (move-up state)
  (let ((index (index-of 'B state)))
    (if (< index 3)
        '()  ; Top row, can't move up
        (swap (list-copy state) index (- index 3)))))

(define (move-down state)
  (let ((index (index-of 'B state)))
    (if (> index 5)
        '()  ; Bottom row, can't move down
        (swap (list-copy state) index (+ index 3)))))

(define (move-left state)
  (let ((index (index-of 'B state)))
    (if (= (modulo index 3) 0)
        '()  ; Leftmost column, can't move left
        (swap (list-copy state) index (- index 1)))))

(define (move-right state)
  (let ((index (index-of 'B state)))
    (if (= (modulo index 3) 2)
        '()  ; Rightmost column, can't move right
        (swap (list-copy state) index (+ index 1)))))

(define (bfs-8-puzzle initial-state goal-state)
  (let loop ((queue (list (list initial-state)))
             (visited '()))
    (if (null? queue)
        '()  ; No solution found
        (let* ((path (car queue))
               (current-state (if (null? path) '() (car (reverse path)))))
          (if (equal? current-state goal-state)
              (reverse path)  ; Found the solution
              (if (member current-state visited)
                  (loop (cdr queue) visited)
                  (let ((new-paths
                         (filter (lambda (new-state)
                                   (and (not (null? new-state))
                                        (not (member new-state visited))))
                                 (map (lambda (move) (move current-state))
                                      (list move-up move-down move-left move-right)))))
                    (loop (append (cdr queue) (map (lambda (state) (append path (list state))) new-paths))
                          (cons current-state visited)))))))))


(define (print-8-puzzle solution)
  (for-each
   (lambda (state)
     (do ((i 0 (+ i 3)))
         ((= i 9))
       (display (list-ref state i))
       (display " ")
       (display (list-ref state (+ i 1)))
       (display " ")
       (display (list-ref state (+ i 2)))
       (newline))
     (newline))
   solution))