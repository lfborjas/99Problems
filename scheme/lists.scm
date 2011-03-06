;A stab at the 99 prolog problems
; taken from: http://www.haskell.org/haskellwiki/99_Haskell_exercises
; or : https://sites.google.com/site/prologsite/prolog-problems
; to work in this: 
; screen -S scheme ; scheme
; in vim: C-c C-c 

;Problem 1: get the last element from a list
(define (my-last l)
    (cond 
        ((empty? (cdr l)) (car l))
        (else (my-last (cdr l)))))

;Problem 2: get the last but one (avant-dernier) element of a list
(define (avant-dernier l)
    (cond
        ((empty? (cdr (cdr l))) (car l))
        (else (avant-dernier (cdr l)))))

;Problem 3: find the k-th element of a list
(define (element-at l n)
    (letrec 
        ((find-at 
            (lambda (l n curr)
                (cond 
                    ((empty? l) 'nil)
                    ((= curr n) (car l))
                    (else (find-at (cdr l) n (+ 1 curr)))))))
                    
         (find-at l n 1)))

;Problem 4: find the number of elements in a list
(define (len l)
    (letrec 
        ((list-len
            (lambda (l accum)
                (cond
                    ((empty? l) accum)
                    (else (list-len (cdr l) (+ 1 accum)))))))
       (list-len l 0)))

;Problem 5: reverse a list
(define (reversed l)
    (cond 
        ((empty? (cdr l)) (list (car l)))
        (else (append (reversed (cdr l)) (list (car l)) ))))

;Problem 6: find out whether a list is a palindrome
(define (palindrome? l)
    (or
        (empty? l)
        (equal? l (reversed l))))

;Problem 7: flatten a nested list
;the shame, the humanity, i ALMOST had it, but couldn't resist confirming it with SICP:
;http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#call_footnote_Temp_182
(define (flattened l)
    (cond 
        ((null? l) '())
        ((atom? l) (list l))
        (else (append (flattened (car l))
                      (flattened (cdr l))))))
            

;Problem 8: eliminate consecutive duplicates of list elements
(define (compress l)
    (letrec 
        ((do-compress
            (lambda (seq curr buff)
                (cond 
                    ((empty? seq) (append buff (list curr)))
                    ((eq? curr (car seq)) 
                        (do-compress (cdr seq) curr buff))
                    (else (do-compress (cdr seq) (car seq) (append buff (list curr))))))))
         (do-compress l (car l) '())))

;Problem 9: pack consecutive duplicates into sublists
(define (pack l)
    (letrec 
        ((do-pack
            (lambda (seq curr buff)
                (cond 
                    ((empty? seq) (append buff (list curr)))
                    ( (or  (empty? curr) (eq? (car curr) (car seq)))
                        (do-pack (cdr seq) (append curr (list (car seq))) buff))
                    (else (do-pack (cdr seq) (list (car seq)) (append buff (list curr))))))))
         (do-pack l '() '())))

;Problem 10: run-length encode a list
(define (encode l)
    (letrec 
        ([lists (pack l)]
         [count-encodings 
            (lambda (encodings count)
                (cond 
                    ((empty? encodings) count)
                    (else (count-encodings (cdr encodings) (append count (list (cons
                                                                            (length (car encodings)) 
                                                                            (list (caar encodings)))))))))])
         (count-encodings lists '())))
 
;Problem 11: modified run-length encoding: single elements don't become a frecuency pair
(define (encode-plus l)
    (letrec 
        ([lists (pack l)]
         [count-encodings 
            (lambda (encodings count)
                (cond 
                    ((empty? encodings) count)
                    (else (count-encodings 
                            (cdr encodings)
                            (append count  
                                            (or (and (= (length (car encodings)) 1) (car encodings))
                                            (list(cons
                                                    (length (car encodings)) 
                                                    (list (caar encodings))))))))))])
         (count-encodings lists '())))
        
;Problem 12: Decode a run-length encoded list
(define (decode l)
    (letrec 
        ((multi 
            ;look mom, default values!! http://docs.racket-lang.org/reference/define.html
            (lambda (times [element null] [accum '()]) 
                (cond 
                    ((null? element) (list times))
                    ((zero? times) accum)
                    (else (append (multi (- times 1) element accum) (list element))))))
         (do-decode
            (lambda (seq result)
                (cond 
                    ((empty? seq) result)
                    (else (do-decode (cdr seq) (append result (apply multi 
                                                                (or 
                                                                   (and (atom? (car seq)) (list (car seq)))
                                                                   (car seq))))))))))
         (do-decode l '())))
                    
;extracted from the above problem
(define (multiplicate times [element null] [accum '()])
    (cond 
        ((null? element) (list times))
        ((zero? times) accum)
        (else (append (multiplicate (- times 1) element accum) (list element)))))

;Problem 13: Direct run-length:
(define (direct-encode l)
    (letrec 
        ((do-encode
            (lambda (seq curr count buff)
                (cond 
                    ((empty? seq) (append buff (list (list count  curr))))
                    ((eq? curr (car seq)) 
                        (do-encode (cdr seq) curr (+ count 1) buff))
                    (else (do-encode (cdr seq) (car seq) 1 (append buff (list (list count curr)))))))))
         (do-encode l (car l) 0 '())))

;Problem 14: duplicate elements:
(define (duplicate l)
    (flatten (map (lambda (elem) (multiplicate 2 elem)) l)))
            
;Problem 15: multiplicate elements
;also, generalized for solving the previous problem with a default argument
(define (multiplicate-all l [t 2])
    (flatten (map (lambda (elem) (multiplicate t elem)) l)))

;Problem 16: Drop every n-th element of a list
; N.B: I'm using default args instead of a `letrec` for conciseness.
; If you have a puritanical but also imaginative mind, use it to rewrite
; this with a letrec, I -for one- am ok.
(define (drop-every l n [accum '()] [count 1])
    (cond
        ((empty? l) accum)
        ((= 0 (modulo count n)) (drop-every (cdr l) n accum (+ 1 count)))
        (else (drop-every (cdr l) n (append accum (list (car l))) (+ 1 count)))))

;Problem 17: Split a list in two parts, with the length of the first part given
(define (split l len [accum '()] [curr '()])
    (cond 
        ((empty? l) (append accum (list curr)))
        ((zero? len) (split (cdr l) -1 (append accum (list curr)) (list (car l))))
        (else (split (cdr l) (- len 1) accum (append curr (list (car l)))))))

;Problem 18: Extract a slice from a list, with both limits inclusive
(define (slice l start end [count 1] [accum '()])
    (cond 
        ((= count end) (append accum (list (car l))))
        ((>= count start) (slice (cdr l) start end (+ count 1) (append accum (list (car l)))))
        (else (slice (cdr l) start end (+ count 1) accum))))
        
;Problem 19: rotate a list N places to the left (using length, append and the solution to P17)
(define (rotate l n)
    (let 
        ([parts (split l (if (< n 0) (+ (length l) n) n))])
        (append (cadr parts) (car parts))))

;Problem 20: Remove the k-th element from a list    
(define (remove-at l p [count 1] [accum '()])
    (cond
        ((empty? l) accum)
        ((= p count) (append accum (cdr l)))
        (else (remove-at (cdr l) p (+ count 1) (append accum (list (car l)))))))

;Problem 21: insert an element at a given position
(define (insert-at e l p [count 1] [accum '()])
    (cond
        ((empty? l) accum)
        ((= p count) (append accum (list e) l))
        (else (insert-at e (cdr l) p (+ count 1) (append accum (list (car l)))))))

;Problem 22: create a list containing all integers within a given range
(define (range start end )
    (letrec
        ((do-range
            (lambda (start end count [accum '()])
                (cond
                    ((<= count end) (do-range start end (+ count 1) (append accum (list count))))
                    (else accum)))))
        (do-range start end start)))

;Problem 23: Extract a given number of randomly selected elements from a list
(define (take-random l n)
    (letrec
        ((take-them
            (lambda (l ln count accum chosen)
                (cond
                    ((zero? count) (append accum (list (element-at l chosen))))
                    (else (take-them 
                                (remove-at l chosen)
                                (- ln 1)
                                (- count 1)
                                (append accum (list (element-at l chosen)))
                                (+ (random (- ln 1)) 1)))))))
         (take-them l (length l) (- n 1) '() (+ (random (length l)) 1))))

;Problem 24: Lotto: draw N numbers from the range 1..M
(define (lotto n m)
    (let 
       ((ballot (range 1 m)))
    (take-random! ballot n)))

;Problem 25: Generate a random permutation of the elements of a list
(define (random-permutation l)
    (take-random l (length l)))

;Problem 26: generate the combinations of k distinct objects chosen from the n elements of a list
;Problem 27: group the elements of a set into disjoint sets

;Problem 28a: sorting a list of lists according to their length

;First, my favorite sorting algorithm, generalized for getting the elements via a function
(define (quicksort l [elem (lambda (e) e)])
         (cond 
            ((empty? l) l)
            (else 
              (let ((s  (car l))
                   (xs (cdr l)))
                  (append
                    (append (quicksort 
                                (filter (lambda (e) (< (elem e) (elem s))) xs) elem) 
                            (list s))
                    (quicksort (filter (lambda (e) (>= (elem e) (elem s))) xs) elem))))))

;Now, the problem, with the selector function being a length observer:
(define( length-sort l)
    (quicksort l (lambda (e) (length e))))
;Problem 28b: sorting a list of lists according to their length frequency

