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
;Problem 15: multiplicate elements
