;;; Coursework 1
;;; Question 1 
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define string-clean
 (lambda (s)
  (if (string? s) 
    (list-clean (string->list s)) 
    0)))

(define list-clean
 (lambda (l)
  (cond 
    ((null? l) l)
    ((char-alphabetic? (car l)) (append (list (char-downcase (car l))) (list-clean (cdr l))))
    (else (list-clean (cdr l))))))

;;; string-clean ensures it is a string, and then passes a list on to list-clean
