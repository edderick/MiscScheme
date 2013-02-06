;;; Coursework 1
;;; Question 2
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define remove-all 
 (lambda (c l)
  (if (null? l)
   '()
   (if (eqv? (car l) c) 
    (remove-all c (cdr l))
    (cons (car l) (remove-all c (cdr l)))))))

(define count 
 (lambda (c l)
  (if (null? l)
   0
   (if (eqv? (car l) c)
    (+ 1 (count c (cdr l)))
    (count c (cdr l))))))

(define insert
 (lambda (c n l)
  (if (null? l)
   (list (cons c n))
   (if (> n (cdar l))
    (cons (cons c n) l)
    (cons (car l) (insert c n (cdr l)))))))

(define frequency
 (lambda (l)
  (if (null? l) 
   '()
   (insert (car l) (count (car l) l) (frequency (remove-all (car l) l))))))

;;; remove-all - removes all occurrences of a character in a list
;;; count - counts the number of occurrences of a character in a list
;;; insert -  inserts a pair of character and frequency to the 
;;; correct position in a list
;;; frequency - performs the task on the list

