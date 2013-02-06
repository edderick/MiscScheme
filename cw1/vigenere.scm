;;; Coursework 1 
;;; Question 4
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define lower-char-limit 97)
(define upper-char-limit 122)
(define alphabet-length 26)

(define shift-char 
 (lambda (c n)
  (integer->char 
   (if (> (+ (char->integer c) n) upper-char-limit)
    (- (+ (char->integer c) n) alphabet-length)  
    (+ (char->integer c) n)))))

(define vigenere
 (lambda (l1 l2)
  (if (null? l1) 
   l1
   (cons 
    (shift-char (car l1) (if (null? l2) 
     0 
     (- (char->integer(car l2)) lower-char-limit ))) 
    (vigenere (cdr l1) (if (null? l2) 
     '() 
     (append (cdr l2) (list (car l2)))))))))

;;; first thre definitions remove magic numbers
;;; shift-char - shifts the characters in a positive direction.
;;; Accounting for negative numbers is not required as all shifts are positive

