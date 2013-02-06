;;; Coursework 1
;;; Question 3
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define upper-char-limit 122)
(define alphabet-length 26)

(define shift-char 
 (lambda (c n)
  (integer->char
   (if (> (+ (char->integer c) (modulo n alphabet-length)) upper-char-limit) 
    (- (+ (char->integer c) (modulo n alphabet-length)) alphabet-length)
    (+ (char->integer c) (modulo n alphabet-length))))))

(define caesar
 (lambda (l n)
  (if (null? l) 
   l
   (cons (shift-char (car l) n) (caesar (cdr l) n)))))

;;; two first definitions are to remove magic numbers
;;; shift-char - shifts a character n places
;;; caesar - performs the task on the list

