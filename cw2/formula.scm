;;; Coursework 2
;;; Question 1
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define formula?
 (lambda (e)
  
  (define reserved-word?
   (lambda (e)
    (or (eqv? e 'not) 
        (eqv? e 'and)
    (eqv? e 'or))))

  (define wffs? 
   (lambda (e)
    (cond ((null? e) #f)
          ((and (wff? (car e)) (null? (cdr e))) #t) 
          ((and (wff? (car e)) (wffs? (cdr e))) #t)
          (else #f))))
    
  (define wff?
   (lambda (e)
    (cond ((and (not (reserved-word? e)) (symbol? e)) #t)
          ((pair? e) (cond ((and (eqv? (car e) 'not) 
                                 (pair? (cdr e)) 
                                 (null? (cddr e))  
                                 (wff? (cadr e))) #t)
                     ((and (eqv? (car e) 'and) (wffs? (cdr e))) #t)
                     ((and (eqv? (car e) 'or) (wffs? (cdr e))) #t)
                     (else #f)))
          (else #f))))
  
  (wff? e)))

; I chose to name the functions wff? and wffs? (and call wff? from formula?) 
; to help improve readability. 
