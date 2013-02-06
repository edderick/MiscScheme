;;; Coursework 2
;;; Question 4
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define formula->nnf 
 (lambda (e)

  (define atom? 
   (lambda (s)
    (not (pair? s))))

  (define wff->nnf 
   (lambda (e)
    (cond ((symbol? e) e)
          ((eqv? (car e) 'not) (not->nnf e))
          (else e))))

  (define wffs->nnf
   (lambda (e)
    (if (null? e) 
        '()
        (cons (wff->nnf (cons 'not (list (car e)))) (wffs->nnf (cdr e))))))

  (define not->nnf
   (lambda (e) 
    (cond ((atom? (cadr e)) e)
          ((eqv? (caadr e) 'not) (wff->nnf (cadadr e))) 
          ((eqv? (caadr e) 'and) (cons 'or (wffs->nnf (cdadr e))))
          ((eqv? (caadr e) 'or) (cons 'and (wffs->nnf (cdadr e))))
          (else e))))

  (wff->nnf e)))

; I chose to keep the name of the function as wff->nnf (and call it from 
; formula->nnf) to aid readability.
