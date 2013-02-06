;;; Coursework 2
;;; Question 3
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define evaluate-formula
 (lambda (e al)

  (define evaluate-wff 
   (lambda (e)
    (cond ((symbol? e) (cdr (assv e al)))
          ((eqv? (car e) 'not) (not (evaluate-wff (cadr e))))
          ((eqv? (car e) 'and) (evaluate-wffs (cdr e) 'and))
          ((eqv? (car e) 'or) (evaluate-wffs (cdr e) 'or)))))

  (define evaluate-wffs
   (lambda (e op)
    (let (( wff (lambda (s) (evaluate-wff (car s)))) 
          ( wffs (lambda (s) (evaluate-wffs (cdr s) op))))
     (if (null? (cdr e)) 
         (wff e)
         (cond ((eqv? op 'and) (and (wff e) (wffs e)))
               ((eqv? op 'or) (or (wff e) (wffs e))))))))
  
  (evaluate-wff e)))

; I chose to name the functions evaluate-wff and evaluate-wffs, so that
; it is clear to see what they aim to do based on the grammar. I felt 
; for the sake of readability it was neccessary to keep these names and 
; call them from evaluate-formula.
