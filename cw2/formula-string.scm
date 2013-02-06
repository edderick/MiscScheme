;;; Coursework 2
;;; Question 2
;;; Edward Seabrook ejfs1g10@ecs.soton.ac.uk

(define formula->string
 (lambda (e)

  (define or-char #\v)
  (define and-char #\^)
  (define not-char #\~)

  (define capitalise
   (lambda (l)
    (if (null? l) 
        '() 
        (cons (char-upcase (car l)) (capitalise (cdr l))))))

  (define wff->infix
   (lambda (e)
    (cond ((symbol? e) (capitalise (string->list (symbol->string e))))
          ((eqv? (car e) 'not) (not->infix(cdr e)))
          ((eqv? (car e) 'and) (wffs->infix(cdr e) and-char))
          ((eqv? (car e) 'or) (wffs->infix(cdr e) or-char)))))

  (define not->infix 
   (lambda (e)
    (cond ((symbol? (car e)) (cons not-char (wff->infix (car e))))
          ((pair? (car e)) (if (eqv? (caar e) 'not)
                               (cons not-char (wff->infix (car e)))
                               (cons not-char (cons #\( (append (wff->infix(car e)) (list #\) )))))))))
  
  (define wffs->infix 
   (lambda (e c)
    (let ((operators-different? 
     (lambda (s) 
      (or (and (eqv? (caar s) 'or) (eqv? c and-char)) 
          (and (eqv? (caar s) 'and) (eqv? c or-char)))))) 
    (if (and (pair? (car e)) (operators-different? e)) 
       (if (null? (cdr e))
            (cons #\( (append (wff->infix (car e)) (list #\) )))
            (cons #\( (append (wff->infix (car e)) (cons #\) (cons #\space (cons c (cons #\space (wffs->infix (cdr e) c))))))))
       (if (null? (cdr e)) 
            (wff->infix (car e))
            (append (wff->infix (car e)) (cons #\space (cons c (cons #\space (wffs->infix (cdr e) c))))))))))
       
  (list->string (wff->infix e))))

; I chose to work in lists so that I could use car instead of append where possible. 
; Constants for the characters have been used incase they are changed, and to improve readability.
; The strings are constructed as a flat list of chars using append only where neccessary. It would be possible to
; construct a non-flat list and then flatten it, but this would require as many if not more appends. 
