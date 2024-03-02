(define (calc) 
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:
; Map helps to give arbitrary many recursive calls inside a list.

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((word? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (accumulate + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + 0 (cdr args))))))
	((eq? fn '*) (accumulate * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate * 1 (cdr args))))))
	((eq? fn 'first) (string->symbol (list->string (list (string-ref (symbol->string (car args)) 0) #\null))))
	((eq? fn 'butfirst) (string->symbol(list->string (cdr (string->list (symbol->string (car args)))))))
	((eq? fn 'last) (string-ref (symbol->string (car args)) (- (string-length (symbol->string (car args))) 1)))
	((eq? fn 'word) (string->symbol (string-append (symbol->string (car args)) (symbol->string (cadr args)))))
	(else (error "Calc: bad operator:" fn))))



