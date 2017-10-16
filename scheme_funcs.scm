;;;;;;;funcs for runtime;;;;;;;;;;;

(define map
  (lambda(proc lst)
    (if (null? lst) lst
        (cons (proc (car lst)) (map proc (cdr lst))))))


(define list
 (lambda x x))

(define append
	(lambda (list1 list2)
		(if (null? list1)
		list2
		(cons (car list1)
		(append (cdr list1) list2)))))
		
		
;(define append
;  (letrec ((helper (lambda(s)
;                    (if (null? s) '()
;                        (if (null? (cdr s)) (car s)
;                            (bin_append (car s) (helper (cdr s))))))))
;    (lambda s 
;      (helper s))))

