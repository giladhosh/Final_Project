;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      remww        ;;;;;
;;;;;    extra work     ;;;;;
;;;;;        in         ;;;;;
;;;;;   extra project   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;helper debugger, prints all pair items
; [List(Pair) -> void]
(define print_all
  (lambda (lst)
    (newline) ;
    (display (caar lst))
    (newline) ;
    (display (cdar lst))
    (if (null? (cdr lst))
        *void-object*
        (print_all (cdr lst)))))


(define lst-delete-element
  (lambda (lst x)
    (filter (lambda(z) (not(eq? z x))) lst)))


(define rem-xlst-lst
 (lambda (lst xs)
  (if (null? xs)
      lst
      (let ((firstx (car xs))
            (restxs (cdr xs)))
        (rem-xlst-lst (lst-delete-element lst firstx) restxs))
      )
 ))

(define add-2-lst
 (lambda (to add)
    (cond ((null? add) to)
          ((member (car add) to) (add-2-lst to (cdr add)))
          (else (add-2-lst `(,@to ,(car add)) (cdr add))))
 ))


(define LV-in
 (lambda (cmd registers)
  (add-2-lst (rem-xlst-lst registers 
                 (code-get3 cmd))   (code-get2 cmd))
 ))
 
(define build-acc
 (lambda (code regs)
    (if (null? code)
        `()
        (let ((first (car code))
              (rest (cdr code)))
          `(,`(,first ,regs)
          ,@(build-acc rest (LV-in first regs)))))
 ))

(define lst-tst
 (lambda ()
    (cons `(1 2 3 4 5 ) (list 1 2 3 4 5))
 ))

(define ^number-range-lst
 (lambda (n)
    (if (= n 0) `(0)
        `(,@(^number-range-lst (- n 1))
          ,n))
 ))

(define get-biggest-register
 (lambda (code last_pos)
    (cond
      ((number? code) (max last_pos code))
      ((or(null? code) (not (list? code))) last_pos)
          (else
           (let ((first (car code))
                 (rest (cdr code)))
             (max (get-biggest-register first last_pos)
                  (get-biggest-register rest last_pos)))))
 ))


(define has_member?
 (lambda (defs LV-out)
    (cond ((null? defs) #f)
          ((member (car defs) LV-out) #t)
          (else (has_member? (cdr defs)
                            LV-out)))
 )) 

(define build
 (lambda (code)
    (let ((registers (^number-range-lst (get-biggest-register code 15))))
        (reverse (build-acc (reverse code) 
                                        registers)))
 ))

(define is-dead?
 (lambda (cmd-and-registers)
    (if (null? (code-get3 (car cmd-and-registers))) #t
        (not (has_member? (code-get3 (car cmd-and-registers))
                         (code-get2 cmd-and-registers))))
 ))
 
(define remove-dead-puts
 (lambda (code)
    (if (null? code)
        `()
        (let ((first (car code))
              (rest  (cdr code)))
          (if (is-dead? first)
              (remove-dead-puts rest)
              `(,first
                ,@(remove-dead-puts rest))))
          )))
 
(define delete-bad-code
 (lambda (code)
    (let ((code-iter (remove-dead-puts code)))
        (if (equal? code code-iter)
            (map car code-iter)
            (delete-bad-code (build (map car code-iter)))))
 ))
 
(define remww
 (lambda (code)
    (if (null? code)
        `()
         (let* ((code-live-in-out (build code))
                    (ans (delete-bad-code code-live-in-out)))
           ans)
         )
 ))


(define code-get1
 (lambda (code)
  (car code)
   ))

(define code-get2
 (lambda (code)
  (cadr code)
   ))

(define code-get3
 (lambda (code)
  (caddr code)
 ))

 