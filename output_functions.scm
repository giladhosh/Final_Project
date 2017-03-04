;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Functions used to get prolog and epilog;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define get-prolog
  (lambda()
 (string-append 
"#define DO_SHOW 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include \"arch/cisc.h\"
#include \"debug_macros.h\"

int main(){
	START_MACHINE;
	JUMP(START);
	#include \"arch/char.lib\"
	#include \"arch/io.lib\"
	#include \"arch/math.lib\"
	#include \"arch/string.lib\"
	#include \"arch/system.lib\"
	#include \"arch/scheme.lib\"
	#include \"arch/lib/scheme/types.inc\"

	#define SOB_VOID 1
	#define SOB_NIL 2
	#define SOB_TRUE 3
	#define SOB_FALSE 5

START:
	MOV(R0, R0);
INFO;
CALL(MAKE_SOB_VOID);
CALL(MAKE_SOB_NIL);
PUSH(1);
CALL(MAKE_SOB_BOOL);
DROP(1);
PUSH(0);
CALL(MAKE_SOB_BOOL);
DROP(1);
/*end of prolog*/
")))


(define get-after-prolog
  (lambda ()
    (begin 
           (string-append (define-const-table-cisc) (load-const-table-cisc) ;making constant table string
                          ;(define-fvart-cisc) (load-fvar-ta-cisc) ;making fvar table string
                          )
    )))


(define get-epilog
  (lambda()
"INFO;

	CMP(R0, IMM(SOB_VOID));
	JUMP_EQ(L_no_print);
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(1);
	PUSH(IMM(10));
	CALL(PUTCHAR);
	DROP(1);
L_no_print: 
	STOP_MACHINE;
	return 0;
}
"))

#;(define get-epilog2
  (lambda()
    (string-append 
"JUMP(BEFORE_END);        

 L_ERR_ARG_TYPE:
 SHOW(\"WRONG ARGUMENT TYPE\", R0);
 JUMP(BEFORE_END);

 L_ERR_APPLY_NON_CLOSURE:
 SHOW(\"TRYING TO APPLY NON CLOSURE\", INDD(R0,0));
 JUMP(BEFORE_END);
     
  L_ERROR_LAMBDA_ARGS_COUNT:
  SHOW(\"WRONG LAMBADA ARGS COUNT\", FPARG(1));
  SHOW(\"TEST RESULT\", TR);
  JUMP(BEFORE_END);

  BEFORE_END:
  CMP(R0,IMM(722689));
  JUMP_EQ(END);

  END:
  STOP_MACHINE;

  return 0;}"
)))

(define make-full-code
  (lambda(cisc-output code-gen-ans)
    (write-file (string-append (get-prolog) (get-after-prolog) " \n "  " \n " (get-epilog)) ;add code gen
                cisc-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;read from file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file->sexpr
  (lambda (input)
    (let ((file2sexpr-ans  (car (list->sexpr (file->list input)))))
      (print_all (list (cons "file2sexpr-ans" file2sexpr-ans)))
      file2sexpr-ans
      )))


(define list->sexpr
  (lambda(file-as-list)
    (let ((cont (lambda(match remaining) 
                  (if (null? remaining) 
                      (list match) 
                      (cons match (list->sexpr remaining)))))
          (fail (lambda(x) `(failed ,x))))
      (<Sexpr> file-as-list cont fail) 
      )))

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (run)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;write to file borrowed;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Write to file.
(define write-file
  (lambda (text file)
    (let ((out (open-output-file file 'replace)))
      (begin
        (display text out)
        (close-output-port out)))))

;Holds the file port for writing to the output file
(define *out-file-port* 0)
;Opens the output file for writing and saves its port
(define fopen
  (lambda (out-file)
    (set! *out-file-port* (open-output-file out-file 'replace))))
;Writes to the output file
(define fwrite
  (lambda texts
    (if (not (null? texts))
        (begin (display (car texts) *out-file-port*)
               (apply fwrite (cdr texts))))))
;Closes the output file
(define fclose
  (lambda ()
    (begin
      (close-output-port *out-file-port*)
      (set! *out-file-port* 0))))

(define write-in-code
  (lambda (txt)
   (write-file txt "out.txt")))