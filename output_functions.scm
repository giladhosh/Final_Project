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
//#include \"arch/debug_macros.h\"
#include \"arch/lib/scheme/types.inc\"
#define SOB_VOID 1
#define SOB_NIL 2
#define SOB_TRUE 3
#define SOB_FALSE 5
#define undefined 7
#define START_OF_SYMTBL 8

int main(){
	START_MACHINE;
 /* Allocate memory for constants and free variables */ 
     
CALL(MAKE_SOB_VOID);
CALL(MAKE_SOB_NIL);
PUSH(1);
CALL(MAKE_SOB_BOOL);
DROP(1);
PUSH(0);
CALL(MAKE_SOB_BOOL);
DROP(1);

JUMP(START);
	#include \"arch/char.lib\"
	#include \"arch/io.lib\"
	#include \"arch/math.lib\"
	#include \"arch/string.lib\"
	#include \"arch/system.lib\"
	#include \"arch/scheme.lib\"

        #include \"arch/our_functions.lib\"

START:
	MOV(R0, R0);
#include \"arch/runtime.asm\"

/*end of prolog*/
")))


(define get-after-prolog
  (lambda ()
    (begin
      (string-append (define-const-table-cisc) (load-const-table-cisc) ;making constant table string

                     (begin
                       (fvar-table-address-builder (get-fvar-table) (get-ctbl-end-address)) ;sets addresses after constable
                       (string-append (define-fvar-table) "\n" (load-fvar-table) "\n" ;making fvar table string
                                      "MOV(IND(0),IMM(" (number->string (fvar-table-end)) "));\n"  ; load sym table
                                      "MOV(IND(START_OF_SYMTBL),IMM(0));/*init symtbl start*/ \n"
                                      (fold-str (map (lambda(entry)
                                                       (if (symbol? (cadr entry))
                                                           (string-append "PUSH(IMM(" 
                                                                          (to-string (ctbl-entry-get-string-rep entry)) "));\n"
                                                                                     "PUSH(IMM(1));\n"
                                                                                     "PUSH(9000000);\n"
                                                                                     "CALL(L_lookup_string);\n"
                                                                                     "DROP(3);\n")
                                                           "")) (get-ctbl-entries (get-ctbl))))
                       ))))))


(define get-epilog
  (lambda()
"
JUMP(L_no_print);
L_error_not_proc:
  SHOW(\"Error not a procedure\",  FPARG(1));
  JUMP(Before_Ending);

L_error_not_pair:
  SHOW(\"Error not a pair\" ,  FPARG(1));
  JUMP(Before_Ending);

L_error_lambda_args_count:
 SHOW(\"Error Lambda arg count\" ,  FPARG(1));
  JUMP(Before_Ending);

Before_Ending:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;read from file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define string->sexpr
    (lambda(file-list)
        (<Sexpr> file-list
                 (lambda(match un-match)
                   (if (null? un-match)
                       (list match)
                       (cons match (string->sexpr un-match)))) (lambda(x) `(failed ,x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;write to file;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;junkyard;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
