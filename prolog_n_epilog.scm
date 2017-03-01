;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Functions used to get prolog and epilog;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;MAY NEED TO ADD arch directory to path of includes! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define get-prolog
  (lambda()
 (string-append 
  "#include <stdio.h>
#include <stdlib.h>
#define DO_SHOW 1
#include \"cisc.h\"

int main()
{
 START_MACHINE;
 JUMP(CONTINUE);
 #include \"char.lib\"
 #include \"io.lib\"
 #include \"math.lib\"
 #include \"string.lib\"
 #include \"system.lib\"
 CONTINUE:
")))

(define get-epilog
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
  (lambda(code-gen-ans)
    (string-append (get-prolog) " \n " code-gen-ans " \n " (get-epilog))))