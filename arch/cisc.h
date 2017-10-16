/* cisc.h
 * Mock-assembly programming for a CISC-like architecture
 * 
 * Programmer: Mayer Goldberg, 2015
 */

#include <stdio.h>
#include <stdlib.h>

#define Bytes(n) (n)
#define Kilo(n) (Bytes(n) << 10)
#define Mega(n) (Kilo(n) << 10)
#define Giga(n) (Mega(n) << 10)

#define RAM_SIZE Mega(512)
#define STACK_SIZE Mega(256)

#define WORD_SIZE (sizeof(long))



/* Support for identifying memory issues                */
/* (stack over/underflow and RAM overuse), if activated */
/*
#define	TRACK_MEM
#ifdef	TRACK_MEM
	#define PUSH(x) { if (SP >= STACK_SIZE) goto Lstack_overflow; M(stack)[SP++] = (x); }
	#define POP(x) { if (SP <= 0) goto Lstack_underflow; (x) = M(stack)[--SP]; }
	#define DROP(n) { if (SP <= 0) goto Lstack_underflow; SP -= (n); }
	#define	MOVSP(n) { if ((n) > STACK_SIZE) goto Lstack_overflow; if ((n) < 0) goto Lstack_underflow; SP = (n); }
	#define	ADDSP(n) { if (SP + (n) > STACK_SIZE) goto Lstack_overflow; SP += (n); }
	#define	SUBSP(n) { if (SP - (n) < 0) goto Lstack_underflow; SP -= (n); }
	#define	INCRSP() { if (SP + 1 > STACK_SIZE) goto Lstack_overflow; ++SP; }
#else
	#define PUSH(x) { M(stack)[SP++] = (x); }
	#define POP(x) { (x) = M(stack)[--SP]; }
	#define DROP(n) { SP -= (n); }
	#define	ADDSP(n) { SP += (n); }
	#define	SUBSP(n) { SP -= (n); }
	#define	INCRSP() { ++SP; }
#endif
*/

//#define PUSH(x) { M(stack)[SP++] = (x); }



typedef struct Machine {
  long mem[RAM_SIZE];
  long stack[STACK_SIZE];
  long r0, r1, r2, r3, r4, r5, r6, r7, 
       r8, r9, r10, r11, r12, r13, r14, r15;
  long sp, fp;
  long test_result;
} Machine;


extern Machine *machine;

#define M(p) (machine->p)

#define L(x) ((long)(x))
#define LP(x) ((long *)(x))
#define LPR(x) (*LP(x))

#define R0 (M(r0))
#define R1 (M(r1))
#define R2 (M(r2))
#define R3 (M(r3))
#define R4 (M(r4))
#define R5 (M(r5))
#define R6 (M(r6))
#define R7 (M(r7))
#define R8 (M(r8))
#define R9 (M(r9))
#define R10 (M(r10))
#define R11 (M(r11))
#define R12 (M(r12))
#define R13 (M(r13))
#define R14 (M(r14))
#define R15 (M(r15))
#define SP (M(sp))
#define FP (M(fp))

#define PUSHALL { PUSH(R1); \
                  PUSH(R2); \
                  PUSH(R3); \
                  PUSH(R4); \
                  PUSH(R5); \
                  PUSH(R6); \
                  PUSH(R7); \
                  PUSH(R8); \
                  PUSH(R9); \
                  PUSH(R10); \
                  PUSH(R11); \
                  PUSH(R12); \
                  PUSH(R13); \
                  PUSH(R14); \
                  PUSH(R15); \
}

#define POPALL {  POP(R15); \
                  POP(R14); \
                  POP(R13); \
                  POP(R12); \
                  POP(R11); \
                  POP(R10); \
                  POP(R9); \
                  POP(R8); \
                  POP(R7); \
                  POP(R6); \
                  POP(R5); \
                  POP(R4); \
                  POP(R3); \
                  POP(R2); \
                  POP(R1); \
}

#define MINUS(x) { (x) = -(x); }

#define IMM(n) (L(n))
#define IND(r) (M(mem)[(r)])
#define INDD(r, d) (M(mem)[(r) + (d)])
#define ADDR(r) (IND(r)) /* ADDR(n) == IND(n) */
#define STACK(n) (M(stack)[(n)])
#define STARG(n) (STACK(SP-(n)-2))
#define FPARG(n) (STACK(FP-(n)-3))
#define SCMARG(n) (STACK(FP-(n)-5))
#define LOCAL(n) (STACK(FP+(n)))
#define LABEL(l) (L(&&l))
#define	NUM_ARGS (FPARG(1))
	#define	MOVSP(n) {  SP = (n); }
	#define	ADDSP(n) {  SP += (n); }

/* definition of the opcodes */

#define CMP(x, y) (M(test_result) = ((x) - (y)))
#define JUMP_GT(label) { if (M(test_result) > 0) goto label; }
#define JUMP_LT(label) { if (M(test_result) < 0) goto label; }
#define JUMP_GE(label) { if (M(test_result) >= 0) goto label; }
#define JUMP_LE(label) { if (M(test_result) <= 0) goto label; }
#define JUMP_EQ(label) { if (M(test_result) == 0) goto label; }
#define JUMP_NE(label) { if (M(test_result) != 0) goto label; }
#define JUMP(label) { goto label; }
#define JUMPA(address) { goto *(void *)address; }
#define PUSH(x) { M(stack)[SP++] = (x); }
#define POP(x) { (x) = M(stack)[--SP]; }
#define DROP(n) { SP -= (n); }
#define MOV(x, y) { (x) = (y); }

#define DECR(x) { --(x); }
#define INCR(x) { ++(x); }

#define ADD(x, y) { (x) += (y); }
#define SUB(x, y) { (x) -= (y); }
#define MUL(x, y) { (x) *= (y); }
#define DIV(x, y) { (x) /= (y); }
#define REM(x, y) { (x) %= (y); }

#define SHR(x, n) { (x) >>= n; }
#define SHL(x, n) { (x) <<= n; }

#define AND(x, y) { (x) = (x) & (y); }
#define OR(x, y) { (x) = (x) | (y); }
#define XOR(x, y) { (x) ^= (y); }
#define NEG(x) { (x) = ~(x); }

#define BEGIN_LOCAL_LABELS { __label__ 
#define END_LOCAL_LABELS }

#define CALL(subr) {			\
    __label__ Lcisc;			\
    M(stack)[SP++] = L(&&Lcisc);	\
    goto subr;				\
  Lcisc: ; }

#define CALLA(addr) {			\
    __label__ Lcisc;			\
    M(stack)[SP++] = L(&&Lcisc);	\
    goto *(void *)addr;			\
  Lcisc: ; }

#define RETURN { 			\
    goto *(void *)M(stack)[--SP]; }

#define NOP ;
#define HALT { exit(0); }

#define IN(x, y) {				\
  switch (y) {					\
    case 1: (x) = (long)getchar(); break;	\
    default: break;				\
  }						\
}

#define OUT(x, y) {				\
  switch (x) {					\
    case 2: putchar((char)(y & 255)); break;	\
    default: break;				\
  }						\
}

#define START_MACHINE 				\
  Machine *machine = 				\
    (Machine *)malloc(sizeof(Machine)); 	\
  MOV(SP, IMM(0));				\
  MOV(ADDR(0), IMM(1))

#define STOP_MACHINE				\
  free(machine)

/* Set to 0 for no-debug, 1 for trace, 2 for step: */
/* #define DO_SHOW 0 */

/* for debugging only, use SHOW("<some message>, <arg> */
#if DO_SHOW==2
#define SHOW(msg, x) { \
  printf("%s %s = %ld\n", (msg), (#x), (x)); \
  getchar(); }
#elif DO_SHOW==1
#define SHOW(msg, x) { printf("%s %s = %ld\n", (msg), (#x), (x)); }
#else
#define SHOW(msg, x) {}
#endif
