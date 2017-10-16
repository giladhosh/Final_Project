/*

*/

#define BASE_START_ADDRESS  200           //address after static defines ; null, void, bools
#define CLOSURE_ENV         1000000     //fake env

	
#define MO_CLOSURE(address, env, code_label)	\
	MOV(INDD(address, 0), IMM(T_CLOSURE));		\
	MOV(INDD(address, 1), IMM(env));			\
	MOV(INDD(address, 2), LABEL(code_label));



#define	MAKE_CLOSURE_ADDRESS(index_num)     BASE_START_ADDRESS + (index_num * 3)


/* Give Addresses */
#define prim_apply MAKE_CLOSURE_ADDRESS(0)

#define prim_less_than MAKE_CLOSURE_ADDRESS(1)
#define prim_numeric_equals MAKE_CLOSURE_ADDRESS(2)
#define prim_greater_than MAKE_CLOSURE_ADDRESS(3)
#define prim_plus MAKE_CLOSURE_ADDRESS(4)
#define prim_division MAKE_CLOSURE_ADDRESS(5)
#define prim_minus MAKE_CLOSURE_ADDRESS(6)

#define is_boolean MAKE_CLOSURE_ADDRESS(7) //boolean?
#define prim_car MAKE_CLOSURE_ADDRESS(8)    //car
#define prim_cdr MAKE_CLOSURE_ADDRESS(9)    //cdr
#define prim_char_to_int MAKE_CLOSURE_ADDRESS(10)   

#define prim_is_char MAKE_CLOSURE_ADDRESS(11) //char?
#define prim_cons MAKE_CLOSURE_ADDRESS(12)
#define prim_is_eq MAKE_CLOSURE_ADDRESS(13) 
#define prim_is_integer MAKE_CLOSURE_ADDRESS(14)
#define prim_int_to_char MAKE_CLOSURE_ADDRESS(15)

#define prim_make_string MAKE_CLOSURE_ADDRESS(16)
#define prim_make_vector MAKE_CLOSURE_ADDRESS(17)
#define prim_is_null MAKE_CLOSURE_ADDRESS(18)
#define prim_is_number MAKE_CLOSURE_ADDRESS(19)
#define prim_is_pair MAKE_CLOSURE_ADDRESS(20)
#define prim_is_procedure MAKE_CLOSURE_ADDRESS(21)
#define prim_set_car MAKE_CLOSURE_ADDRESS(22)
#define prim_set_cdr MAKE_CLOSURE_ADDRESS(23)

#define prim_string_length MAKE_CLOSURE_ADDRESS(24)
#define prim_string_ref MAKE_CLOSURE_ADDRESS(25)
#define prim_string_set MAKE_CLOSURE_ADDRESS(26)
#define prim_string_to_symbol MAKE_CLOSURE_ADDRESS(27) //TODO
#define prim_is_string MAKE_CLOSURE_ADDRESS(28)
#define prim_is_symbol MAKE_CLOSURE_ADDRESS(29)
#define prim_symbol_to_string MAKE_CLOSURE_ADDRESS(30)

#define prim_vector_length MAKE_CLOSURE_ADDRESS(31)

#define prim_vector_ref MAKE_CLOSURE_ADDRESS(32)
#define prim_vector_set MAKE_CLOSURE_ADDRESS(33)
#define prim_is_vector MAKE_CLOSURE_ADDRESS(34)
#define prim_is_zero MAKE_CLOSURE_ADDRESS(35) 
#define prim_multiple MAKE_CLOSURE_ADDRESS(36) 

#define prim_vector MAKE_CLOSURE_ADDRESS(37)
#define prim_not MAKE_CLOSURE_ADDRESS(38)
#define prim_denom MAKE_CLOSURE_ADDRESS(39) 
#define prim_numer MAKE_CLOSURE_ADDRESS(40) 
#define prim_remainder MAKE_CLOSURE_ADDRESS(41)

//#define prim_is_rational MAKE_CLOSURE_ADDRESS(41)  //no need

//#define prim_numer MAKE_CLOSURE_ADDRESS(44) 

//prim_string_to_symbol
/**/
/* Make Closures */

MO_CLOSURE(prim_apply, CLOSURE_ENV, L_prim_apply)	//apply

MO_CLOSURE(prim_less_than, CLOSURE_ENV, L_prim_less_than)	// <
MO_CLOSURE(prim_numeric_equals, CLOSURE_ENV, L_math_eq)	//=
MO_CLOSURE(prim_greater_than, CLOSURE_ENV, L_prim_greater_than)	//>
//MO_CLOSURE(prim_plus, CLOSURE_ENV, L_prim_plusbin)	//+

MO_CLOSURE(prim_plus, CLOSURE_ENV, L_prim_plus)	//+

MO_CLOSURE(prim_division, CLOSURE_ENV, L_prim_division)	 // division
MO_CLOSURE(prim_minus, CLOSURE_ENV, L_minus)	// -


MO_CLOSURE(is_boolean, CLOSURE_ENV, L_prim_is_bool)	
MO_CLOSURE(prim_car, CLOSURE_ENV, L_prim_car)	//car
MO_CLOSURE(prim_cdr, CLOSURE_ENV, L_prim_cdr)	//cdr
MO_CLOSURE(prim_char_to_int, CLOSURE_ENV, L_char_to_int) //L_prim_char_to_int)	

MO_CLOSURE(prim_is_char, CLOSURE_ENV, IS_SOB_CHAR) // L_prim_is_char)	
MO_CLOSURE(prim_cons, CLOSURE_ENV, L_prim_cons)	
MO_CLOSURE(prim_is_eq, CLOSURE_ENV, L_prim_is_eq)	
MO_CLOSURE(prim_is_integer, CLOSURE_ENV, IS_SOB_INTEGER)    //L_prim_is_integer)	
MO_CLOSURE(prim_int_to_char, CLOSURE_ENV, L_prim_int_to_char) //L_prim_int_to_char)	

MO_CLOSURE(prim_make_string, CLOSURE_ENV, L_prim_make_string)//L_prim_make_string)	
MO_CLOSURE(prim_make_vector, CLOSURE_ENV, MAKE_SOB_VECTOR)//L_make_vector)	
MO_CLOSURE(prim_is_null, CLOSURE_ENV, IS_SOB_NIL) //L_prim_is_null)	
MO_CLOSURE(prim_is_number, CLOSURE_ENV, L_prim_is_number)	
MO_CLOSURE(prim_is_pair, CLOSURE_ENV, IS_SOB_PAIR) //L_prim_is_pair)	
MO_CLOSURE(prim_is_procedure, CLOSURE_ENV, IS_SOB_CLOSURE ) //L_prim_is_procedure)	
MO_CLOSURE(prim_set_car, CLOSURE_ENV, L_prim_set_car)	
MO_CLOSURE(prim_set_cdr, CLOSURE_ENV, L_prim_set_cdr)

MO_CLOSURE(prim_string_length, CLOSURE_ENV, STRLEN)
MO_CLOSURE(prim_string_ref, CLOSURE_ENV, L_prim_string_ref)
MO_CLOSURE(prim_string_set, CLOSURE_ENV, L_string_set) //
MO_CLOSURE(prim_string_to_symbol, CLOSURE_ENV, L_prim_string_to_symbol)
MO_CLOSURE(prim_is_string, CLOSURE_ENV, L_prim_is_string)
MO_CLOSURE(prim_is_symbol, CLOSURE_ENV, L_prim_is_symbol)
MO_CLOSURE(prim_symbol_to_string, CLOSURE_ENV, L_prim_symbol_to_string)

MO_CLOSURE(prim_vector_length, CLOSURE_ENV, L_prim_vector_length)
MO_CLOSURE(prim_vector_ref, CLOSURE_ENV, L_prim_vector_ref)
MO_CLOSURE(prim_vector_set, CLOSURE_ENV, L_prim_vector_set)
MO_CLOSURE(prim_is_vector, CLOSURE_ENV, L_prim_is_vector)
MO_CLOSURE(prim_is_zero, CLOSURE_ENV, L_is_zero) //L_prim_is_zero) //zero?
MO_CLOSURE(prim_multiple, CLOSURE_ENV, L_prim_multiple) // multiply

MO_CLOSURE(prim_vector, CLOSURE_ENV, L_vector)
MO_CLOSURE(prim_not, CLOSURE_ENV, L_not)
MO_CLOSURE(prim_denom, CLOSURE_ENV, L_denominator) //
MO_CLOSURE(prim_numer, CLOSURE_ENV, L_numerator) //

MO_CLOSURE(prim_remainder, CLOSURE_ENV, L_remainder)
/*
*/
