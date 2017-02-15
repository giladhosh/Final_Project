;(load "pattern-matcher.scm")
;(load "pc.scm")
(load "C:\\Users\\Gilad\\Documents\\BGU\\6th\\Compi\\Ass3\\pattern-matcher.scm")
(load "C:\\Users\\Gilad\\Documents\\BGU\\6th\\Compi\\Ass3\\pc.scm")


;from Mayer's tutorial:
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    (new (*parser (char #\;))
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star
	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;------------------------------------ infix comments---------------------------------------------


(define <eat_until_space>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser (char #\;))
              (*parser (word "#;"))
              (*parser <end-of-input>)
              (*disj 4)
              done)))
    (new 
     (*parser <any-char>)
     (*parser <end-of-line-comment>)
     *diff *star
     done)))

(define <infix-comment>
  (new (*parser (word "#;"))
       ;(*parser <eat_until>)
       ;(*delayed (lambda () <Sexpr>))
       ;(*delayed (lambda () <Initial>))
       ;*diff
              (*delayed (lambda () <Initial>)) 
              (*delayed (lambda () <Number>))
              (*delayed (lambda () <Symbol>))
              (*delayed (lambda () <String>))
             ; (*delayed (lambda () <Char>))
              (*disj 3)
               *diff
       (*delayed (lambda () <Sexpr>))

       ; *diff
       (*disj 2)
       (*caten 2)
       done))

(define <char-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Char>))
       (*caten 2)
       done))

(define <comment_for_infix>
  (disj <char-comment>
        <line-comment>
	<infix-comment>))

(define <skip_for_infix>
  (disj <comment_for_infix>
	<whitespace>))

(define ^<skipped_infix*> (^^<wrapped> (star <skip_for_infix>)))

;------------------------------------ done dealing with whitespace & endline------------------------------------------------------

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <ValidChar>
  (range #\! #\~))

(define <a-f_Char>
  (range-ci #\a #\f))

(define <ci-Char-a-z>
  (range-ci #\a #\z))


(define <Chars-for-SymbolChar>
  (new (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       (*disj 12)
       done
       ))

(define <False>
  (new (*parser (char #\#))
       (*parser (char-ci #\f))
       (*caten 2)
       (*pack (lambda(_) #f))
       done))

(define <True>
  (new (*parser (char #\#))
       (*parser (char-ci #\t))
       (*caten 2)
       (*pack (lambda(_) #t))
       done))
		
(define <Boolean>
  (new (*parser <False>)
       (*parser <True>)
       (*disj 2)
       done))
       
(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
	done))

(define  <VisibleSimpleChar>
  (^<skipped*>(new
   (*parser <ValidChar> )
   done)))

(define  <NamedChar>  
  (new  (*parser (word-ci "lambda"))
	(*pack (lambda (_)  (integer->char 955)))
	(*parser (word-ci "newline"))
       	(*pack (lambda (_)  (integer->char 10)))
       	(*parser (word-ci "nul"))
     	(*pack (lambda (_)  (integer->char 0)))
       	(*parser (word-ci "page"))
       	(*pack (lambda (_) (integer->char 12)))
       	(*parser (word-ci "return"))
      	(*pack (lambda (_) (integer->char 13)))
       	(*parser (word-ci "space"))
      	(*pack (lambda (_) (integer->char 32)))
	(*parser (word-ci "tab"))
	(*pack (lambda (_) (integer->char 9)))
	(*disj 7)
	done ))

(define <HexChar>
  (new (*parser <digit-0-9>)
       (*parser <a-f_Char>)
       (*disj 2)
       (*pack (lambda(ch) (char-downcase ch)))
       done
       ))

(define  <HexUnicodeChar> 
  (new
   (*parser (char-ci #\x))
   (*parser <HexChar>) *plus
   (*caten 2)
   (*pack-with (lambda(x_ch list)          
                 (integer->char
                  (string->number
                   (list->string list) 16))))
   done))

(define <Char>
  (^<skipped*>
   (new (*parser <CharPrefix>)
       (*parser <NamedChar>)
       (*parser <HexUnicodeChar>)
       (*parser <VisibleSimpleChar>) ;case-sensative
       (*disj 3)
       (*caten 2)
       (*pack-with (lambda(chPref ch)
		    ch))
      done)))

(define <string-meta-char-from-Mayer>
  (new (*parser (word "\\\\"))
       (*pack (lambda (_) #\\))
       (*parser (word "\\\""))
       (*pack (lambda (_) #\"))
       (*disj 2)
       done))


(define <StringLiteralChar>
  (new (*parser <string-meta-char-from-Mayer>)
       (*parser <any-char>)
       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)
       *diff
       (*disj 2)
       done))

(define <StringMetaChar>
  (new (*parser (word  "\\\\"))
       (*pack (lambda(_) "\\"))
       (*parser (word "\\\""))
       (*pack (lambda(_) "\""))
       (*parser (word "\\t"))
       (*pack (lambda(_) "\t"))
       (*parser (word "\\f"))
       (*pack (lambda(_) "\f"))
       (*parser (word "\\n"))
       (*pack (lambda(_) "\n"))
       (*parser (word "\\r"))
       (*pack (lambda(_) "\r"))
       (*disj 6)
       done))

	       
(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*caten 3)
       done))


(define <string-meta-char>
  (new (*parser (word "\\\\"))
       (*pack (lambda (_) #\\))

       (*parser (word "\\\""))
       (*pack (lambda (_) #\"))

       (*disj 2)
       done))

(define <StringChar>
  (new
   (*parser <string-meta-char>)
   (*parser <any-char>)
   (*parser (char #\"))
   (*parser (char #\\))
   (*disj 2)
   
   *diff
   (*disj 2)
   done))

(define <String>  ; removed skipped
  (^<skipped*>
   (new
    (*parser <StringHexChar>)
    (*parser (char #\"))
    (*parser <StringChar>) *star
    (*parser (char #\"))
       (*caten 3)
       (*disj 2)
       (*pack-with (lambda(bra1 str bra2)
                     (list->string str)))
       done)))


(define <Natural>
  (new 
       (*parser (char #\0)) *star
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 3)
       (*pack-with
	(lambda (zeros a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))
       
       (*parser (char #\0)) *plus
       ;(*parser <Chars_no_zero>)
       ;*not-followed-by
       (*pack (lambda (_) 0))

       (*disj 2)
       
       done))

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
          (if (zero? num)
              0
              (/ num den))))
       done))

(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       done))

(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser <ci-Char-a-z>)
       (*parser <Chars-for-SymbolChar>)
       (*disj 3)
       (*pack (lambda(ch)
                (char-downcase ch)))
      done))

(define <Symbol>
  (new (*parser <SymbolChar>) *plus
       (*pack (lambda(x) (string->symbol (list->string x))))
      done))


(define <Numbers_Char_Filter>
  (^<skipped*>(new (*parser
        (not-followed-by <Number> <Symbol>))
       done)))


(define <open-Bra>
  (new (*parser (char #\( ))
       (*pack
	(lambda(_) #\( ))
       done))

(define <close-Bra>
  (new (*parser (char #\) ))
       (*pack
	(lambda(_) #\) ))
       done))

(define <ProperList>
  (new
   (*parser <open-Bra> )
   (*delayed (lambda () <Sexpr> )) *star
   (*parser <close-Bra> )
  (*caten 3)
       (*pack-with
	(lambda(a exprs c)
	  exprs))
       done))


(define <ImproperList>
  (new (*parser <open-Bra>)
       (*delayed (lambda () <Sexpr> ))  *plus
       (*parser (char #\. ))
       (*delayed (lambda () <Sexpr> ))
       (*parser <close-Bra>)
       (*caten 5)
       (*pack-with (lambda(a exps c last e)
		     (fold-right cons last exps)))
       done))

(define <Vector>
  (new (*parser (char #\# ))
       (*parser <open-Bra>)
      (*delayed (lambda () <Sexpr> )) *star
       (*parser <close-Bra> )
       (*caten 4)
       (*pack-with
	(lambda(a b sexprs d)
	  (list->vector sexprs)))
       done))

(define <Quoted>
  (new (*parser (char #\' ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with
	(lambda(ch sexp)
	  (list 'quote sexp)))
       done))

(define <QuasiQuoted>
  (^<skipped*>
   (new (*parser (char #\` ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda(ch sexpr)
		     (list 'quasiquote sexpr)))
       done)))

(define <Unquoted>
  (new (*parser (char #\, ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda(ch sexpr)
		     (list 'unquote sexpr)))
	      done))

(define <UnquoteAndSpliced>
  (new (*parser (char #\, ))
       (*parser (char #\@ ))
        (*delayed (lambda () <Sexpr>))
       (*caten 3)
       (*pack-with (lambda(ch1 ch2 sexpr)
			    (list 'unquote-splicing sexpr)))
       done))

(define <Sexpr> 
(^<skipped*>  ;support for comment-line & whitespace 
 (new
  (*delayed (lambda () <InfixExtension>))
  (*parser <Boolean>)
  (*parser <Char>)
  (*parser <Numbers_Char_Filter>)
  (*parser <String>)  
  (*parser <Symbol>)
  (*parser <ProperList>)
      (*parser <ImproperList>)
      (*parser <Vector>)
      (*parser <Quoted>)
      (*parser <QuasiQuoted>)
      (*parser <Unquoted>)
      (*parser <UnquoteAndSpliced>)
     
       (*disj 13)
       done
       )))

(define <InfixPrefixExtensionPrefix>
  (^<skipped*>(new (*parser (char #\#))
       (*parser (char #\#))
       (*caten 2)
       (*parser (char #\#))
       (*parser (char #\%))
       (*caten 2)
       (*disj 2)
       done)))


(define <Infix_Prohibited_SymbolList>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser (char #\*))
       (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2)
       (*parser (char #\^))
       (*parser (char #\/))
       (*disj 6)
       done))

(define <PlusMinusChars>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*disj 2)
       done))

(define <MulDivChars>
  (new (*parser (char #\*))
       (*parser (char #\/))
       (*disj 2)
       done))

(define <PowChars>
  (new (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
       done))

(define <SymbolChar_forInfixSymbol>
  (new (*parser <digit-0-9>)
       (*parser <ci-Char-a-z>)
       (*disj 2)
       (*pack (lambda(ch)
                (char-downcase ch)))
      done))


(define <InfixSymbol>
  (new
   (*parser <SymbolChar_forInfixSymbol>)
   (*parser <Infix_Prohibited_SymbolList>)
   *diff
   *star
   (*pack (lambda(sym) 
            (string->symbol (list->string sym))))
   done))

(define <InfixNeg>
  (new (*parser (char #\-))
       (*delayed (lambda() <Pow_End>))
       (*caten 2)
       (*pack-with
        (lambda(char exp)
                `(- ,exp)))
       done))


(define <InfixParen>
  (new (*parser (char #\( ))
       (*delayed (lambda() <Initial>)) ;*plus
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda(bra1 exp bra2)
                    exp))
       done))

(define <InfixArrayGet>
  (new
  (*delayed (lambda() <Number>))
  (*delayed (lambda() <InfixParen>))
  (*delayed (lambda() <InfixSymbol>))

  (*disj 3)
  (*parser  <skip_for_infix>) *star
  (*parser (char #\[))
  (*delayed (lambda() <Initial>))
  (*parser (char #\]))
  (*parser  <skip_for_infix>) *star
  (*caten 5)
  (*pack-with (lambda (startSpace par1 indexList par2 endSpace) (lambda (vec) (list 'vector-ref vec indexList))))
  *plus
  (*caten 2)
  (*pack-with (lambda (vecName func) (fold-left (lambda (acc elment) (elment acc)) vecName func)))
    done))


(define <InfixFuncall>
  (new 
 
     (*delayed (lambda() <Number>))
     (*delayed (lambda() <InfixParen>))
     (*delayed (lambda() <InfixNeg>))
     (*delayed (lambda() <Sexpr>))
 
  (*disj 4)
       (*parser (char #\())
       
       (*delayed (lambda() <Initial>))
       (*parser (char #\,))
       (*delayed (lambda() <Initial>))
       (*caten 2)
       (*pack-with (lambda (com exp) exp))
       *star
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda(func par1 first args par2)
		      (cond ((null? first) `(,func))
		      ((null? args) `(,func ,first))
		      (else
		      `(,func ,first ,@args)))))
       done))

(define <Pow_End>   ;L3
  (^<skipped_infix*>
   (new
    (*delayed (lambda() <InfixSexprEscape>))
    (*parser  <InfixArrayGet>) ;symbol
    (*parser  <InfixFuncall>)  ;symbol

    (*parser <InfixParen>)
    (*parser <Number>)
    
    (*parser <InfixNeg>)
    (*parser <InfixSymbol>)
    (*parser <epsilon>)

    (*disj 8)
    done)))

(define <MulDiv> ;L2=L3(+L3)*
  (^<skipped_infix*>
   (new
   (*parser <Pow_End>)
   (*parser <PowChars>)
   (*delayed (lambda() <MulDiv>))
   (*parser <Pow_End>)
   (*disj 2)
   (*caten 2)
   (*pack-with (lambda (sign exps)
                 (lambda (first_element)
                   `(expt ,first_element ,exps))))
   *star
   (*caten 2)
   (*pack-with (lambda (first_exp lambda_rest_exps)
                 (fold-left (lambda (acc operator )
                              (operator acc)) first_exp lambda_rest_exps)))
   done)))


(define <AddSub> ;L1=L2(+L2)*
    (^<skipped_infix*>
     (new
      (*parser <MulDiv>)
      (*parser <MulDivChars>)
      (*parser <MulDiv>)
      (*caten 2)
      (*pack-with (lambda (sign exps)
                    (lambda (first_element)
                      `(,(string->symbol (string sign)) ,first_element ,exps))))
      *star
      (*caten 2)
      (*pack-with (lambda (first_exp lambda_rest_exps)
                    (fold-left (lambda (operator acc)
                                 (acc operator)) first_exp lambda_rest_exps)))
      done)))


(define <Initial>  ;L0=L1(+L1)*
  (^<skipped_infix*>
   (new
   (*parser <AddSub>)
   (*parser <PlusMinusChars>)
   (*parser <AddSub>)
   (*caten 2)
        (*pack-with (lambda (sign exps)
                      (lambda (first_element)
                        `(,(string->symbol (string sign)) ,first_element ,exps))))
   *star
   (*caten 2)
   (*pack-with (lambda (first_exp lambda_rest_exps)
                 (fold-left (lambda (operator acc)
                              (acc operator)) first_exp lambda_rest_exps)))
   done)))

(define <InfixExtension>
  (^<skipped_infix*>
   (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <Initial>)
       (*caten 2)
       (*pack-with
        (lambda(pre exp) exp))
       done)))


(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with (lambda(pre exp)
                    exp))
       done))


;Defined by Mayer
(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

; *** QQ from assignment page ***

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const-mayer?
  (let ((simple-sexprs-predicates
	 (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
		 simple-sexprs-predicates)
	  (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
	    (pair? e)
	    (symbol? e)
	    (vector? e))
	`',e
	e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
	(cadr e)
	e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
	 (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
	    (lambda (e)
	      (cond ((unquote? e) (cadr e))
		    ((unquote-splicing? e)
		     (error 'expand-qq
		       "unquote-splicing here makes no sense!"))
		    ((pair? e)
		     (let ((a (car e))
			   (b (cdr e)))
		       (cond ((unquote-splicing? a)
			      `(append ,(cadr a) ,(expand-qq b)))
			     ((unquote-splicing? b)
			      `(cons ,(expand-qq a) ,(cadr b)))
			     (else `(cons ,(expand-qq a) ,(expand-qq b))))))
		    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
		    ((or (null? e) (symbol? e)) `',e)
		    (else e))))
	   (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
	   (optimizer
	    (compose-patterns
	     (pattern-rule
	      `(append ,(? 'e) '())
	      (lambda (e) (optimize-qq-expansion e)))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const-mayer?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
	      (lambda (c1 c2)
		(let ((c (quotify (append (unquotify c1) (unquotify c2)))))
		  c)))
	     (pattern-rule
	      `(append ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  `(append ,e1 ,e2))))
	     (pattern-rule
	      `(cons ,(? 'c1 const-mayer?) (cons ,(? 'c2 const-mayer?) ,(? 'e)))
	      (lambda (c1 c2 e)
		(let ((c (quotify (list (unquotify c1) (unquotify c2))))
		      (e (optimize-qq-expansion e)))
		  (optimize-qq-expansion `(append ,c ,e)))))
	     (pattern-rule
	      `(cons ,(? 'e1) ,(? 'e2))
	      (lambda (e1 e2)
		(let ((e1 (optimize-qq-expansion e1))
		      (e2 (optimize-qq-expansion e2)))
		  (if (and (const-mayer? e1) (const-mayer? e2))
		      (quotify (cons (unquotify e1) (unquotify e2)))
		      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))



;*** end of QQ ***

;helper debugger, prints all pair items
; [List(Pair) -> void]
(define print_all
  (lambda (lst)
    (newline)
    (display (caar lst))
    (newline)
    (display (cdar lst))
    (if (null? (cdr lst))
        (newline)
        (print_all (cdr lst)))))


(define *void-object*  `(const ,(void)))

(define void? (lambda(var) (eq? var (void))))

(define not-resereved-word?
  (lambda (el)
    (not (member el *reserved-words*))))

(define variable?
  (lambda (exp)
    (and (symbol? exp)
         (not-resereved-word? exp))))

(define constant?
  (lambda (exp)
    (or (eq? (void) exp)
        (vector? exp)
        (boolean? exp)
        (char? exp)
        (number? exp)
        (string? exp))))

;; used?
(define notNumber?
	(lambda (exp) (or 
		(not (number? exp))
		(#f))))

;; used?
(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (car s))
			(else `(begin ,@s)))))

;verify lst is a list >= 2
(define assignment-list?
  (lambda (lst)
    (and (list? lst)
         (not (null? lst))
         (not (null? (cdr lst))))))

; Validate list of let assignments
(define check-let-assign?
  (lambda (let-lst)
     (and (not (null? let-lst))
          (list? let-lst)
          (andmap (lambda (assign-lst)
                    (and (assignment-list? assign-lst)
                         (variable? (car assign-lst))
                         (null? (cddr assign-lst))))
                  let-lst))
    ))

; take a let assignment list, check it and return a list of two lists - vars and values(exprs)
(define seperate-vars-vals
  (lambda (lst)
    (if (check-let-assign? lst)
        (list (map car lst) (map cadr lst))
        #f)))

(define identify-lambda
  (lambda (argl ret-simple ret-opt ret-var)
    (cond 
      ((null? argl) (ret-simple '()))
      ((variable? argl) (ret-var argl))      
      (else (identify-lambda (cdr argl)
          (lambda (s) (ret-simple `(,(car argl) ,@s))) 
          (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
          (lambda (var) (ret-opt `(,(car argl)) var)))))
))

(define and-macro-exp
  (lambda (expr) 
            (if (not (null? (cdr expr)))
             `(if ,(car expr),(and-macro-exp (cdr expr)) #f)
             (car expr))))

(define cond-macro-exp
  (lambda (seq) 
    (if (not (null? (cdr seq))) 
        `(if ,(caar seq) (begin ,@(cdar seq)) ,(cond-macro-exp (cdr seq)))
        (if (eq? (caar seq) 'else) 
            `(begin ,@(cdar seq))
            `(if ,(caar seq) (begin ,@(cdar seq)))))))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define new-beginning
  (lambda (seq)
    (display (map
              (lambda (exp)
                (if (list? exp) 
                    (if (eq? (car exp) 'begin) 
                        (cdr exp)
                        exp)
                    exp))
              seq)))) 

(define letToApplic
  (lambda (listOfPairs body bodies)
    (let ((vars (car (seperate-vars-vals listOfPairs)))
          (vals (cadr (seperate-vars-vals listOfPairs))))
                      `((lambda ,vars ,body ,@bodies) ,@vals)
                      )))

(define let-star-To-Applic
  (lambda (listOfPairs body bodies)
    (let ((vars (car (seperate-vars-vals listOfPairs)))
          (vals (cadr (seperate-vars-vals listOfPairs))))
      (let ((first_var (car vars))
            (first_val (car vals))
            (rest_vars (cdr vars))
            (rest_vals (cdr vals)))
        (let ((first-hasama (list(list first_var first_val)))
              (rest_hasamot (cdr listOfPairs)))
          (if (null? rest_hasamot)
              `(let ,first-hasama ,body ,@bodies)
              (let ((next_ans `(let* ,rest_hasamot ,body ,@bodies)))
                `(let ,first-hasama ,next_ans))
              ))))))

(define letrecToApplic
  (lambda(listOfPairs body bodies)
    (let ((vars (car (seperate-vars-vals listOfPairs)))
          (vals (cadr (seperate-vars-vals listOfPairs))))
      (let ((all_init_sets (map (lambda (var) `(,var #f)) vars))
            (all_set_bodies (map (lambda(var val) `(set! ,var ,val)) vars vals))
            (final_rest_bodies `(let () ,body ,@bodies)))
        (let((begin_final `(,@all_set_bodies ,final_rest_bodies)));)
          (let ((ans `(let ,all_init_sets ,@begin_final )))
            ans
            ))))))

(define check-seq? 
  (lambda (exp) 
    (equal? (car exp) 'seq)))

(define filter_let_bodies
  (lambda (bodies)
    (if(null? bodies)
       '()
       (let ((first (car bodies))
             (rest (cdr bodies)))
         (if (list? first)
             (if (check-seq? first)
                 (let ((no-seq-exp (cadr first)))
                   (let ((first_filtered (filter_let_bodies no-seq-exp)))
                     (fold-right cons  (filter_let_bodies rest) first_filtered) 
                     ))
                 (cons first (filter_let_bodies rest))
                 )
             (cons first (filter_let_bodies rest)); 'not-list-cannot-be-seq
             )
         )
       )
    ))

(define flat-begin
  (lambda(body bodies)
    (let ((all_bodies (cons body bodies)))
      (let ((unfiltered_ans `(seq ,@(map tag-parse all_bodies))))
        (let ((filtered_ans (filter_let_bodies (cdr unfiltered_ans))))
          `(seq ,filtered_ans)
          )
        )
      )
    ))

 
 (define tag-parse
   (let ((run
          (compose-patterns
           ;constants
           (pattern-rule
            (? 'el constant?)
            (lambda(el) `(const ,el)))
           ;quote
           (pattern-rule
            `(quote ,(? 'el)) ;no guards - returns list
            (lambda(el) `(const ,el)))
           ;symbol
           (pattern-rule
           (? 'el variable?)
           (lambda(el) `(var ,el)))
           
           ;if
           (pattern-rule
            `(if
              ,(? 'pred)
              ,(? 'conequence)
              ,(? 'alternative))
            (lambda (pred consequence alternative)
              `(if3 ,(tag-parse pred) ,(tag-parse consequence) ,(tag-parse alternative))))
           ;if-no-consequence
           (pattern-rule
           `(if ,(? 'pred) ,(? 'conseq))
           (lambda(pred conseq) `(if3 ,(tag-parse pred) ,(tag-parse conseq) ,*void-object*)))
           ;empty or
           (pattern-rule
            `(or)
            (lambda() `,(tag-parse #f)))
           ;or 1 element
           (pattern-rule
            `(or ,(? 'el))
            (lambda(el) (tag-parse el)))
           ;or
           (pattern-rule
            `(or ,(? 'first) ,(? 'second) . ,(? 'rest list?)) ;list? checks if there are any more params
            (lambda(first second rest) `(or ,(map tag-parse `(,first ,second . ,rest)))))
           
           ;set!
           (pattern-rule
            `(set! ,(? 'first) ,(? 'rest))
            (lambda (first rest)
              `(set ,(tag-parse first) ,(tag-parse rest))))
           
           ;let*
           (pattern-rule 
            `(let* ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
            (lambda (listOfPairs body bodies)
              (if (null? listOfPairs)
                  (tag-parse `((lambda () ,body ,@bodies))) 
                  (tag-parse (let-star-To-Applic listOfPairs body bodies))
                  )))
           
           ; let
           (pattern-rule 
            `(let ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
            (lambda (listOfPairs body bodies)
              (if (null? listOfPairs)
                  (tag-parse `((lambda () ,body ,@bodies)))
             (tag-parse (letToApplic listOfPairs body bodies)))
              ))
           
           ;letrec 
           (pattern-rule 
            `(letrec ,(? 'listOfPairs) ,(? 'body) . ,(? 'bodies))
            (lambda (listOfPairs body bodies)
             (if (null? listOfPairs)
             (tag-parse `((lambda () ((lambda () ,(beginify (cons body bodies))) ,@listOfPairs)) ,@listOfPairs))
             (tag-parse
              (letrecToApplic listOfPairs body bodies))
             )))

          ;and-macro-exp
       (pattern-rule 
        `(and .,(? 'andExp))
        (lambda (andExp)
          (if (null? andExp)
              (tag-parse '#t)
              (tag-parse(and-macro-exp andExp)))
          ))

          ;cond-macro-exp
       (pattern-rule 
        `(cond .,(? 'condExp))
        (lambda (condExp)
          (tag-parse (cond-macro-exp condExp))
          ))
       ;lambda
       (pattern-rule
        `(lambda ,(? 'argl) ,(? 'body) . ,(? 'rest))
        (lambda (argl body rest)
          (if (null? rest) 
              `(,@(identify-lambda argl (lambda (s) `(lambda-simple ,s)) 
                                   (lambda (s opt) `(lambda-opt ,s,opt)) 
                                   (lambda (var) `(lambda-var ,var)))
                ,(tag-parse body))
                 ;else
              `(,@(identify-lambda argl (lambda (s) `(lambda-simple ,s)) 
                                   (lambda (s opt) `(lambda-opt ,s,opt)) 
                                   (lambda (var) `(lambda-var ,var)))
                ;,(tag-parse `(begin ,body ,@rest))))
                (seq (,(tag-parse body) ,@(map tag-parse rest)))))
          ))
    
 ;Regular\MIT-style define
       (pattern-rule 
        `(define
           ,(? 'first) ,(? 'body) . ,(? 'rest))
        (lambda (first body rest)
          (if (not (variable? first))
              (if (null? rest)
                  `(def ,(tag-parse (car first)) 
                     ,(tag-parse `(lambda ,(cdr first) ,body)))
                  ;else
                  `(def ,(tag-parse (car first)) 
                     ,(tag-parse `(lambda ,(cdr first) ,body ,(car rest))))) 
              ;else
              `(def ,(tag-parse first) ,(tag-parse body) ,@(map tag-parse rest)))
          ))
       
       (pattern-rule 
           `(begin ,(? 'sequance) . ,(? 'rest))
           (lambda (sequance rest)
                 (if (null? rest)
                     (tag-parse sequance)
                     (flat-begin sequance rest)
                 )))
       
       ;empty begin
       (pattern-rule 
        `(begin)
        (lambda()*void-object*))
       
       ;qq
       (pattern-rule 
        `(,'quasiquote ,(? 'arg))
        (lambda (arg) (tag-parse (expand-qq arg))
          ))
          
       ;Application with arguments 
       (pattern-rule
        `( ,(? 'func not-resereved-word?)  . ,(? 'args) )
        (lambda (func args) `(applic ,(tag-parse func) ,(map tag-parse args))))

         )))
    
    (lambda (sexpr)
          (run sexpr (lambda () `(cannot match input  ,sexpr ))))))

(define parse tag-parse)

;-----------------------------------------------Assignment 3----------------------------------------------------------
 

;------------------------------------remove empty application---------------------------------------------
;checks if statment is an empty application
(define check-empty-lambda? 
  (lambda (exp)
    (if (or (not(list? exp)) (list?(car exp)) (not (equal? (car exp) 'applic)) (null?(cdr exp)) (not (list?(cadr exp))) (null? (cadr exp)) (null? (caadr exp)) (not (equal? (caadr exp) 'lambda-simple))  );(null? (caddr exp))) 
        #f
        (let ((op (car exp))
              (first_exp (caadr exp))
              (params? (cadadr exp)))
          (and (equal? op 'applic) (equal? first_exp 'lambda-simple) (equal? (list) params?))
          ))))

(define runCheckEmptyLambda
  (lambda(exp)
    (if(or (null? exp) (not (list? exp)))
       exp
       (if (check-empty-lambda? exp)
           (car(runCheckEmptyLambda (cddadr exp))) ;remove on bodies of lambda
           (let ((first (car exp))
                 (rest (cdr exp)))
             ;(print_all (list (cons "first" first) (cons "rest" rest)))
             (if (list? first)
                  (let((first_no_empty_lamda (runCheckEmptyLambda first)))
                    ;(print_all (list (cons "first_no_empty_lamda" first_no_empty_lamda)))
                    (cons first_no_empty_lamda (runCheckEmptyLambda rest))
                    )  
                  (cons first (runCheckEmptyLambda rest))))
           ))
    ))

 (define remove-applic-lambda-nil
   (lambda(exp)
     (let ((ans (runCheckEmptyLambda exp)))
       ;(print_all (list (cons "ans" ans)))
       ans)
     ))

 

(define lambda_names '(lambda-simple lambda-var lambda-opt))
 (define var_names '(var)) 

(define member?
  (lambda(el lst)
    (if (or (null? lst) (null? el))
        #f
        (let ((first (car lst))
              (rest (cdr lst)))
          (if (equal? first el)
              #t
              (member? el rest)))))) ;)

;recieves exp and list of types, checks if exp is one of those types
;[exp*List(types) -> Boolean ]
 (define check-type? 
   (lambda (exp type)
         (let ((first (car exp)))
           (member? first type))
 ))

;checks annotate base cases
 (define pass-anno-tc-base-cases?
   (lambda(exp)
     (or (null? exp) (not (list? exp)) 
         (check-type? exp var_names) (check-type? exp '(const seq applic)) )
     ))

 ;receives only lambda bodies, determines which applic is tc
#;(define run-in-lambda ;change this !!! run in *all-forms-that-can-be-tail* (lambda seq if ..)
  (lambda(lamb_bodies) 
    (let ((first (car exp))
          (rest (cdr exp)))
      ;(if(check-type? first '(seq))
         
      ;)
      )))

(define has_next?
  (lambda (lst)
    (let ((current (car lst))
          (next (cdr lst)))
      (null? next))))

;receives bodies of or, cannot be empty, only check last body
(define annotate_or
  (lambda (or_exp)
    (let ((current (car or_exp)))
      ;(display "run annotate or")
      ;(print_all (list (cons "or_exp" or_exp) (cons "current" current)))
      (if (has_next? or_exp)
          (cons current (annotate_or or_exp))
          (annotate-tc-run or_exp) ; annotate last body
          ))
    ))

;annotate if exp
(define annotate_if
  (lambda (if_exp)
    (let ((pred (cadr if_exp))
          (conseq (caddr if_exp))
          (alt (cdddr if_exp)))
      (let ((ans-to-if `(if3 ,pred ,(annotate-tc-run conseq) ,@(annotate-tc-run alt)))) ;change this!!
       ; (print_all (list (cons "ans-to-if" ans-to-if)))
        ans-to-if
        )
      )
    ))

;annotate def exp
(define annotate_def
  (lambda (def_exp)
    (let ((var (cadr def_exp))
          (definition (caddr def_exp)))
      (let ((ans-to-def `(def ,var ,(annotate-tc-run definition))))
        ;(print_all (list (cons "var" var) (cons "definition" definition) (cons "ans-to-def" ans-to-def)))
        ans-to-def
        )
      )
    ))
     
(define annotate-tc-run
  (lambda(exp)
    (cond ((pass-anno-tc-base-cases? exp) exp)
          ((check-type? exp '(or)) (cons 'or (annotate_or (cadr exp)))) ;or cannot be empty
          ((check-type? exp '(if3)) (annotate_if exp))
          ((check-type? exp '(def)) (annotate_def exp))
          ;((check-type? exp '(applic)) exp)
          (else (let ((first_exp (car exp))
                (rest_exps (cdr exp)))
            ;(print_all (list (cons "first_exp" first_exp) (cons "rest_exps" rest_exps)))
            (if(member? first_exp lambda_names) ;check if first exp of exp is a lambda exp
               (let ((lamb_vars (cadr rest_exps))
                     (lamb_bodies (get-lamb-bodies exp)))
                ; (print_all (list(cons "lamb_vars" lamb_vars) (cons "lamb_bodies" lamb_bodies)))
                 (cons first_exp (cons lamb_vars (run-in-lambda rest_exps)))) ; run on body of lambda , first is of lambda_names
               (cons (annotate-tc-run first_exp) (annotate-tc-run rest_exps))
               )))
          )))

(define annotate-tc
  (lambda(exp)
        (let ((ans (annotate-tc-run exp)))
          (print_all (list (cons "ans" ans)))
          ans
          )
    ))

;--------------------------------------- box-set ---------------------------------------------

(define box-base-cases?
  (lambda(exp)
    (or (null? exp) (not(list? exp)) (check-type? exp '(const)) (check-type? exp var_names))
    ))


(define get-var-of-set
  (lambda(set_exp)
  (let ((ans (cadadr set_exp)))
    ;(print_all (list (cons "get-var-of-set ans" ans)))
    ans
    )
  ))

(define get-seq-or-bodies 
  (lambda(seq_exp)
    ;(print_all (list (cons "get-seq-or-bodies  ans to " seq_exp) (cons "is" (cadr seq_exp))))
   (cadr seq_exp)
    ))

(define get-or-bodies 
  (lambda(or_exp)
    ;(print_all (list (cons "get-seq-or-bodies  ans to " or_exp) (cons "is" (cdr or_exp))))
   (cdr or_exp)
    ))

(define get-lamb-bodies
  (lambda(lamb_exp)
    (if (equal? (car lamb_exp) 'lambda-opt) 
        (cdddr lamb_exp)
        (cddr lamb_exp))))

;checks if exp is a set expression and if the var is the pvar we are looking for
(define check-set-var 
  (lambda (exp pvar)
    ;(print_all (list (cons "entered check-set-var with exp" exp) (cons "and pvar" pvar)))
    (if (or (null? exp) (not (= 3 (length exp))))
          #f
        (if (check-type? exp '(set))
            (equal? (get-var-of-set exp) pvar)
              #f ))))

;(if3 or applic)
(define check-box-inside-if-or 
  (lambda (if_or_exp)
    ;(print_all (list (cons "entered check-box-inside-if-or with " if_or_exp)))
    (let ((command (car if_or_exp))
          (all_exps (cdr if_or_exp)))
     ; (print_all (list (cons "command" command) (cons "all_exps" all_exps)))
      (if (check-type? if_or_exp '(applic))
          (box-run all_exps)
          (let ((ans
                 (if (check-type? if_or_exp '(or))
                     `(,command ,@(map box-run all_exps))
                     `(,command ,(map box-run all_exps))))) ;check if we need to run this for pred (i think we do)
            ;(print_all (list (cons "ans-to-check-box-inside-if-or " ans)))
           ans
          )))
      ))

(define check-box-inside-set-def
  (lambda (def_set_exp)
    (let (;(command (car def_set_exp))
          ;(var (cadr def_set_exp))
          (value (caddr def_set_exp)))
      ;(print_all(list (cons "entered check-box-inside-set-def (from box-run) w/bodies of def/set value" value)))
      (box-run value))))

(define has-set-helper-check-lambda
  (lambda(lamb_exp pvar)
    (let((lamb_type (car lamb_exp))
         (lamb_pvars (cond((equal? (car lamb_exp) 'lambda-var) (list(cadr lamb_exp)))
                          ((equal? (car lamb_exp) 'lambda-opt) (flatten (cons (cadr lamb_exp) (caddr lamb_exp))))
                          (else (cadr lamb_exp))))
         (lamb_bodies (get-lamb-bodies lamb_exp)))
        (if (member? pvar lamb_pvars)
            #f
            (ormap (lambda(body) (has-set? body pvar)) lamb_bodies))
        )))

(define has-bvar-helper-check-lambda
  (lambda(lamb_exp var)
    (let((lamb_type (car lamb_exp))
         (lamb_pvars (cond((equal? (car lamb_exp) 'lambda-var) (list(cadr lamb_exp)))
                          ((equal? (car lamb_exp) 'lambda-opt) (flatten (cons (cadr lamb_exp) (caddr lamb_exp))))
                          (else (cadr lamb_exp))))
         (lamb_bodies (get-lamb-bodies lamb_exp)))
      (if (member? var lamb_pvars)
          #f
          (ormap (lambda(body) (or (has-pvar? body var) 
                                   (if(check-type? body lambda_names)
                                      (has-bvar-helper-check-lambda body var)
                                      (has-bvar? body var))
                                   )) lamb_bodies)
          ))
      ))

(define has-bvar?
  (lambda(exp var)
      ;(print_all (list (cons "entered has-bpvar? with exp " exp) (cons "var" var) ))
      (if(null? exp)
         #f
         (let ((has-bvar-ans 
                (cond 
                  ((check-type? exp '(seq or)) (ormap (lambda(body) (has-bvar? body var)) (get-seq-or-bodies exp))) ;check if seq, check all bodies 
                  ((check-type? exp '(if3)) (ormap (lambda(el) (has-bvar? el var )) (cdr exp))) ;check pred seq alt
                  ((check-type? exp '(applic)) (ormap (lambda(el) (has-bvar? el var )) (cdr exp)))
                  ((check-type? exp '(set)) (ormap (lambda(el) (has-bvar? el var )) (cddr exp)))
                  ((check-type? exp lambda_names) (has-bvar-helper-check-lambda exp var))
                  ((list? (car exp))  ; --> work them one by one 
                   (let ((first-body-ans (has-bvar? (car exp) var ))
                         (ans-of-rest (has-bvar? (cdr exp) var )))
                     (or first-body-ans ans-of-rest)))
                  (else #f)
                  )))
           ;(print_all (list (cons "has-pvar?-ans is:" has-bvar-ans)))
         has-bvar-ans
           )
         )
    ))

;receives body of lambda and var to look for , returns boolean
;def exps do not get here, they stop at box-run
(define has-set?
  (lambda(exp pvar)
    ;(print_all (list (cons "entered has-set? with exp:" exp) (cons "and pvar" pvar)))
    (if(null? exp)
       #f
       (let ((has-set-ans 
              (cond 
                ((check-type? exp '(seq or)) (ormap (lambda(body) (has-set? body pvar)) (get-seq-or-bodies exp))) ;check if seq, check all bodies 
                ((check-type? exp '(if3)) (ormap (lambda(el) (has-set? el pvar)) (cdr exp))) ;check pred seq alt
                ((check-type? exp '(applic)) (ormap (lambda(el) (has-set? el pvar)) (cdr exp)))
                ((check-type? exp lambda_names) (has-set-helper-check-lambda exp pvar))
                ((not (list? (car exp))) (check-set-var exp pvar));if only one exp in body -> work on it , const,var,set,
                (else ; --> work them one by one ;cannot happen? the function that sends here works them 1-by-1
                 (let ((first-body-ans (has-set? (car exp) pvar))
                       (ans-of-rest (has-set? (cdr exp) pvar)))
                   (or first-body-ans ans-of-rest))))))
               ;(print_all (list (cons "has-set-ans is:" has-set-ans)))
         has-set-ans
         )
       )))

(define has-pvar?
  (lambda(exp var)
      ;(print_all (list (cons "entered has-bpvar? with exp " exp) (cons "var" var) ))
      (if(null? exp)
         #f
         (let ((has-pvar-ans 
                (cond 
                  ((check-type? exp '(seq or)) (ormap (lambda(body) (has-pvar? body var)) (get-seq-or-bodies exp))) ;check or, seq, check all bodies 
                  ((check-type? exp '(if3 applic)) (ormap (lambda(el) (has-pvar? el var )) (cdr exp))) ;check bodies
                  ((check-type? exp '(set)) (ormap (lambda(el) (has-pvar? el var )) (cddr exp)))
                  ((check-type? exp lambda_names) #f)
                  ((not (list? (car exp))) (equal? exp (list 'var var)));if only one exp in body -> check it , const,var,
                  (else ; --> work them one by one 
                   (let ((first-body-ans (has-pvar? (car exp) var ))
                         (ans-of-rest (has-pvar? (cdr exp) var )))
                     (or first-body-ans ans-of-rest))))))
           ;(print_all (list (cons "has-pvar?-ans is:" has-pvar-ans)))
         has-pvar-ans
           )
         )
      ))

(define boxit-helper-check-lambda
  (lambda(lamb_exp var)
    (let((lamb_type (car lamb_exp))
         (lamb_pvars (cadr lamb_exp))
         (flattened_lamb_pvars  (cond((equal? (car lamb_exp) 'lambda-var) (list(cadr lamb_exp)))
                                     ((equal? (car lamb_exp) 'lambda-opt) (flatten (cons (cadr lamb_exp) (caddr lamb_exp))))
                                     (else (cadr lamb_exp))))
         (lamb_bodies (get-lamb-bodies lamb_exp)))
      (let((boxit-lambda-ans 
            (if (member? var flattened_lamb_pvars)
                lamb_exp
                (if (equal? lamb_type 'lambda-opt) ;if lamb opt add optional variable to ans
                    `(,lamb_type ,lamb_pvars ,(caddr lamb_exp)  ,@(map (lambda(body) (box-t-mf-pvar body var)) lamb_bodies))
                    `(,lamb_type ,lamb_pvars ,@(map (lambda(body) (box-t-mf-pvar body var)) lamb_bodies)))
                
                )))
        ;(print_all (list (cons "boxit-helper-check-lambda ans is " boxit-lambda-ans)))
        boxit-lambda-ans
      ))))

(define box-t-mf-pvar
  (lambda(exp var)
    ;(print_all (list (cons "entered box-t-mf-pvar with exp " exp) (cons "var" var) ))
      (if(or (null? exp) (not (list? exp)))
         exp
         (let ((ans
                (cond 
                  ((check-type? exp '(box-get)) exp)
                  ((check-type? exp '(box-set)) `(box-set ,(cadr exp) ,(box-t-mf-pvar (caddr exp) var)))
                  ((check-type? exp '(or)) `(or ,@(map (lambda(body) (box-t-mf-pvar body var)) (get-or-bodies exp)))) ;check if seq, check all bodies 
                  ((check-type? exp '(seq)) `(seq ,(map (lambda(body) (box-t-mf-pvar body var)) (get-seq-or-bodies exp)))) ;check if seq, check all bodies 
                  ((check-type? exp '(if3)) `(if3 ,(map (lambda(el) (box-t-mf-pvar el var )) (cdr exp)))) ;check pred seq alt
                  ((check-type? exp '(applic)) `(applic ,@(map (lambda(el) (box-t-mf-pvar el var )) (cdr exp))))
                  ((check-type? exp '(set)) (if (equal? (cadr exp) (list 'var var))
                                                `(box-set (var ,var) ,(caddr exp) )
                                                `(set (var ,(get-var-of-set exp)) ,@(map (lambda(el) (box-t-mf-pvar el var )) (cddr exp)))))
                  ((check-type? exp lambda_names) (boxit-helper-check-lambda exp var))
                  ((and(not (list? (car exp))) (equal? exp (list 'var var))) `(box-get (var ,var)));if only one exp in body -> check it , const,var,
                  (else ; --> work them one by one 
                   (if (symbol? (car exp))
                       exp
                       (let ((first-body-ans (box-t-mf-pvar (car exp) var ))
                             (ans-of-rest (box-t-mf-pvar (cdr exp) var )))
                         (if(null? ans-of-rest)
                            `(,first-body-ans)
                            (cons first-body-ans ans-of-rest))))))))
              ; (print_all (list (cons "box-t-mf-pvar ans is:" ans)))
          ans))
  ))

(define build-set-seq
  (lambda (pvars)
    (map (lambda(pvar) `(set (var ,pvar) (box (var ,pvar)))) pvars)
    ))

;receives lambda expression, if pvars are null runs box-run on all bodies , 
;else check if pvars need to be boxed
(define box-lambda
  (lambda(lamb_exp)
    ;(display "entered box-lambda") (newline)
    (let ((lamb_type (car lamb_exp))
          (lamb_pvars  (cadr lamb_exp))
          (lamb_bodies (get-lamb-bodies lamb_exp)))
       ; (print_all (list (cons "lamb_type" lamb_type) (cons "lamb_pvars" lamb_pvars)(cons "lamb_bodies" lamb_bodies) ))
        (let ((boxed_inner_bodies (box-run lamb_bodies)))
          (if (null? lamb_pvars)
              `(,lamb_type ,lamb_pvars ,@boxed_inner_bodies) ;no pvars, nothing to box. run box-run on all bodies
              ;else pvars not null
              (let ((flattend_pvars 
                     (cond
                     ((equal? lamb_type 'lambda-var) (list(cadr lamb_exp)))
                     ((equal? lamb_type 'lambda-opt) (flatten (cons (cadr lamb_exp) (caddr lamb_exp))))
                     (else lamb_pvars)) ;case of lamb-simple
                    ))
              ;  (print_all (list (cons "flattend_pvars" flattend_pvars)))
                (let ((pvars-to-box ;holds a list of 
                       (filter (lambda(x) (not(equal? x #f))) 
                               (map (lambda (pvar)
                                      (let ((ans_pvar_need_box  (and (has-set? boxed_inner_bodies pvar)  (has-bvar? boxed_inner_bodies pvar ))))
                                     ;   (print_all (list (cons "ans_pvar_need_box of " pvar) (cons "is" ans_pvar_need_box)))
                                        (if ans_pvar_need_box
                                            pvar
                                            #f
                                            )))
                                    flattend_pvars
                                    ))))
                  ;(print_all (list (cons "pvars to box are" pvars-to-box)))
                  (letrec((boxer (lambda(bodies pvars2box)
                                   (if(null? pvars2box)
                                      bodies
                                      (if (list? (car pvars2box))
                                          (boxer bodies (car pvars2box)) ;for opt,var lambdas
                                          (let ((bodies_after_first_pvar_box 
                                                 (box-t-mf-pvar bodies (car pvars2box))))
                                            ;(print_all (list (cons "bodies_after_first_pvar_box" bodies_after_first_pvar_box)))
                                            (boxer bodies_after_first_pvar_box (cdr pvars2box)))
                                          )
                                      ))))
                    (if (null? pvars-to-box);no need to box anything
                        (if (equal? lamb_type 'lambda-opt)
                            `(,lamb_type ,lamb_pvars ,(caddr lamb_exp) ,@boxed_inner_bodies)
                            `(,lamb_type ,lamb_pvars ,@boxed_inner_bodies))
                        (let ((set-seq (build-set-seq pvars-to-box))
                              (final_all_pvars_boxed_bodies (boxer boxed_inner_bodies pvars-to-box)));(boxer lamb_bodies pvars-to-box)))
                          ;(print_all (list(cons "set-seq" set-seq) (cons "final_all_pvars_boxed_bodies" final_all_pvars_boxed_bodies)))
                          (begin (cond ((check-type? final_all_pvars_boxed_bodies '(seq)) ;remove first inner seq
                                        (set! final_all_pvars_boxed_bodies (cdr final_all_pvars_boxed_bodies)))
                                       ((and (null? (cdr final_all_pvars_boxed_bodies)) (check-type? (car final_all_pvars_boxed_bodies) '(seq)))
                                        (set! final_all_pvars_boxed_bodies (cadar final_all_pvars_boxed_bodies))))
                                 (let ((folded_bodies (fold-right cons  final_all_pvars_boxed_bodies set-seq)))
                                   ; (print_all (list (cons "folded_bodies" folded_bodies)))
                                    (if (equal? lamb_type 'lambda-opt) ;if lamb opt add optional variable to ans
                                        `(,lamb_type ,lamb_pvars ,(caddr lamb_exp) (seq ,folded_bodies ))
                                        `(,lamb_type ,lamb_pvars (seq ,folded_bodies )))))))
                        ))
                  ))
              ))))

;run on general expression (that can consist of other expressions)
(define box-run
  (lambda(exp)
    ;(print_all (list (cons "box-run on exp:" exp)))
    (cond ((box-base-cases? exp) exp) ;  base cases: const , all var types , 
          ((check-type? exp '(if3 or)) (check-box-inside-if-or exp)) ;case: if3 ,or
          ((check-type? exp '(applic)) `(applic ,@(check-box-inside-if-or exp)))
          ((check-type? exp '(set def)) `(,(car exp) ,(cadr exp) ,(check-box-inside-set-def exp))) ;case: set,def
          ((check-type? exp lambda_names) (box-lambda exp)) ; lambda case
          ((check-type? exp '(seq)) `(seq ,(map (lambda(body) (box-run body)) (get-seq-or-bodies exp))))
          (else ;cases: mulitple expressions(list of expressions, like bodies of seq)
           (let ((first_exp (car exp))
                 (rest_exps (cdr exp)))
             ;(print_all (list (cons "first_exp" first_exp) (cons "rest_exps" rest_exps)))
             (cons (box-run first_exp) (box-run rest_exps))
             ))
          )
    ))

;runner
 (define box-set
   (lambda(exp)
         (let ((ans (box-run exp)))
           ans)
        ))