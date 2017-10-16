(load "pattern-matcher.scm")
(load "pc.scm")
(load "output_functions.scm")
(load "remww.scm")
(load "pc.scm")

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

      
#;(define <stop_taking>
  (new (*parser (char #\space))
       (*parser (char #\newline))
       (*parser (char #\;))
       (*parser (word "#;"))
        done))


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

#;(define <StringChar> ; removed skipped
  (new (*parser <StringLiteralChar>) ;case sensative
       (*parser <StringMetaChar>)
       (*parser <StringHexChar>)
       (*disj 3)
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
    (newline) ;819
    (display (caar lst))
    (newline) ;821
    (display (cdar lst))
    (if (null? (cdr lst))
        *void-object*
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
      )))

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

;;;;;;;;;;;;;;;;;;;;;;end ass2;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define lambda? (lambda(exp) (and (list? exp) (not (null? exp)) (or (lambda-simple? exp) (lambda-opt? exp) (lambda-var? exp)))))

(define lambda-simple?
        (lambda(exp)
         (eq? (car exp) 'lambda-simple)))
(define lambda-opt?
        (lambda(exp)
         (eq? (car exp) 'lambda-opt)))
(define lambda-var?
        (lambda(exp)
         (eq? (car exp) 'lambda-var)))
         
(define get-params
    (lambda(exp)
        (cond ((lambda-simple? exp)  (cadr exp))
               ((lambda-var? exp) (list (cadr exp)))
               ((lambda-opt? exp) (append (cadr exp) (list (caddr exp)))))))
               
(define get-body
    (lambda(exp)
        (cond ((lambda-simple? exp)  (cddr exp))
               ((lambda-var? exp) (cddr exp))
               ((lambda-opt? exp) (cdddr exp)))))
               
(define get-seq
    (lambda(seq)
      (if (eq? (caar seq) 'seq)
        (cadar seq)
        seq
        )
      ))
        

(define replace-body
        (lambda (exp body)
          (let ((new-body (if (> (length body) 1) `(seq ,body) (car body))))
            (if (or (lambda-simple? exp) (lambda-var? exp)) 
                `(,(car exp) ,(cadr exp) ,new-body)
                `( ,(car exp) ,(cadr exp) ,(caddr exp) ,new-body)) )))

(define eliminate-nested-defines 
    (lambda (e)
    (cond ((lambda? e ) (foo (get-body e) (lambda(dips es) 
          (if (null? dips) (replace-body e (map eliminate-nested-defines (get-body e)))
          (let ((vars (map cadadr dips)))
            (let ((sets (map (lambda(d) `(set ,(cadr d) ,(eliminate-nested-defines (caddr d)) )) dips)))
              (let* ((es2 (map eliminate-nested-defines es))
                    (func (replace-body `(lambda-simple ,vars a) (append sets es2))))
          
                  (replace-body e `((applic ,func ,(map (lambda(x) `(const ,#f)) dips))))
                               ))
          )))))
          ( (list? e) (map eliminate-nested-defines e))
          (else e)
          )
        ))

        
(define foo
    (lambda (pes ret-ds+es)
        (if (null? pes)
            (ret-ds+es '() '())
            (foo (cdr pes)
                (lambda(ds es)
                    (cond ((eq? (caar pes) 'def)
                            (ret-ds+es (cons (car pes) ds) es))
                            ((eq? (caar pes) 'seq)
                            (foo (cadar pes)
                              (lambda(ds1 es1)
                              (ret-ds+es (append ds1 ds)
                                        (append es1 es)))))
                            (else (ret-ds+es ds (cons (car pes) es)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define orderMap
  (lambda (func lst)
    (if (null? lst)
        lst
        (let* ((first (func (car lst)))
               (rest (orderMap func (cdr lst))))
          (cons first rest)))))

(define isVar?
  (lambda (exp) (equal? 'var (car exp)))
)

(define checkLex
  (lambda (exp argsList) 
    (letrec ((minor 0) (major -1) (exists #f) (arg (cadr exp))
            (loop (lambda (argsList2)
               (if (null? argsList2) `(fvar ,arg)
               (if (null? (car argsList2)) (begin (set! major (+ major 1)) 
               (loop (cdr argsList2))) 
                (orderMap (lambda (exp) 
                
                  (if (equal? exp arg) (set! exists #t)
                  (if (not exists) (set! minor (+ minor 1))))) (car argsList2))))
               (if (and exists (= major -1)) `(pvar ,arg ,minor)  
                    (if exists `(bvar ,arg ,major ,minor ) 
                    (begin (set! major (+ major 1)) 
                           (set! minor 0)
                         (if (null? argsList2) `(fvar ,arg) (loop (cdr argsList2)))))) 
            ))
    ) (loop argsList))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;pe->lex-pe;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        
(define lst-index
  (lambda (e lst)
    (letrec ((helper (lambda(lst n)
                       (if (null? lst)
                           -1
                            (let ((first (car lst))
                                  (rest (cdr lst)))
                              (if (eq?  e first)
                                  n
                                  (helper rest (+ n 1))))))))
      (helper lst 0))))

   
(define find-pe (lambda (v lst)
                  (letrec ((helper (lambda(lst n)
                                     (if (null? lst)
                                         `(fvar ,v)
                                         (let ((index (lst-index v (car lst))))
                                           (if (= index -1)
                                               (helper (cdr lst) (+ n 1))
                                               (let ((test 1) (test2 2))
                                                 (if (= n -1)
                                                     `(pvar ,v ,index)
                                                     `(bvar ,v ,n ,index)))))))))
                    (helper lst -1))))


(define applic? (lambda(x)
    (and (list? x) (> (length x) 1) (eq?  'applic (car x)))))

(define pe->lex-pe
  (letrec ((helper (lambda (exp lst) 
                     (if (lambda? exp)
                         (let ((params (get-params exp))
                               (body (get-body exp)))
                           (replace-body exp (helper body (cons params lst))))
                         (let ((t_str "abc") (test123 123))
                           (if (applic? exp)
                               `(applic ,(helper (cadr exp) lst) ,(map (lambda(x) (helper x lst) ) (caddr exp)))
                               (if (and (not (null? exp)) (list? exp))
                                   (if (eq?  (car exp) 'var)
                                       (find-pe (cadr exp) lst)
                                       (let ((ans (map (lambda(x) (helper x lst)) exp)))
                                         ans))
                                   exp)))))))
           (lambda (sexpr)
      (helper sexpr '() ))     
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;checkers;;;;;
(define if3? (lambda(x)
    (and (list? x) (eq? (car x) 'if3))))

(define or? (lambda(x)
    (and (list? x) (eq? (car x) 'or))))

(define def? (lambda(x)
    (and (list? x) (eq? (car x) 'def))))
;dsfsdfsdfsdfsdf
(define tc-applic? (lambda(x)
    (and (list? x) (> (length x) 1) (eq? (car x) 'tc-applic))))
;;;;;343423423423423
(define box-set? (lambda(x)
    (and (list? x) (eq? (car x) 'box-set))))

(define box-get? (lambda(x)
    (and (list? x) (eq? (car x) 'box-get))))

(define box? (lambda(x)
    (and (list? x) (eq? (car x) 'box))))
;(let ((x 1) (y 2))
(define get-tail-list
    (lambda(l)
        (list-ref l (- (length l) 1))))

(define seq? (lambda(x)
    (and (list? x) (eq? (car x) 'seq))))
;;;;;;;;;;;end checkers;;;;;;;;;;;;;;;
;(let ((x 1) (y 2))
(define get-1st-list
    (lambda(lst)
    (reverse (cdr (reverse lst)))))

(define annotate-tc
  (letrec ((annotate-tc-helper 
            (lambda(exp tp?)
    (cond ((and (list? exp) (or (eq? (car exp) 'bvar) (eq? (car exp) 'const) (eq? (car exp) 'pvar) (eq? (car exp) 'fvar)  (eq? (car exp) 'var) )) exp)
          ( (or? exp) (let* ((tsts (cadr exp))
                             (t (annotate-tc-helper (get-tail-list tsts) tp?))
                             (h (map (lambda(el) (annotate-tc-helper el #f)) (get-1st-list tsts))))
                        `(or ( ,@h ,t))))
          ( (if3? exp) `(if3 ,(annotate-tc-helper (cadr exp) #f) ,(annotate-tc-helper (caddr exp) tp?) ,(annotate-tc-helper (cadddr exp) tp?) ) )
          ( (def? exp) `(def ,(cadr exp) ,(annotate-tc-helper (caddr exp) #f)))
          ( (lambda? exp) 
            (let* ((body (get-seq (get-body exp)))
                   (t (annotate-tc-helper (get-tail-list body) #t))
                   (h (map (lambda(el) (annotate-tc-helper el #f)) (get-1st-list body))))
                (replace-body exp (append h (list t)) ))
            )
          ( (applic? exp) `(,(if tp? 'tc-applic 'applic) ,(annotate-tc-helper (cadr exp) #f) ,(map (lambda(el) (annotate-tc-helper el #f)) (caddr exp))))
          ( (box-set? exp) `(box-set ,(cadr exp) ,(annotate-tc-helper (caddr exp) #f)))
          ( (check-type? exp '(set)) `(set ,(set-var exp) ,(annotate-tc-helper (set-exp exp) #f)))
          ( (seq? exp) 
            (let* ((body (cadr  exp))
                   (t (annotate-tc-helper (get-tail-list body) tp?))
                   (h (map (lambda(el) (annotate-tc-helper el #f)) (get-1st-list body))))
              `(seq (,@h ,t))))
          ;( (list? exp) (map (lambda(x) (annotate-tc-helper x tp?)) exp))
          (else
           (let ((x 1) (y 2))
             exp))
          )  
      )))
    (lambda(exp)
      (annotate-tc-helper exp #f))))


;;;;;;;;;empty lambda;;;;;;;;;;;;;;;

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
    (runCheckEmptyLambda exp)))


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
;examples: (check-type? exp '(set seq def))
(define check-type? 
  (lambda (exp type)
    ;(print_all(list (cons "checktype strtd with " exp) (cons "type to check for" type)))
    (let ((first (car exp)))
      (member? first type))
    ))

    
     #|
     This is an extended comment.
     Such comments are useful for commenting out code fragments.
     |#

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
                     `(,command ,@(map box-run all_exps))))) ;check if we need to run this for pred (i think we do)
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
                ((check-type? exp '(if3)) `(if3 ,@(map (lambda(el) (box-t-mf-pvar el var )) (cdr exp)))) ;check pred seq alt
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
    ;(display "entered box-lambda") (\n)
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
             (cons (box-run first_exp) (box-run rest_exps))))
          )
    ))

;runner
(define box-set
  (lambda(exp)
    (let ((ans (box-run exp)))
      ans)
    ))                         

(define op
  (lambda(exp) (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines exp)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Project;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define run-ass3
  (lambda (pe)
    (annotate-tc 
    (pe->lex-pe
     (box-set (remove-applic-lambda-nil (eliminate-nested-defines
                         (parse pe))))))))


;;;;getters;;;;;;;;
(define const-exp->value cadr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Tables;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;constant table ;;;;;;;;;
(define *ctbl-start-location* 1000) ;start location for constants
(define *init-ctbl* '()) ;initial value of constant table
(define make-ctbl cons) ;(lambda (tbl-size tbl) (cons tbl-size tbl))) == cons ; constant table maker
(define constant-tbl (box (make-ctbl *ctbl-start-location* *init-ctbl*))) ;the constant table
(define get-ctbl (lambda() (unbox constant-tbl)))
;getters ctbl
(define get-ctbl-size car);(lambda (ctbl) (car (unbox ctbl))))
(define get-ctbl-entries cdr);(lambda (ctbl) (cdr (unbox ctbl)))) 
(define get-ctbl-end-address (lambda () (get-ctbl-size (get-ctbl))))
;ctbl entries
(define make-constant-tbl-entry
  (lambda (address constant memory-rep)
    (list address constant memory-rep)))
;getters for entries
;#define SOB_VOID 1
;#define SOB_NIL 2
(define ctbl-entry-get-address  (lambda(entry)
                                  (if (null? entry)
                                      2
                                      (if(void? entry)
                                         1
                                         (car entry)))))
(define ctbl-entry-get-constant  cadr)
(define ctbl-entry-get-memory-rep  caddr)

(define ctbl-entry-get-string-rep  (lambda(entry) (cadr (ctbl-entry-get-memory-rep entry))))


(define ctbl-lookup
  (lambda (ctbl-entries constant)
    ;(print_all (list (cons "ctbl lookup, ctbl-lookup searching for : " constant)))
    (if (null? ctbl-entries)
        #f
        ;(let ((entries (get-ctbl-entries ctbl-entries)))
        ;(print_all (list (cons "ctbl not null, ctbl-lookup searching for : " constant) (cons "and entries" ctbl-entries)))
        (if (null? ctbl-entries)
              #f
              (ormap (lambda (entry) (if (equal? constant (ctbl-entry-get-constant entry)) entry #f))
                     ctbl-entries)
              ))));)

(define ctbl-base-cases-addresses
  (lambda(const)
    (begin ;(display "arrived at chack base cases") (newline)
    (cond ((void? const) 1) ;1=void address
          ((null? const) 2) ;2=null address
          ((equal? #t const) 3) ;3=true address
          ((equal? #f const) 5) ;5=false address
          (else #f)
          ))))


(define ctbl-lookup-address
  (lambda (constant)
    (let ((ans-ctbl-base-cases (ctbl-base-cases-addresses constant)))
      ;(print_all (list (cons "entered ctbl-lookup-address with " constant) (cons "base cases answer is " ans-ctbl-base-cases)))
      (if ans-ctbl-base-cases
          ans-ctbl-base-cases
          (let ((entries (get-ctbl-entries (get-ctbl))))
            (letrec ((runner
                      (lambda (entries)
                        (if (null? entries) ;this should never happen!! delete at production
                            "error: ctbl-lookup-address: constant doesn't exist in code. You should not be here. code-gen error"
                            (let ((first (car entries))
                                  (rest (cdr entries)))
                              (if (equal?  (ctbl-entry-get-constant first) constant)
                                  (ctbl-entry-get-address first)
                                  (runner rest))
                              )))
                      ))
              (runner entries))              
            )))))

;(display 'print-const-table_not_inplemented))
;adds to constant table.
;[constant_value*entry_value_field -> ctbl_entry]
(define constant-adder
  (lambda (const data)
   ; (print_all (list (cons "strtd constant-adder wth: " const)))
    (let* ((ctbl (get-ctbl))
           (curr_table (get-ctbl-entries ctbl))
           (size-ctbl (get-ctbl-size ctbl))
           (entry-exists (ctbl-lookup curr_table const)))
      ;(print_all (list (cons "entry-exists ans: " entry-exists)))
      (if entry-exists ;if already exist
          (begin  ;(print_all (list (cons "entry-exists already: " const)))
           entry-exists) ;don't add
          (let ((new-entry (make-constant-tbl-entry size-ctbl const data))) ;else - add
            
            (set-box! constant-tbl (make-ctbl (+ (length data) size-ctbl) (cons new-entry curr_table)))
            ;(print_all (list (cons "after set box " new-entry)))
            new-entry)))))


;recurssive, adds all constants to table
(define add-constant-to-table
  (lambda (const-value)
    ;(print_all (list (cons "strtd add-constant-to-table wth: " const-value)))
    (if (or (void? const-value)  (null? const-value))
        const-value
        ;(if (void? const) 
         ;   (make-constant-tbl-entry "SOB_VOID" const-value `("T_VOID"))
        (begin ;(print_all (list (cons "not void or null: " const-value)))
          (cond 
          ((integer? const-value) (constant-adder const-value `("T_INTEGER" ,const-value)))
          ((rational? const-value) (constant-adder const-value `("T_FRAC" ,(number->string (numerator const-value)) ,(number->string (denominator const-value)))))
          ((char? const-value) (constant-adder const-value `("T_CHAR" ,(char->integer const-value))))
          ((string? const-value) (constant-adder const-value
                                                 `("T_STRING" ,(string-length const-value)
                                                              ,@(map char->integer (string->list const-value)))))
          ((pair? const-value)
           (let ((first_el (add-constant-to-table (car const-value)))
                 (second_el (add-constant-to-table (cdr const-value))))
             (constant-adder const-value `("T_PAIR" ,(ctbl-entry-get-address first_el) ,(ctbl-entry-get-address second_el)))))
          ((symbol? const-value) 
           (let*((str-rep-entry (add-constant-to-table (symbol->string const-value)))) ;first add string to represent symbol name
             ;(new-symbol-entry
                     (constant-adder const-value `("T_SYMBOL" ,(ctbl-entry-get-address str-rep-entry))))) ; Add to constant table
          
          ((vector? const-value)  (let ((vec-len (vector-length const-value))
                                        (vec-list (vector->list const-value)))
                                    (constant-adder const-value `("T_VECTOR" ,vec-len ,@(map ctbl-entry-get-address (map add-constant-to-table vec-list))))
                                    ))
          ((boolean? const-value) (if const-value
                                      (make-constant-tbl-entry "SOB_TRUE" #t `("T_BOOL" 1))
                                      (make-constant-tbl-entry "SOB_FALSE" #f `("T_BOOL" 0))))
          (else (error "add-constant-to-table" (format "~s" const-value))))
          ))));)


;     ctbl-entry-get-memory-rep
(define define-const-table-cisc
  (lambda ()
    (let* ((entries (get-ctbl-entries(get-ctbl)))
           (all-data (apply append (reverse (map ctbl-entry-get-memory-rep entries))))
           (data-string (map (lambda (value) (string-append (to-string value) ", ")) all-data)))
      ;(document
      (string-append "long const_table[] = { " (apply string-append data-string) "};") )));)

;To-string.
(define to-string
  (lambda (c)
    (if (string? c)
        c
        (format "~s" c))))

(define load-const-table-cisc
  (lambda ()
              (string-append "memcpy((void*) &ADDR("
               (to-string *ctbl-start-location*)
               "), (void*) const_table, "
               (to-string (- (get-ctbl-size (get-ctbl)) *ctbl-start-location*))
               " * WORD_SIZE);")))

;;;;;;;;;tools;;;;;;;;;;;;;;;;;;;


(define add-to-end-of-lst
  (lambda (lst elem)
    (fold-right cons (list elem) lst)))

(define fix-address-of-before-last
  (lambda (new-address lst)
    (let* ((current-el (car lst))
           (current-el-address (get-symtable-entry-next current-el)))
      (if(= current-el-address 0)
         (let ((symb (car current-el))
               (symb-address (cadr current-el)))
           (list symb symb-address new-address))
         (list (fix-address-of-before-last (cdr lst)))
         ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;fvar table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-counter-from counter)
  (lambda ()
    (set! counter (+ counter 1))
    counter))

(define count (make-counter-from 1))

(define get-fvar-table-start-location (lambda() (get-ctbl-end-address)) )  ;end if constants table
;(define fvar-table (box '())) ;the fvar table
(define fvar-table (box (cons (list 'append 'undefined)
                             ; (cons (list 'bin_append 'undefined)
                                    (cons (list 'list 'undefined)
                                          (cons (list 'map 'undefined) '()) ))));)
(define get-fvar-table (lambda() (unbox fvar-table)))
(define fvar->name cadr)
(define fvar->address car)
(define ftbl-entry->value caddr)
(define ftbl-entry->name cadr)
(define ftbl-entry->address car)

(define fVar-table-lookup-address
  (lambda (fvar)
    (let((ans-search-runtime-vars (search-runtime-vars fvar)))
      (if ans-search-runtime-vars
          ans-search-runtime-vars
          (let ((unboxed-fvar-table (get-fvar-table)))
            (letrec ((runner
                      (lambda (fvar-entries)
                        (if (null? fvar-entries) 
                            "error: Var-table-lookup-address: variable doesn't exist in code"
                            (let ((first (car fvar-entries))
                                  (rest (cdr fvar-entries)))
                              (if (equal?  (ftbl-entry->name first) fvar)
                                  (ftbl-entry->address first)
                                  (runner rest))
                              )))
                      ))
              (let ((ans (runner unboxed-fvar-table)) )
                ; (print_all (list (cons "fVar-table-lookup-address ans is " ans  ) ))
                ans)
              ))))))

(define add-fvar
  (lambda (var-name)
    (let ((unboxed-fvars (get-fvar-table)))
    ;  (print_all (list (cons "add-fvar, var-name is " var-name)))
      (if (and (not (assoc var-name unboxed-fvars)) (not (assoc var-name (unbox runtime-var-list))))
          (set-box! fvar-table (cons (list var-name 'undefined) unboxed-fvars)))))) ;'undefined 7


(define fvar-table-address-builder
  (lambda(fvar-lst start-address)
    (letrec ((runner 
              (lambda (lst address)
                (if (null? lst)
                    lst
                    ;(let((current_el (car lst)))
                    (cons `(,address ,(caar lst) ,(cadar lst)) 
                          (runner (cdr lst) (+ 1 address)))
                    )
                )))
  ;(print_all (list (cons "fvar-table-address-builder started with" (get-fvar-table)  )))
      (set-box! fvar-table (runner fvar-lst start-address)))
    ;(print_all (list (cons "fvar-table-address-builder finished with" (get-fvar-table)  )))
    ))



(define define-fvar-table
  (lambda ()
    (let* ((fvar-table  (get-fvar-table))
           (all-data (map ftbl-entry->value fvar-table))
           (data-string (map (lambda (value) (string-append (to-string value) ", ")) all-data))
           (fvar-array (string-append "long fvar_table[] = { " (apply string-append data-string) "};")))
      fvar-array )))

;Loads the free variables table to memory
(define load-fvar-table
  (lambda ()
              (string-append
               
               "memcpy((void*) &ADDR("
               (to-string (get-ctbl-end-address))
               "), (void*) fvar_table, "
               (to-string (length (get-fvar-table)))
               " * WORD_SIZE);")
    ))

;Defines the free variables table end.
(define fvar-table-end (lambda () (+ (get-ctbl-end-address) (length (unbox fvar-table)))))

(define build-tables
  (lambda (pe)
    ;(print_all (list (cons "build-tables"  pe )))
    (if (or (null? pe) (not (pair? pe)))
        pe
        (cond 
          ((check-type? pe '(fvar)) (add-fvar (fvar->name pe))) 
          ((check-type? pe '(const)) (add-constant-to-table (const-exp->value pe)))
          (else ;(let ((ans
                       (map build-tables pe);))
                 ; (print_all (list (cons "build-tables ans"  ans )))
                  ;)
                ;(build
          ))
    ;*void-object* 
    )))

(define fold-str
    (lambda(x) (fold-left string-append "" x)))
    

;((const (+ 1)) (applic (fvar cons) ((applic (fvar car) ((const (1 2)))) (const (2 3)))))))


(define compile-scheme-file 
  (lambda (scheme-source cisc-output) ;gets names of files
    (let ((sexprs (string->sexpr (string->list (string-append (file->string "scheme_funcs.scm")
                                                (file->string scheme-source))))))
      (let ((ready_sexprs (map run-ass3 sexprs))) ; parsed + ass3
        (begin
          (build-tables ready_sexprs)
           ;(print_all (list  (cons "fvartable" (get-fvar-table)) (cons "ctbl" (get-ctbl))))
          (let ((prolog (get-prolog))
                     (after-prolog (get-after-prolog))
                     (epilog (get-epilog)))
                 (let ((gen-exprs
                        (fold-str (map (lambda(exp)
                                         (let* ((index (count))
                                                (void-label (string-append "L_void_end_" (to-string index)   )))
                                           (begin
                                           (string-append (code-gen exp '() '() 0) "CMP(R0,SOB_VOID);\n"
                                                          "JUMP_EQ("void-label");\n"
                                                          "PUSH(R0);\n"
                                                          "CALL(WRITE_SOB);\n"
                                                          "DROP(1);\n"
                                                          "PUSH(IMM('\\n'));\n"
                                                          "CALL(PUTCHAR);\n"
                                                          "DROP(1);\n"
                                                          void-label":\n")))) ready_sexprs))))
                   
                   (write-file (string-append prolog after-prolog " \n " " \n " "// code-gen " " \n " " \n "
                                              gen-exprs
                                              " \n " "//end of code-gen" " \n "
                                              epilog
                                              " \n " ) cisc-output)
                   )))
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;Code-gen;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define set? (lambda(x)
    (and (list? x) (> (length x) 1) (eq? (car x) 'set))))

(define set-var cadr)
(define set-exp caddr)



(define code-gen-box
    (lambda(e env params depth)
        (string-append
        "PUSH(1);\n"
        "CALL(MALLOC);\n"
        "DROP(1);\n"
        "MOV(IND(R0),FPARG(" (to-string (+ 2 (caddr (cadr e)))) "));\n"
        "MOV(FPARG(" (to-string (+ 2 (caddr (cadr e)))) "),R0);\n"
        )))
        
(define code-gen-box-get

    (lambda(e env params depth)
        (string-append (code-gen (cadr e) env params depth)
        "MOV(R0,IND(R0));\n"
        )
        ))  

(define code-gen-box-set

    (lambda(e env params depth)
        (string-append (code-gen (caddr e) env params depth)
        "MOV(R1,R0);\n"
        (code-gen (cadr e) env params depth)
        "MOV(IND(R0),R1);\n"
        "MOV(R0,SOB_VOID);\n"
        )
        )) 


;env extension
(define envExt
  (lambda (oldEnv params depth bodyLabel exitLabel)
    (let ((major (length oldEnv))
        (count (to-string (count))))       
      (begin
         (string-append "PUSH(IMM (" (to-string major) ")); \n "
         "CALL(MALLOC);\n " ;allocate new 
         "DROP(1);\n "
         "MOV (R2,R0);\n "
         "MOV (R1, FPARG(0)); \n "
         "MOV (R3,IMM(0));\n " ;i
         "MOV(R4,IMM(1));\n " ;j

         "envExt_for_loop_start_"count": \n "
         "CMP(R3,IMM(" (to-string major) "));\n "
         "JUMP_GE(envExt_for_loop_end_" count ");\n "
         "MOV(INDD(R2,R4),(INDD(R1,R3)));\n "
         "INCR(R3);\n "
         "INCR(R4);\n "
         "JUMP(envExt_for_loop_start_"count");\n "
         "envExt_for_loop_end_"count": \n "

         ;"PUSH (IMM (FPARG(1)));\n "
           "PUSH (FPARG(1));\n "
         "CALL(MALLOC);\n "
         "DROP(1);\n "
         "MOV(INDD(R2,0), R0);\n " ;R2[0]<-malloc(fparg(1))

         "MOV (R5,INDD (R2,0));\n "
         "MOV (R3,IMM(0));\n " ;i
         "MOV(R4,IMM(2));\n " ;j

         "envExt_loop_copy_param_start_"count":\n "
         "CMP(R3,FPARG(1));\n "
         "JUMP_GE(envExt_loop_copy_param_end_"count");\n "
         "MOV (INDD(R5,R3),FPARG(R4));\n "
         "INCR(R3);\n "
         "INCR(R4);\n "
         "envExt_loop_copy_param_end_"count":\n "
         "PUSH(IMM(3));\n "
         "CALL(MALLOC);\n "
         "DROP(1);\n "
         "MOV(INDD(R0, 0), T_CLOSURE);\n "
         "MOV(INDD(R0, 1), R2);\n "
         "MOV (INDD(R0,2),LABEL(" bodyLabel ")); \n " ;append
         "JUMP(" exitLabel ") \n ")
          ;add label for the procedure
      )
    )))


;cgen if expression
(define code-gen-if3
  (lambda (e env params depth)
    (let ((test (cadr e))
       (dit (caddr e))
       (dif (cadddr e))
       (count (count))
       (nextDepth (+ 1 depth)))
    (begin
        (string-append
          (code-gen test env params nextDepth)
         
           "CMP(R0,IMM(SOB_FALSE));\n "
           "JUMP_EQ(L_if3_else_" (to-string count) ");\n "
          (code-gen dit env params nextDepth)
           "JUMP(L_if3_exit_"(to-string count)");\n "
           "L_if3_else_"(to-string count)":\n "
           
          (code-gen dif env params nextDepth) ";\n"
          ;"CMP(R0,IMM(SOB_VOID));\n "
          ;"JUMP_EQ(L_return_void_" (to-string count) "); \n"
          ; "L_return_void_"(to-string count)":\n "

           
           "L_if3_exit_"(to-string count)": \n "))
      
    )
  ))

;cgen seq expression
(define code-gen-seq
  (lambda (e env params depth)
      (letrec ((loop (lambda (seq app-string)
          (if (null? seq) app-string
            (loop (cdr seq) (string-append app-string (code-gen (car seq) env params depth)))))))
    (loop (cadr e) " "))

  ))

;last or-exp
(define last-or-exp
  (lambda(or-exp)
    (if (null? or-exp) 
      or-exp
      (let ((rev (reverse or-exp)))
      (list (reverse (cdr rev)) (car rev)))
  )))

(define genlistOr
  (lambda (listr count env params nextDepth)
      (letrec ((loop (lambda (argsList app-string)
      (if (null? argsList)   app-string
      (loop (cdr argsList) 
        (string-append app-string 
          (code-gen (car argsList) env params nextDepth) 
           "CMP (R0,IMM(SOB_FALSE));\n "
           "JUMP_NE(L_or_exit_"(to-string count)");\n " ))))))
    (loop listr ""))))

(define code-gen-or
  (lambda (e env params depth) 
    (let ((or-without-last (reverse (cdr (reverse (cadr e)))))
       (or-last-exp (car (reverse (cadr e))))
       (count (count))
       (nextDepth (+ depth 1)))
    (begin
      ;(display "\n\n\n\n") (display or-without-last) (display "\n\n\n")
       ;(display "\n\n\n\n") (display or-last-exp) (display "\n\n\n")

      (string-append
        (genlistOr or-without-last count env params nextDepth)
        (code-gen or-last-exp env params nextDepth)
       "L_or_exit_"(to-string count)":\n "))
  )))

;cgen pvar
(define code-gen-pvar
  (lambda(e env params depth) 
    (let ((minorString (to-string(caddr e))))
      (begin
         (string-append "MOV(R0,FPARG (2+" minorString "));\n ")))
    )
  )


(define code-gen-fvar
  (lambda(e env params depth) 
    (let* ((free-var (cadr e))
          (addrString (to-string (fVar-table-lookup-address free-var))))
      (let ((fvar_from_runtime (member? free-var (map car (unbox runtime-var-list)))))
        ;(print_all (list (cons "free-var is:" free-var) (cons "(map car (unbox runtime-var-list)):" (map car (unbox runtime-var-list)))))
              (if fvar_from_runtime
                  (string-append  "MOV (R1,IMM(" addrString ")); \n "
                                  "MOV (R0,IMM(R1));\n ") ;was IND
                  (string-append  "MOV (R1,IND(" addrString ")); \n "
                                  "MOV (R0,IMM(R1));\n ") ;was IND
  )))))



;cgen bvar
(define code-gen-bvar
  (lambda (e env params depth)
    ;(display "\n\n")
    ;(display "imHERE")
    ;(display "\n\n")
    (let ((minorString (to-string (cadddr e)))
         (majorString (to-string (caddr e))))
      (begin 
        (string-append
         "MOV (R0,FPARG(0));\n "
         "MOV (R0,INDD(R0," majorString ")); \n "
         "MOV (R0,INDD(R0," minorString ")); \n "))
    )))

;cgen constant
(define code-gen-const
  (lambda(e env params depth)
    (let* ((const (cadr e))
          (addrString (to-string (ctbl-lookup-address const))))
    
      (if (void? const) "IMM(SOB_VOID)"
      (string-append
       ;"MOV(R1,IMM(" addrString "));\n "
       ;"MOV(R0,IND(R1));\n "))
       "MOV(R0,IMM(" addrString ")); \n "
        ))


  )))


(define code-gen-set
  (lambda(e env params depth)
    (let ((var (cadr e))
          (val (caddr e)))
        (string-append
          "//start cgen-set \n"
          (code-gen val env params depth)
          "\n"
          (if (eq? (car var) 'pvar)
              (string-append "MOV (FPARG(2+" (to-string (caddr var))"),R0);\n ")
              (if (eq? (car var) 'fvar)
                (string-append "MOV (IND(" (to-string (fVar-table-lookup-address (cadr get-var))) ",R0)\n;" )
                (if (eq? (car var) 'bvar)
                      (string-append  "MOV(R1,FPARG(0));\n"
                                      "MOV(R2,INDD(R1,"(to-string (caddr var)) "));\n"
                                      "MOV(INDD(R2,"(to-string (cadddr var))"),R0);\n")
                      (string-append  "JUMP (Before_Ending);\n") ;error, should not happen
                    )
                )
          )
          "MOV (R0,IMM(SOB_VOID));\n "
          "//end cgen-set \n"
          ))
          ))

;cgen box-get expression
(define code-gen-boxGet
  (lambda (e env params depth) 
    (let ((minor (caddr e)))
      (begin
         (string-append "MOV(R0,FPARG(2+" (to-string minor) "));\n "
         "MOV(R0,IND(R0));\n ")))
))



(define genlist
  (lambda (listr env params nextDepth)
    (letrec ((loop (lambda (argsList app-string)
      (if (null? argsList)
          app-string
          (loop (cdr argsList) 
                (string-append (code-gen (car argsList) env params nextDepth) 
         "PUSH (R0);\n " app-string))))))
      (loop listr ""))
    
    ))


(define code-gen-applic
  (lambda (e env params depth)
    (let* ((args (caddr e))
         (proc (cadr e))
         (m (length args))
         (nextDepth (+ depth 1)))
    (begin
      (string-append
        "//start-applic \n"
         (genlist  args env params depth)
          "PUSH (IMM(" (to-string m) "));\n "
          (code-gen proc env params nextDepth)
          "PUSH (INDD(R0,1));\n "
         "CALLA (INDD(R0,2));\n "
         "DROP(1);\n " ;take environment out
         "POP(R1);\n " ;pop m and put inside R1
         "DROP(R1);\n "
      "//end-applic\n") ;take all the arguments out of the stack
    )
  )))


;;cgen-tc-applic;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-gen-tc-applic
    (lambda(e env params depth)

        (let* ((index (count))
            (args (caddr e))
            (proc (cadr e))
            (m (length args))
            (nextDepth (+ 1 depth))
            (loop-label_tc_applic (string-append "tc_applic_loop_"(to-string index)))
              (exit-loop-label_tc_applic (string-append "tc_applic_loop_EXIT_"(to-string index))) 
              (end-label (string-append "tc_applic_END_"(to-string index))))
              
        (string-append 

        (genlist args env params depth)
        "PUSH(IMM(" (to-string m) "));\n"
        (code-gen proc env params nextDepth)
        "PUSH(INDD(R0,1));\n" 
        "PUSH(FPARG(-1));\n" 
        "MOV(R5,FPARG(-2));\n " 
        "MOV(R1,R5);\n" 
        "MOV(R7,FPARG(1));\n"  
        "MOV(R10,R7);\n"
        "ADD(R7,IMM(1));\n"    
        "MOV(R3,0);\n"        
        loop-label_tc_applic":\n"
        "CMP(R3,IMM("(to-string (+ 3  m))"));\n"
        "JUMP_EQ("exit-loop-label_tc_applic");\n"
        "MOV(R4,IMM(-3));\n"
        "SUB(R4,R3);\n"
        "MOV(FPARG(R7),FPARG(R4));\n"
        "INCR(R3);\n"
        "DECR(R7);\n"
        "JUMP("loop-label_tc_applic");\n"
        exit-loop-label_tc_applic":\n"
        "ADD(R10,4);\n"
        "DROP(R10);\n"
         "MOV(FP,R1);\n"
        
        "JUMPA(INDD(R0,2));\n"
        
        ))))


;cgen lambda-simple expression
(define code-gen-lambda-simple
  (lambda (e env params depth)
    (let* ((lambda-vars (cadr e))
         (lambda-body (caddr e))
         (nextDepth (+ 1 depth))
         (count (count))
         (lambdaSimple-body-label (string-append "Lsimple_clos_body_" (to-string count)))
         (lambdaSimple-exit-label (string-append "Lsimple_clos_exit_" (to-string count))))

    (begin
      (string-append
        (envExt env params depth 
          lambdaSimple-body-label lambdaSimple-exit-label)

         "Lsimple_clos_body_" (to-string count) ":\n "
         "PUSH (FP);\n "
         "MOV (FP,SP);\n "
         ;"CMP(FPARG(1),IMM(" (to-string (length params)) "));\n "
         ;"JUMP_NE (L_error_lambda_args_count);\n "
        (code-gen lambda-body (cons env params) lambda-vars nextDepth)
         "POP (FP);\n "
         "RETURN;\n "
         "Lsimple_clos_exit_" (to-string count) ":\n ")

))))


(define code-gen-lambda-opt
  (lambda(e env params depth)
    (let* ((vars (cadr e))
          (lambda-body (cadddr e))
          (fixedVarsNum (length vars)) 
          (optVars (caddr e))
          (count (count))
          (lambdaOpt-body-label (string-append "L_clos_body_"(to-string count)))
          (lambdaOpt-exit-label (string-append "L_clos_exit_"(to-string count)))
          (nextDepth (+ depth 1)))

    (begin
      (string-append
        (envExt env params depth 
          lambdaOpt-body-label lambdaOpt-exit-label)

         "L_clos_body_"(to-string count)":\n "
         "PUSH (FP);\n "
         "MOV (FP,SP);\n "
        ; "CMP(FPARG(1),IMM(" (to-string (length params)) ")); \n ";check arguments number
        ; "JUMP_NE (L_error_lambda_args_count);\n " ;HANDLE ERRORS
         "MOV (R1,IMM(SOB_NIL));\n "
         "MOV(R5,FPARG(1));\n "
         "MOV (R6,IMM(" (to-string fixedVarsNum) ")); \n "
         "CMP (R5,R6); \n "
         "JUMP_EQ (lambda_opt_fixStackNoOptArgs_loop_start);\n "

         "INCR(R6);\n "
         "INCR(R5);\n "
;building the list of optional arguments
         "lambda_opt_loop_start:\n "
         "CMP(R5,R6);\n "
         "JUMP_LE (lambda_opt_loop_end);\n "
         "PUSH (FPARG(R5));\n "
         "PUSH (R1);\n "
         "CALL (MAKE_SOB_PAIR) ;\n "
         "DROP (2);\n "
         "MOV (R1,R0);\n " ; R1 - the list of optional arguments
         "DECR(R5);\n " 
         "JUMP(lambda_opt_loop_start);\n "
         "lambda_opt_loop_end:\n "
         "MOV (R6,IMM(" (to-string fixedVarsNum ) "));\n "
        "MOV(R7,IMM(" (to-string fixedVarsNum) "));\n " ;R7 - number of element to copy
        "ADD(R7,4);\n "
         "MOV(R8,FPARG(1));\n "
         "INCR(R8); \n "
         "MOV(R9,FPARG(1)); \n "
         "SUB(R9,R6);\n "
         "INCR(R9);\n "
         "MOV(FPARG(R8),R1);\n "
          "DECR (R8);\n "

          "MOV (R4,FPARG(1));\n "
          "SUB(R4,R6);\n "
          "DECR(R4);\n "
          "MOV(FPARG(1),R6);\n "
          "INCR(FPARG(1));\n "
        ; R6 - the addr of the last fixed argument

        ; "CMP (R7,FPARG(1));\n "
        ; "JUMP_EQ(lambda_opt_fixStackNoOptArgs_loop_start);\n " ;jump if t r no optional arguments

        

        ;FIXING THE STACK IF THERE ARE OPTIONAL ARGUMENTS

         "lambda_opt_fixStack_loop_start:\n "
         "CMP(R7,IMM(0));\n "
         "JUMP_EQ(lambda_opt_fixStack_loop_end);\n "
         "MOV(FPARG(R8),FPARG(R9));\n "
        "DECR(R8);\n "
        "DECR(R9);\n "
         "DECR(R7);\n "

         "JUMP(lambda_opt_fixStack_loop_start); \n "
         "lambda_opt_fixStack_loop_end:\n "
         "DROP(R4); \n "
         "MOV(FP,SP);\n "
         "JUMP(do_body_"count");\n "

        ;FIX THE STACK IF T R NO OPTIONAL ARGUMENTS
         "lambda_opt_fixStackNoOptArgs_loop_start:\n "
         "PUSH(SOB_NIL);\n "
         "MOV (FPARG(-3),FPARG(-2));\n "
         "MOV(FPARG(-2),FPARG(-1));\n "
         "MOV(FPARG(-1),FPARG(0));\n "
         "MOV(FPARG(0),FPARG(1));\n "
         "MOV(R4,1);\n "
         "MOV(R3,2);\n "
         "loop_update_stack_start:\n "
         "CMP(R5,IMM(0));\n "
         "JUMP_EQ(lambda_opt_loop_noArgs_end);\n "
         "MOV(FPARG(R4),FPARG(R3));\n "
         "DECR(R5);\n "
         "INCR(R4);\n "
         "INCR(R3);\n "
         "JUMP(loop_update_stack_start);\n "
         "lambda_opt_loop_noArgs_end:\n "
         "MOV(FPARG(R4),IMM(SOB_NIL));\n "
         "MOV(FP,SP);\n "
         "MOV(R10,FPARG(1));\n "
        "INCR(R10);\n "
        "MOV(FPARG(1),R10);\n "

         "do_body_"count":\n "
        (code-gen lambda-body (cons env params) vars nextDepth)
         "POP (FP);\n "
         "RETURN;\n "
         "L_clos_exit_" (to-string count)" :\n ")

    ))))


(define code-gen-lambda-var
  (lambda(e env params depth) 
    (let* ((vars (cadr e))
           (lambda-body (caddr e))
           (count (to-string (count)))
           (lambdaVar-body-label (string-append "L_clos_body_lVar" count))
           (lambdaVar-exit-label (string-append "L_clos_exit_lVar" count))
           (nextDepth (+ depth 1)))
      
      (begin
        ;(print_all (list (cons "exp in code-gen " e)))
        (string-append
         (envExt env params nextDepth 
                 lambdaVar-body-label lambdaVar-exit-label)
         
         lambdaVar-body-label":\n "
         "PUSH (FP);\n "
         "MOV (FP,SP);\n "
         "MOV (R1,IMM(SOB_NIL));\n "
         "MOV (R5,FPARG(1));\n "
         "CMP (R5,IMM(0));\n "
         "JUMP_EQ(lambda_var_insert_nil_"count");\n "
         "MOV (R8,FPARG(2));\n"
         "CMP (R8,IMM(SOB_NIL));\n "
         "JUMP_EQ(lambda_var_insert_nil_"count");\n "
         "INCR (R5);\n "
         "MOV (R6,IMM(1));\n "
         "lambda_var_loop_start_"count":\n "
         "CMP (R5,R6);\n "
         "JUMP_EQ (lambda_var_loop_end_"count");\n "
         "PUSH (R1);\n "
         "PUSH (FPARG(R5));\n "
         "CALL (MAKE_SOB_PAIR) ;\n "
         "DROP (2);\n "
         "MOV (R1,R0);\n "`
         "DECR (R5);\n "
         "JUMP (lambda_var_loop_start_"count");\n"
         "lambda_var_loop_end_"count":\n "

         "//**DONE CREATING THE ARGUMENTS LIST**\n"
         
         "MOV (R5,FPARG(1));\n "
         "INCR (R5);\n "
         "MOV(FPARG(R5),R1)"
         "CMP (FPARG(1),IMM(1));\n"
         "JUMP_EQ (do_body_"count");\n" 
         
         "//**FIXING THE STACK**\n"
         "MOV(R6,FPARG(1));\n"
         "DECR(R6);\n"
         "MOV (R5,FPARG(1));\n"
         "MOV(R3,IMM(1));\n "
         "MOV(R2,IMM(4));\n "
         "copy_lambda_var_fixed_stack_start_"count":\n"
         "CMP(R2,IMM(0));\n "
         "JUMP_EQ(copy_lambda_var_fixed_stack_exit_"count");\n"
         "MOV(FPARG(R5),FPARG(R3));\n "
         "DECR(R5);\n "
         "DECR(R3);\n "
         "DECR(R2);\n"
         "JUMP(copy_lambda_var_fixed_stack_start_"count");\n"
         
         "copy_lambda_var_fixed_stack_exit_"count":\n "
         "DROP(R6);\n"
         "MOV(FP,SP);\n "
         "JUMP (do_body_"count");\n"
         "/*THE STACK IS FIXED NOW AND FP EQ SP*/ \n"
         
         "lambda_var_insert_nil_"count":\n"
         "MOV(R3,FPARG(1));\n"
         "MOV(R4,IMM(4));\n"
         "PUSH(FP);\n"
         "MOV(R5,(IMM(-2)));\n"
         "MOV(R6,R5);\n"
         "DECR(R6);\n"
         "no_args_lambda_var_loop_start_"count":\n"
         "CMP(R4,IMM(0));\n"
         "JUMP_EQ(push_nil_"count");\n"
         "MOV(FPARG(R6),FPARG(R5));\n"
         "INCR(R5);\n"
         "INCR(R6);\n"
         "DECR(R4);\n"
         "JUMP (no_args_lambda_var_loop_start_"count");\n"
         "push_nil_"count":\n"
         "MOV(FP,SP);\n"
         "MOV(FPARG(2),IMM(SOB_NIL));\n"
         
        "\n do_body_"count":\n"
        (code-gen lambda-body (cons env params) (cons vars '()) nextDepth)
        "POP (FP);\n "
        "RETURN;\n "
        lambdaVar-exit-label":\n ")
  ))))


;cgen define
(define code-gen-define
  (lambda(e env params depth)
    (let ((definition (caddr e))
          (free-var (cadr e))
          (nextDepth (+ 1 depth)))
      (begin
        (string-append 
         (code-gen definition env params nextDepth)
         "MOV(IND("(to-string (fVar-table-lookup-address (cadr free-var)))"),R0);\n "
         "MOV(R0,IMM(SOB_VOID));\n ")))
    ))


;Code-gen main procedure.
(define code-gen
  (lambda (parsed-exp env params depth)
    (begin
      (string-append
    (cond
      ((null? parsed-exp) "")
      ((check-type? parsed-exp '(seq)) (code-gen-seq parsed-exp env params depth))
      ((check-type? parsed-exp '(if3)) (code-gen-if3 parsed-exp env params depth))
      ((check-type? parsed-exp '(box)) (code-gen-box parsed-exp env params depth))
      ((check-type? parsed-exp '(box-get)) (code-gen-box-get parsed-exp env params depth))
      ((check-type? parsed-exp '(box-set)) (code-gen-box-set parsed-exp env params depth))
      ((check-type? parsed-exp '(or)) (code-gen-or parsed-exp env params depth))
      ((check-type? parsed-exp '(set)) (code-gen-set parsed-exp env params depth))
      ((check-type? parsed-exp '(const)) (code-gen-const parsed-exp env params depth))
      ((check-type? parsed-exp '(fvar)) (code-gen-fvar parsed-exp env params depth)) ;change to fvar
      ((check-type? parsed-exp '(applic)) (code-gen-applic parsed-exp env params depth))
      ((check-type? parsed-exp '(tc-applic)) (code-gen-tc-applic parsed-exp env params depth))
      ((check-type? parsed-exp '(lambda-simple)) (code-gen-lambda-simple parsed-exp env params depth))
      ((check-type? parsed-exp '(lambda-opt)) (code-gen-lambda-opt parsed-exp env params depth))
      ((check-type? parsed-exp '(lambda-var)) (code-gen-lambda-var parsed-exp env params depth))
      ((check-type? parsed-exp '(pvar)) (code-gen-pvar parsed-exp env params depth))
      ((check-type? parsed-exp '(bvar)) (code-gen-bvar parsed-exp env params depth))
      ((check-type? parsed-exp '(def)) (code-gen-define parsed-exp env params depth))
      (else (error "code-gen" (format "~s" parsed-exp))))
    ))))


(define search-runtime-vars
  (lambda(var)
    (let ((runt-var-lst (unbox runtime-var-list)))
      (letrec ((runner
                      (lambda (entries)
                        (if (null? entries) 
                            #f
                            (let ((first (car entries))
                                  (rest (cdr entries)))
                              (if (equal?  (car first) var) ;if we found the var
                                  (cadr first) ;returns label
                                  (runner rest))
                              )))
                      ))
              (runner runt-var-lst)))
      ))


 
(define runtime-var-list
  (box '(
         (apply prim_apply)    ;V
         (< prim_less_than)    ;V 
         (= prim_numeric_equals)        ;V
         (> prim_greater_than) ;V
         (+ prim_plus)         ;V
         (/ prim_division)     ;V
         (* prim_multiple)     ;V
         (- prim_minus)        ;V
         
         (boolean? is_boolean) ;V
         (car prim_car) ;V
         (cdr prim_cdr) ;V
         (char->integer prim_char_to_int) ;V
         (char? prim_is_char) ;V

         (cons prim_cons) ;V

         (eq? prim_is_eq) ;V
         (integer? prim_is_integer) ;V
         
         (integer->char prim_int_to_char) ;V

         (make-string prim_make_string) ;V
         
         (make-vector prim_make_vector) ;V
         
         (null? prim_is_null) ;V
         
         (number? prim_is_number) ;V
         (pair? prim_is_pair) ;V
         (procedure? prim_is_procedure) ;V
         
         (set-car! prim_set_car) ;V
         (set-cdr! prim_set_cdr) ;V
         (string-length prim_string_length) ;V
         (string-ref prim_string_ref) ;V
         (string-set! prim_string_set) ;V
         
         (string->symbol prim_string_to_symbol)
         (string? prim_is_string) ;V
         (symbol? prim_is_symbol) ;V

         (symbol->string prim_symbol_to_string) ;V
         (vector-length prim_vector_length) ;V
         (vector-ref prim_vector_ref) ;V
         (vector-set! prim_vector_set)
         
         (vector? prim_is_vector) ;V
         (zero? prim_is_zero) ;V
         (vector prim_vector) ;V
         (not prim_not) ;V
         (denominator prim_denom) ;V
         (numerator prim_numer) ;V
         
         (rational? prim_is_number) ;V
         (remainder prim_remainder) ;V
         
        ; (list 'und);V  ;map , append
         
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;junk-yard;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
