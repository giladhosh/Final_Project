; Load pattern matcher
(load "pattern-matcher.scm")

; -----------------------------------------
; Required code from assignment description
; -----------------------------------------
(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)

;;; And just for good luck :-)
(define with (lambda (s f) (apply f s)))

; -----------------------------------------

;;; From: qq.scm
;;; A naive, one-level quasiquote implementation
;;;
;;; Programmer: Mayer Goldberg, 2014

;;; expand-qq assumes that you are passing to it the quasiquoted expression
;;; in other words, the expression without the quasiquote!
;;;
;;; Try:
;;; 
;;; > (expand-qq '(a b c ,d e f ,@(foo x y) ,g h i j))
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e)
	   (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))

;; Makes a predicate that checks for a single element list tagged by the given tag.
(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))



; Void object
(define *void-object* (if #f #f))

; Simple constant identification procedure
(define simple-const?
  (lambda (const)
    (or (boolean? const)
        (number? const)
        (char? const)
        (string? const))))

; Reserved words for this assignment
(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote 
    unquote-splicing quote set!))

; Beginify to begin
(define beginify
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        `(begin ,@lst))))

; Reserved word predicate
(define reserved?
  (lambda (sym)
    (ormap (lambda (word) (eq? word sym))
           *reserved-words*)))

; Not reserved word?
(define not-reserved?
  (lambda (sym)
    (not (reserved? sym))))

; Variable predicate
(define var?
  (lambda (var)
    (and (not (simple-const? var))
         (symbol? var)
         (not (reserved? var)))))

; Is it a list of variables?
(define var-list?
  (lambda (lst)
    (and (list? lst)
         (or (null? lst)
             (andmap var? lst)))))

; Verify that a value is a list with at least 2 elements
(define list-length>=2?
  (lambda (value)
    (and (list? value)
         (not (null? value))
         (not (null? (cdr value))))))

; Validate list of let assignments
(define let-list?
  (lambda (lst)
    (if (and (list? lst)
             (not (null? lst))
             (andmap (lambda (assignment)
                       (and (list-length>=2? assignment)
                            (var? (car assignment))
                            (null? (cddr assignment))))
                     lst))
        #t
        #f)))

; Dimsnatle a list of two-element lists to two lists:
; one of the first elements and one of the second
(define split-car-cadr
  (lambda (lst)
    (if (let-list? lst)
        (list (map car lst) (map cadr lst))
        #f)))

; Gets an improper list and separates it to a list of all
; elements except the last, and a list of the last element,
; while ensuring they are all variables
(define extract-opt-vars
  (lambda (val)
    (letrec ((f (lambda (lst cont fail)
                  (if (and (pair? lst) (var? (car lst)))
                      (if (pair? (cdr lst))
                          (f (cdr lst)
                             (lambda (args rest)
                               (cont (cons (car lst) args) rest))
                             fail)
                          (if (var? (cdr lst))
                              (cont (list (car lst)) (cdr lst))
                              (fail)))
                      (fail)))))
      (if (var? val) (list '() val)
          (f val
             (lambda (args rest) (list args rest))
             (lambda () #f))))))

; Applies a procedure on a list of arguments
(define with (lambda (s f) (apply f s)))

; Expands a letrec expression to a Ym invocation
(define expand-letrec
  (lambda (letrec-expr)
    (with letrec-expr
      (lambda (ribs . exprs)
	(let* ((fs (map car ribs))
	       (lambda-exprs (map cdr ribs))
	       (nu (gensym))
	       (nu+fs `(,nu ,@fs))
	       (body-f `(lambda ,nu+fs ,@exprs))
	       (hofs
		(map (lambda (lambda-expr) `(lambda ,nu+fs ,@lambda-expr))
		  lambda-exprs)))
	  `(Ym ,body-f ,@hofs))))))


; Basic parser - main function
(define parse
  (let ((run
	 (compose-patterns
          ;; simple-const
          (pattern-rule
           (? 'c simple-const?)
           (lambda (c) `(const ,c)))
          ;; quote
          (pattern-rule
           `(quote ,(? 'c))
           (lambda (c) `(const ,c)))
          ;; variable
          (pattern-rule
           (? 'v var?)
           (lambda (v) `(var ,v)))
          ;; if without alternative
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit))
           (lambda (test dit)
             `(if3 ,(parse test) ,(parse dit) (const ,*void-object*))))
          ;; normal if
          (pattern-rule
           `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
           (lambda (test dit dif)
             `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
          ;; macro-expander: let* to nested let
          (pattern-rule
           `(let* () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (parse (beginify (cons expr exprs)))))
          (pattern-rule
           `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
           (lambda (var val rest exprs)
             (parse `(let ((,var ,val))
                       (let* ,rest . ,exprs)))))
          ;; sequence - Empty
          (pattern-rule
           `(begin)
           (lambda () `(const ,*void-object*)))
          ;; sequence - Single
          (pattern-rule
           `(begin ,(? 'expr))
           (lambda (expr)
             (parse expr))) 
          ;; sequence - Multiple
          (pattern-rule
           `(begin ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             `(seq (,(parse expr) . ,(map parse exprs))))) 
          ;; simple lambda
          (pattern-rule
           `(lambda ,(? 'vars var-list?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr exprs)
             `(lambda-simple ,vars ,(parse (beginify `(,expr . ,exprs))))))
          ;; optional arguments lambda
          (pattern-rule
           `(lambda (,(? 'var var?) . ,extract-opt-vars) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var vars opts expr exprs)
             `(lambda-opt ,(cons var vars) ,opts ,(parse (beginify `(,expr . ,exprs))))))
          ;; variadic lambda
          (pattern-rule
           `(lambda ,(? 'vars var?) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars expr exprs)
             `(lambda-variadic ,vars ,(parse (beginify `(,expr . ,exprs))))))
          ; define
          (pattern-rule
           `(define ,(? 'var var?) ,(? 'expr))
           (lambda (var expr)
             `(define ,(parse var) ,(parse expr))))
          ; MIT define
          (pattern-rule
           `(define (,(? 'var var?) . ,(? 'vars)) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (var vars expr exprs)
             (parse `(define ,var (lambda ,vars ,expr . ,exprs)))))
          ; Application
          (pattern-rule
           `(,(? 'proc not-reserved?) . ,(? 'exprs))
           (lambda (proc exprs)
             `(applic ,(parse proc) ,(map parse exprs))))
          
          
          ; --------- Macros ---------
          
          ; let: Empty - to application
          (pattern-rule
           `(let () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (parse `((lambda () ,expr . ,exprs)))))
          ; let: Not empty - to application
          (pattern-rule
           `(let ,split-car-cadr ,(? 'expr) . ,(? 'exprs list?))
           (lambda (vars values expr exprs)
             (parse `((lambda ,vars ,expr . ,exprs) . ,values))))
          
          ; and: empty
          (pattern-rule
           `(and)
           (lambda ()
             `,(parse #t)))
          ; and: Single argument
          (pattern-rule
           `(and ,(? 'expr))
           (lambda (expr)
             (parse expr)))
          ; and: Recursive (2 or more arguments) - to if
          (pattern-rule
           `(and ,(? 'expr1) ,(? 'expr2) . ,(? 'exprs list?))
           (lambda (expr1 expr2 exprs)
             (parse `(if ,expr1 (and ,expr2 . ,exprs) #f))))
          
          ; or: empty
          (pattern-rule
           `(or)
           (lambda ()
             `,(parse #f)))
          ; or: Single argument
          (pattern-rule
           `(or ,(? 'expr))
           (lambda (expr)
             (parse expr)))
          ; or: Two or more arguments
          (pattern-rule
           `(or ,(? 'expr1) ,(? 'expr2) . ,(? 'exprs list?))
           (lambda (expr1 expr2 exprs)
             `(or ,(map parse `(,expr1 ,expr2 . ,exprs)))))
          
          ; cond: Empty - to void
          (pattern-rule
           `(cond)
           (lambda ()
             `(const ,*void-object*)))
          ; cond: Else
          (pattern-rule
           `(cond (else ,(? 'expr)))
           (lambda (expr)
             (parse expr)))
          ; cond: General - to if
          (pattern-rule
           `(cond ,(? 'clause list-length>=2?) . ,(? 'clauses list?))
           (lambda (clause clauses)
             (parse `(if ,(car clause)
                         ,(beginify (cdr clause))
                         (cond . ,clauses)))))
          
          ; letrec
          (pattern-rule
           `(letrec . ,expand-letrec)
           (lambda expanded
             (parse expanded)))
          
          ; quasiquote
          (pattern-rule
           (cons 'quasiquote expand-qq)
           (lambda expanded
             `(parse ,expanded)))
          
          )))
    (lambda (e)
      (run e
           (lambda ()
             (error 'parse
                    (format "I can't recognize this: ~s" e)))))))

;predicate for lambda expression.
(define lambda?
  (lambda (pe)
    (and (list? pe)
         (not (null? pe))
         (or (eq? (car pe) 'lambda-simple)
             (eq? (car pe) 'lambda-opt)
             (eq? (car pe) 'lambda-variadic)))))

;Extract the vars from a lambda.
(define lambda->vars
  (lambda (pe)
    (cond ((eq? (car pe) 'lambda-simple) (cadr pe))
          ((eq? (car pe) 'lambda-opt) (append (cadr pe) (list (caddr pe))))
          ((eq? (car pe) 'lambda-variadic) (list (cadr pe)))
          (else '()))))

;Searches the index of expression e inside a list lst
(define list-index
  (lambda (e lst)
    (if (null? lst)
        -1
        (if (eq? (car lst) e)
            0
            (let ((index (list-index e (cdr lst))))
              (if (= index -1) 
                  -1
                  (add1 index)))))))

;Annotates variable to be either fvar or bvar according to the result of member procedure. 
(define annotate-bvar
  (lambda (var vars major)
    (cond ((null? vars) `(fvar ,var))
          ((member var (car vars)) `(bvar ,var ,major ,(list-index var (car vars))))
          (else (annotate-bvar var (cdr vars) (add1 major))))))

;Extracts the name of a bvar.
(define bvar->name
  (lambda (bvar)
    (cadr bvar)))

;Extracts the major index of a bvar.
(define bvar->major
  (lambda (bvar)
    (caddr bvar)))

;Extracts the minor index of a bvar.
(define bvar->minor
  (lambda (bvar)
    (cadddr bvar)))

;Annotate var to be fvar, pvar or bvar.
(define annotate-var
  (lambda (var vars)
    (cond ((null? vars) `(fvar ,var))
          ((member var (car vars)) `(pvar ,var ,(list-index var (car vars))))
          (else (annotate-bvar var (cdr vars) 0)))))

;Extracts the name of a pvar.
(define pvar->name
  (lambda (pvar)
    (cadr pvar)))

;Extracts the minor index of a pvar.
(define pvar->minor
  (lambda (pvar)
    (caddr pvar)))

;Extracts the name of a fvar.
(define fvar->name
  (lambda (fvar)
    (cadr fvar)))

;Annotates variables in the given expression based on
;the list of lists of variables that represents the
;environment
(define aux-pe->lex-pe
  (lambda (pe vars)
    (cond ((not (pair? pe)) pe)
          ((eq? (car pe) 'var) (annotate-var (cadr pe) vars))
          ((lambda? pe)
           (let* ((new-vars (cons (lambda->vars pe) vars))
                  (recursive-call (lambda (x) (aux-pe->lex-pe x new-vars))))
             (map recursive-call pe)))
          ((not (list? (cdr pe))) pe)
          (else (map (lambda (pe) (aux-pe->lex-pe pe vars)) pe)))
    ))

;Wrapper for aux-pe->lex-pe
(define pe->lex-pe
  (lambda (pe)
    (aux-pe->lex-pe pe '())))


; ---------------------
; Annotating Tail Calls
; ---------------------

;Type checking predicate.
(define type?
  (lambda (pe tag)
    (and (pair? pe)
         (eq? tag (car pe)))))

;Checks whether pe's type is one of "tags".
(define one-of-types?
  (lambda (pe tags)
    (and (pair? pe)
         (let ((first (car pe)))
           (ormap (lambda (tag) (eq? first tag)) tags)))))

;Getter: procedure out of application.
(define applic->proc
  (lambda (pe)
    (cadr pe)))
;Getter: arguments out of application.
(define applic->args
  (lambda (pe)
    (caddr pe)))

;Getter: test out of if.
(define if->test
  (lambda (pe)
    (cadr pe)))
;Getter: do-if-true out of if.
(define if->dit
  (lambda (pe)
    (caddr pe)))
;Getter: do-if-false out of if.
(define if->dif
  (lambda (pe)
    (cadddr pe)))

;Splits the whole list apart from the last member of it.
(define split-all-last
  (lambda (pe)
    (if (null? pe)
        pe
        (let ((ep (reverse pe)))
          (list (reverse (cdr ep)) (car ep))))))

;Getter: var out of define.
(define define->var
  (lambda (pe)
    (cadr pe)))
;Getter: body out of define.
(define define->body
  (lambda (pe)
    (caddr pe)))

;Getter: body out of sequence.
(define seq->body
  (lambda (pe)
    (cadr pe)))

;Getter: body out of lambda.
(define lambda->body
  (lambda (pe)
    (cond ((eq? (car pe) 'lambda-simple) (caddr pe))
          ((eq? (car pe) 'lambda-opt) (cadddr pe))
          ((eq? (car pe) 'lambda-variadic) (caddr pe))
          (else '()))))
;Replaces the body inside a lambda.
(define replace-lambda-body
  (lambda (pe new-body)
    (cond ((eq? (car pe) 'lambda-simple) (list (car pe) (cadr pe) new-body))
          ((eq? (car pe) 'lambda-opt) (list (car pe) (cadr pe) (caddr pe) new-body))
          ((eq? (car pe) 'lambda-variadic) (list (car pe) (cadr pe) new-body))
          (else '()))))

;Annotates tc to be false.
(define aux-annotate-tc-false
  (lambda (pe)
    (aux-annotate-tc pe #f)))

;Annotates tc-applic for all types in all cases.
(define aux-annotate-tc
  (lambda (pe in-tp?)
    (cond
      ((one-of-types? pe '(const var fvar bvar pvar)) pe)
      ((type? pe 'if3) `(if3 ,(aux-annotate-tc (if->test pe) #f)
                             ,(aux-annotate-tc (if->dit pe) in-tp?)
                             ,(aux-annotate-tc (if->dif pe) in-tp?)))
      ((type? pe 'or)
       (let ((or-parts (split-all-last (or->body pe))))
         `(or ,(append (map aux-annotate-tc-false (car or-parts))
                       (list (aux-annotate-tc (cadr or-parts) in-tp?))))))
      ((type? pe 'define) `(define ,(define->var pe) ,(aux-annotate-tc (define->body pe) #f)))
      ((type? pe 'seq)
       (let ((seq-parts (split-all-last (seq->body pe))))
         `(seq ,(append (map aux-annotate-tc-false (car seq-parts))
                        (list (aux-annotate-tc (cadr seq-parts) in-tp?))))))
      ((lambda? pe)
       (replace-lambda-body pe (aux-annotate-tc (lambda->body pe) #t)))
      ((type? pe 'applic)
       (let ((proc (applic->proc pe))
             (args (applic->args pe)))
         (let ((new-proc (aux-annotate-tc proc #f))
               (new-args (map aux-annotate-tc-false args)))
           (if in-tp?
               `(tc-applic ,new-proc
                           ,new-args)
               `(applic ,new-proc
                        ,new-args)))))
      (else pe))
       ))

;Annotates tc-applic - main application.
(define annotate-tc
  (lambda (pe)
    (aux-annotate-tc pe #f)))


; ---------------------------------------------------------------------- ;
; ------------------------ Final Project Code -------------------------- ;
; ---------------------------------------------------------------------- ;

;Same as map, but activates the given function on the
;elements of the given list *in the order they appear*.
(define rmap
  (lambda (func lst)
    (if (null? lst)
        lst
        (let* ((first (func (car lst)))
               (rest (rmap func (cdr lst))))
          (cons first rest)))))

;Takes a file and converts it's strings to be sexprs.
(define file->sexprs
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
                (lambda ()
                  (let ((e (read input)))
                    (if (eof-object? e)
                        (begin (close-input-port input)
                               '())
                        (cons e (run)))))))
        (run)))))

;To-string.
(define to-string
  (lambda (c)
    (if (string? c)
        c
        (format "~s" c))))

;Getter: value out of const.
(define const->value
  (lambda (pe)
    (cadr pe)))

;Make-label to be "name" with increasing counter.
(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name
                       (number->string n))))))

;All the label makers of which our compiler makes use
(define ^label-if3else (^^label "Lif3else"))
(define ^label-if3exit (^^label "Lif3exit"))
(define ^label-lambda-simple-code (^^label "Llambda_simp_code"))
(define ^label-lambda-simple-exit (^^label "Llambda_simp_exit"))
(define ^label-lambda-opt-code (^^label "Llambda_opt_code"))
(define ^label-lambda-opt-exit (^^label "Llambda_opt_exit"))
(define ^label-lambda-opt-stackcopy-loop (^^label "Llambda_opt_stackcopy_loop"))
(define ^label-lambda-opt-stackcopy-exit (^^label "Llambda_opt_stackcopy_exit"))
(define ^label-lambda-var-code (^^label "Llambda_var_code"))
(define ^label-lambda-var-exit (^^label "Llambda_var_exit"))
(define ^label-lambda-var-stackcopy-loop (^^label "Llambda_var_stackcopy_loop"))
(define ^label-lambda-var-stackcopy-exit (^^label "Llambda_var_stackcopy_exit"))
(define ^label-lambda-envext-simple-firstloop (^^label "Llambda_envext_simple_firstloop"))
(define ^label-lambda-envext-simple-firstloop-end (^^label "Llambda_envext_simple_firstloop_end"))
(define ^label-lambda-envext-simple-secondloop (^^label "Llambda_envext_simple_secondloop"))
(define ^label-lambda-envext-simple-secondloop-end (^^label "Llambda_envext_simple_secondloop_end"))
(define ^label-lambda-envext-opt-firstloop (^^label "Llambda_envext_opt_firstloop"))
(define ^label-lambda-envext-opt-firstloop-end (^^label "Llambda_envext_opt_firstloop_end"))
(define ^label-lambda-envext-opt-secondloop (^^label "Llambda_envext_opt_secondloop"))
(define ^label-lambda-envext-opt-secondloop-end (^^label "Llambda_envext_opt_secondloop_end"))
(define ^label-lambda-envext-var-firstloop (^^label "Llambda_envext_var_firstloop"))
(define ^label-lambda-envext-var-firstloop-end (^^label "Llambda_envext_var_firstloop_end"))
(define ^label-lambda-envext-var-secondloop (^^label "Llambda_envext_var_secondloop"))
(define ^label-lambda-envext-var-secondloop-end (^^label "Llambda_envext_var_secondloop_end"))
(define ^label-tc-applic-loop (^^label "Ltc_applic_loop"))
(define ^label-tc-applic-exit (^^label "Ltc_applic_exit"))

(define ^label-or-exit (^^label "Lor_exit"))
;Newline.
(define nl (list->string (list #\newline)))
;Void predicate
(define void?
  (lambda (e)
    (eq? *void-object* e)))

;Getter: name out of var.
(define var->name
  (lambda (var)
    (cadr var)))

;Adding comments at the beginning and at the end of a code.
(define document
  (lambda (depth code . comment)
    (let ((comment-string (apply string-append
                                 (map to-string comment))))
      (string-append (indent depth)
                     "/* "
                     comment-string
                     " */" nl
                     code
                     (indent depth) "/* End of "
                     comment-string
                     " */" nl))))

;Creates documentation strings from the given Scheme elements of a comment
(define ^doc
  (lambda (depth . comment)
    (let ((comment-string (apply string-append
                                 (map to-string comment))))
      (cons
       (string-append (indent depth) "/* " comment-string " */" nl)
       (string-append (indent depth) "/* End of " comment-string " */" nl)))))
;Gets the part of the documentation that goes before the code
(define doc->pre car)
;Gets the part of the documentation that goes after the code
(define doc->post cdr)


; Deals with code indentation.
(define ^indentation
  (lambda (depth)
    (if (zero? depth)
        ""
        (string-append "\t" (^indentation (sub1 depth))))))
; Allows tracing line number during execution if set to #t
; here and the TRACE_LINES macro constant is defined in gcc
(define *trace-lines* #f)
(define indent
  (lambda (depth)
    (if *trace-lines*
        (string-append "DEBUG\t" (^indentation depth))
        (^indentation depth))))

; Allows for a quick change between full or fast environment simulation
(define env-extension-type car)

;Extends a given environment.
(define extend-env
  (let* ((full (lambda (env vars) (cons vars env)))
         (fast (lambda (env vars) (add1 env)))
         (options `(,full ,fast)))
    (env-extension-type options)))

;Creates new environment.
(define new-env
  (let* ((full (lambda () '()))
         (fast (lambda () 0))
         (options `(,full ,fast)))
    (env-extension-type options)))
(define new-params new-env)

;Getter: environment size.
(define env-size
  (let* ((full (lambda (env) (length env)))
         (fast (lambda (env) env))
         (options `(,full ,fast)))
    (env-extension-type options)))
(define params-size env-size)

;Predicate: is the environment empty?
(define env-empty?
  (let* ((full null?)
         (fast zero?)
         (options `(,full ,fast)))
    (env-extension-type options)))

;Code-gen for sequence expression.
(define code-gen-seq
  (lambda (pe env params depth)
    (let* ((doc (^doc depth "Sequence: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc)))
      (begin
        (fwrite pre-doc)
        (rmap (lambda (pe)
                (code-gen pe env params (add1 depth)))
              (seq->body pe))
        (fwrite post-doc)
        ))))

;Code-gen for if expression.
(define code-gen-if3
  (lambda (e env params depth)
    (with e
          (lambda (if3 test do-if-true do-if-false)
            (let* ((depth+1 (add1 depth))
                   (-> (indent depth+1))
                   (doc (^doc depth "If: " e))
                   (pre-doc (doc->pre doc))
                   (post-doc (doc->post doc))
                   (label-else (^label-if3else))
                   (label-exit (^label-if3exit)))
              (begin
                (fwrite pre-doc)
                (code-gen test env params depth+1) ; when run, the result of the test will be in R0
                (fwrite -> "CMP(R0, SOB_FALSE);" nl)
                (fwrite -> "JUMP_EQ(" label-else ")" nl)
                (code-gen do-if-true env params depth+1)
                (fwrite -> "JUMP(" label-exit ")" nl)
                (fwrite -> label-else ":" nl)
                (code-gen do-if-false env params depth+1)
                (fwrite -> label-exit ":" nl)
                (fwrite post-doc)
                ))))))

;Code-gen for const.
(define code-gen-const
  (lambda (pe env params depth)
    (let ((value (const->value pe)))
      (fwrite
       (document depth
                 (string-append
                  (indent depth) "MOV(R0, "
                  (cond ((void? value) "IMM(SOB_VOID)")
                        ((null? value) "IMM(SOB_NIL)")
                        ((eq? value #f) "IMM(SOB_FALSE)")
                        ((eq? value #t) "IMM(SOB_TRUE)")
                        (else (let ((entry (ctbl-lookup value)))
                                (if entry
                                    (string-append "IMM(" (to-string (ctbl-entry->addr entry)) ")")
                                    (error "code-gen-const" "Unknown constant")))))
                  ");"
                  nl)
                 "Constant: " pe)))))

;Code-gen for free vars.
(define code-gen-fvar
  (lambda (pe env params depth)
    (let ((name (var->name pe)))
      (fwrite
       (document depth
                 (string-append (indent depth)
                                "MOV(R0, IMM("
                                (let ((entry (ftbl-lookup name)))
                                  (if entry
                                      (to-string (ftbl-entry->addr entry))
                                      (error "code-gen-fvar" "Unknown free variable")))
                                "));" nl
                                (indent depth)
                                "MOV(R0, IND(R0));" nl)
                 "Free var: " name)))))

;Code-gen for application.
(define code-gen-applic
  (lambda (pe env params depth)
    (let* ((args (applic->args pe))
           (proc (applic->proc pe))
           (m (length args))
           (doc (^doc depth "Applic: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (-> (indent (add1 depth))))
      (begin
        (fwrite pre-doc)
        (fwrite -> "PUSH(IMM(SOB_NIL));" nl)
        (rmap (lambda (pe)
                (begin
                  (code-gen pe env params (add1 depth))
                  (fwrite -> "PUSH(R0);" nl)))
              (reverse args))
        (fwrite -> "PUSH(" (to-string (add1 m)) ");" nl)
        (code-gen proc env params (add1 depth))
        (fwrite -> "CMP(IND(R0), T_CLOSURE);" nl)
        (fwrite -> "JUMP_NE(Lnot_proc);" nl)
        (fwrite -> "PUSH(INDD(R0, 1));" nl)
        (fwrite -> "CALLA(INDD(R0, 2));" nl)
        (fwrite -> "DROP(2 + STARG(0));" nl)
      (fwrite post-doc)          
      ))))

;Code-gen for environment extension.
(define code-gen-env-extension
  (lambda (env params depth code-label exit-label first-loop-label first-loop-label-end second-loop-label second-loop-label-end)
    (let* ((> (indent depth))
           (-> (string-append > "\t")))
      (begin
       (fwrite -> "PUSH(IMM(" (to-string (add1 (env-size env))) "));" nl)
       (fwrite -> "CALL(MALLOC);" nl)
       (fwrite -> "DROP(1);" nl)
       (fwrite -> "MOV(R1, R0); /* New environment */" nl)
       (fwrite -> "MOV(R2, " (if (env-empty? env) "0" "FPARG(0)") "); /* Old environment */" nl)
       (fwrite -> "/* Copy the existing environment */" nl)
       (fwrite -> "MOV(R4, 0);" nl)
       (fwrite -> "MOV(R5, 1);" nl)
       (fwrite > first-loop-label ":" nl)
       (fwrite -> "CMP(R4, IMM(" (to-string (env-size env)) "));" nl)
       (fwrite -> "JUMP_GE(" first-loop-label-end ");" nl)
       (fwrite -> "MOV(INDD(R1, R5), INDD(R2, R4));" nl)
       (fwrite -> "INCR(R4);" nl)
       (fwrite -> "INCR(R5);" nl)
       (fwrite -> "JUMP(" first-loop-label ");" nl)
       (fwrite > first-loop-label-end ":" nl)
       (fwrite -> "/* Extend the copied environment with the current parameters */" nl)
       (fwrite -> "PUSH(IMM(" (to-string (params-size params)) "));" nl)
       (fwrite -> "CALL(MALLOC);" nl)
       (fwrite -> "DROP(1);" nl)
       (fwrite -> "MOV(R3, R0);" nl)
       (fwrite -> "MOV(R4, 0);" nl)
       (fwrite > second-loop-label ":" nl)
       (fwrite -> "CMP(R4, IMM(" (to-string (params-size params)) "));" nl)
       (fwrite -> "JUMP_GE(" second-loop-label-end ");" nl)
       (fwrite -> "MOV(INDD(R3, R4), SCMARG(R4));" nl)
       (fwrite -> "INCR(R4);" nl)
       (fwrite -> "JUMP(" second-loop-label ");" nl)
       (fwrite > second-loop-label-end ":" nl)
       (fwrite -> "MOV(INDD(R1, 0), R3); /* Add new frame to environment */" nl)
       (fwrite -> nl)
       (fwrite -> "PUSH(IMM(3));" nl)
       (fwrite -> "CALL(MALLOC);" nl)
       (fwrite -> "DROP(1);" nl)
       (fwrite -> "MOV(INDD(R0, 0), T_CLOSURE);" nl)
       (fwrite -> "MOV(INDD(R0, 1), R1);" nl)
       (fwrite -> "MOV(INDD(R0, 2), &&" code-label ");" nl)
       (fwrite -> "JUMP(" exit-label ");" nl)
       (fwrite -> nl)
       (fwrite > code-label ": /* Code for the procedure */" nl)
       ))))

;Code-gen for lambda simple.
(define code-gen-lambda-simple
  (lambda (pe env params depth)
    (let* ((vars (lambda->vars pe))
           (body (lambda->body pe))
           (> (indent depth))
           (-> (string-append > "\t"))
           (--> (string-append -> "\t"))
           (doc (^doc depth "Lambda-Simple: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (code-label (^label-lambda-simple-code))
           (exit-label (^label-lambda-simple-exit))
           (first-loop-label (^label-lambda-envext-simple-firstloop))
           (first-loop-label-end (^label-lambda-envext-simple-firstloop-end))
           (second-loop-label (^label-lambda-envext-simple-secondloop))
           (second-loop-label-end (^label-lambda-envext-simple-secondloop-end)))
      (begin
        (fwrite pre-doc)
        (code-gen-env-extension env
                                params
                                depth
                                code-label
                                exit-label
                                first-loop-label
                                first-loop-label-end
                                second-loop-label
                                second-loop-label-end)
        (fwrite -> "PUSH(FP);" nl)
        (fwrite -> "MOV(FP, SP);" nl)
        (code-gen body (extend-env env params) vars (add1 depth))
        (fwrite -> "POP(FP);" nl)
        (fwrite -> "RETURN;" nl)
        (fwrite > exit-label ":" nl)
        (fwrite post-doc)
        ))))

;Code-gen for lambda optional.
(define code-gen-lambda-opt
  (lambda (pe env params depth)
    (let* ((vars (lambda->vars pe))
           (body (lambda->body pe))
           (num-fixed-vars (sub1 (length vars)))
           (> (indent depth))
           (-> (string-append > "\t"))
           (--> (string-append -> "\t"))
           (doc (^doc depth "Lambda-Opt: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (code-label (^label-lambda-opt-code))
           (exit-label (^label-lambda-opt-exit))
           (stack-loop-label (^label-lambda-opt-stackcopy-loop))
           (stack-exit-label (^label-lambda-opt-stackcopy-exit))
           (first-loop-label (^label-lambda-envext-opt-firstloop))
           (first-loop-label-end (^label-lambda-envext-opt-firstloop-end))
           (second-loop-label (^label-lambda-envext-opt-secondloop))
           (second-loop-label-end (^label-lambda-envext-opt-secondloop-end)))
      (begin
        (fwrite pre-doc)
        (code-gen-env-extension env
                                params
                                depth
                                code-label
                                exit-label
                                first-loop-label
                                first-loop-label-end
                                second-loop-label
                                second-loop-label-end)
        (fwrite -> "PUSH(FP);" nl)
        (fwrite -> "MOV(FP, SP);" nl)
        (fwrite -> "MOV(R1, NUM_ARGS);" nl)
        (fwrite -> "SUB(R1, " (to-string (add1 num-fixed-vars)) ");" nl)
        (fwrite -> "PUSH(R1); /* Number of optional parameters */" nl)
        (fwrite -> "MOV(R2, FP);" nl)
        (fwrite -> "SUB(R2, 3);" nl)
        (fwrite -> "SUB(R2, NUM_ARGS); /* Address of bottom optional parameter */" nl)
        (fwrite -> "PUSH(R2);" nl)
        (fwrite -> "CALL(MAKE_LIST);" nl)
        (fwrite -> "DROP(2);" nl)
        (fwrite -> "MOV(SCMARG(" (to-string num-fixed-vars) "), R0); /* Replace first optional parameter with result */" nl)
        (fwrite -> "DECR(R2); /* Address of copy destination */" nl)
        (fwrite -> "MOV(R3, " (to-string num-fixed-vars) ");" nl)
        (fwrite -> "ADD(R3, 5); /* Number of memory cells to copy down (mandatories, FP, RetAddr, env, NumArgs, created list) */" nl)
        (fwrite -> "MOV(R4, FP);" nl)
        (fwrite -> "SUB(R4, R3); /* Address of bottom cell to be copied (copy source) */" nl)
        (fwrite > stack-loop-label ":" nl)
        (fwrite -> "CMP(R3, 0);" nl)
        (fwrite -> "JUMP_LE(" stack-exit-label ");" nl)
        (fwrite -> "MOV(STACK(R2), STACK(R4));" nl)
        (fwrite -> "INCR(R2);" nl)
        (fwrite -> "INCR(R4);" nl)
        (fwrite -> "DECR(R3);" nl)
        (fwrite -> "JUMP(" stack-loop-label ");" nl)
        (fwrite > stack-exit-label ":" nl)
        (fwrite -> "SUB(R4, R2); /* Calculate copy distance */" nl)
        (fwrite -> "SUB(FP, R4); /* Correct current FP */" nl)
        (fwrite -> "SUBSP(R4); /* Correct current SP */" nl)
        (fwrite -> "SUB(NUM_ARGS, R4); /* Correct number of arguments */" nl)
        (code-gen body (extend-env env params) vars (add1 depth))
        (fwrite -> "POP(FP);" nl)
        (fwrite -> "RETURN;" nl)
        (fwrite > exit-label ":" nl)
        (fwrite post-doc)
        ))))

;Code-gen for lambda variadic.
(define code-gen-lambda-var
  (lambda (pe env params depth)
    (let* ((vars (lambda->vars pe))
           (body (lambda->body pe))
           (> (indent depth))
           (-> (string-append > "\t"))
           (--> (string-append -> "\t"))
           (doc (^doc depth "Lambda-Variadic: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (code-label (^label-lambda-var-code))
           (exit-label (^label-lambda-var-exit))
           (stack-loop-label (^label-lambda-var-stackcopy-loop))
           (stack-exit-label (^label-lambda-var-stackcopy-exit))
           (first-loop-label (^label-lambda-envext-var-firstloop))
           (first-loop-label-end (^label-lambda-envext-var-firstloop-end))
           (second-loop-label (^label-lambda-envext-var-secondloop))
           (second-loop-label-end (^label-lambda-envext-var-secondloop-end)))
      (begin
        (fwrite pre-doc)
        (code-gen-env-extension env
                                params
                                depth
                                code-label
                                exit-label
                                first-loop-label
                                first-loop-label-end
                                second-loop-label
                                second-loop-label-end)
        (fwrite -> "PUSH(FP);" nl)
        (fwrite -> "MOV(FP, SP);" nl)
        (fwrite -> "MOV(R1, NUM_ARGS);" nl)
        (fwrite -> "DECR(R1);" nl)
        (fwrite -> "PUSH(R1); /* Number of optional parameters */" nl)
        (fwrite -> "MOV(R2, FP);" nl)
        (fwrite -> "SUB(R2, 3);" nl)
        (fwrite -> "SUB(R2, NUM_ARGS); /* Address of bottom optional parameter */" nl)
        (fwrite -> "PUSH(R2);" nl)
        (fwrite -> "CALL(MAKE_LIST);" nl)
        (fwrite -> "DROP(2);" nl)
        (fwrite -> "MOV(SCMARG(0), R0); /* Replace first optional parameter with result */" nl)
        (fwrite -> "DECR(R2); /* Address of copy destination */" nl)
        (fwrite -> "MOV(R3, 5); /* Number of memory cells to copy down */" nl)
        (fwrite -> "MOV(R4, FP);" nl)
        (fwrite -> "SUB(R4, R3); /* Address of bottom cell to be copied (copy source) */" nl)
        (fwrite > stack-loop-label ":" nl)
        (fwrite -> "CMP(R3, 0);" nl)
        (fwrite -> "JUMP_LE(" stack-exit-label ");" nl)
        (fwrite -> "MOV(STACK(R2), STACK(R4));" nl)
        (fwrite -> "INCR(R2);" nl)
        (fwrite -> "INCR(R4);" nl)
        (fwrite -> "DECR(R3);" nl)
        (fwrite -> "JUMP(" stack-loop-label ");" nl)
        (fwrite > stack-exit-label ":" nl)
        (fwrite -> "SUB(R4, R2); /* Calculate copy distance */" nl)
        (fwrite -> "SUB(FP, R4); /* Correct current FP */" nl)
        (fwrite -> "SUBSP(R4); /* Correct current SP */" nl)
        (fwrite -> "SUB(NUM_ARGS, R4); /* Correct number of arguments */" nl)
        (code-gen body (extend-env env params) vars (add1 depth))
        (fwrite -> "POP(FP);" nl)
        (fwrite -> "RETURN;" nl)
        (fwrite > exit-label ":" nl)
        (fwrite post-doc)
        ))))

;Code-gen for parameters variables.
(define code-gen-pvar
  (lambda (pe env params depth)
    (let* ((minor (pvar->minor pe))
           (doc (^doc depth "pvar: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (-> (indent (add1 depth))))
      (begin
        (fwrite pre-doc)
        (fwrite -> "MOV(R0, SCMARG(" (to-string minor) "));" nl)
        (fwrite post-doc)
        ))))

;Code-gen for bound variables.
(define code-gen-bvar
  (lambda (pe env params depth)
    (let* ((major (bvar->major pe))
           (minor (bvar->minor pe))
           (doc (^doc depth "bvar: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (-> (indent (add1 depth))))
      (begin
        (fwrite pre-doc)
        (fwrite -> "MOV(R0, FPARG(0));" nl)
        (fwrite -> "MOV(R0, INDD(R0, " (to-string major) "));" nl)
        (fwrite -> "MOV(R0, INDD(R0, " (to-string minor) "));" nl)
        (fwrite post-doc)
        ))))

;Getter: body out of or.
(define or->body
  (lambda (pe)
    (cadr pe)))

;Code-gen for or expression.
(define code-gen-or
  (lambda (pe env params depth)
    (let* ((body-parts (split-all-last (or->body pe)))
           (all-but-last (car body-parts))
           (last (cadr body-parts))
           (> (indent depth))
           (-> (string-append > "\t"))
           (doc (^doc depth "Or: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (exit-label (^label-or-exit)))
      (begin
        (fwrite pre-doc)
        (rmap (lambda (pe)
                (begin
                  (code-gen pe env params (add1 depth))
                  (fwrite -> "CMP(R0, IMM(SOB_FALSE));" nl)
                  (fwrite -> "JUMP_NE(" exit-label ");" nl)))
              all-but-last)
        (code-gen last env params (add1 depth))
        (if (not (null? all-but-last))
            (fwrite > exit-label ":" nl))
        (fwrite post-doc)
        ))))


; ------------------------------------- ;
; ------------- CONSTANTS ------------- ;
; ------------------------------------- ;

;Make constant table.
(define ^ctbl
  (lambda (size table) (cons size table)))
;Getter: size out of constant table.
(define ctbl->size car)
;Getter: table out of constant table.
(define ctbl->table cdr)
;Getter: address out of constant table entry.
(define ctbl-entry->addr car)
;Getter: constant out of constant table entry.
(define ctbl-entry->const cadr)
;Getter: data out of constant table entry.
(define ctbl-entry->data caddr)
;Make constant table entry.
(define ^ctbl-entry
  (lambda (addr const data) (list addr const data)))

;Searches "const" inside the constant table.
(define ctbl-table-lookup
  (lambda (table const)
    (ormap (lambda (entry) (if (equal? const (ctbl-entry->const entry)) entry #f))
           table)))
;Wrapper for ctbl-table-lookup.
(define ctbl-lookup
  (lambda (const)
    (ctbl-table-lookup (ctbl->table (unbox const-table)) const)))

(define *const-table-location* 1000) ; Basic constants and primitives closures stored at beginning
;Defenition for constant table.
(define const-table (box (^ctbl *const-table-location* '())))

;Adds a constant to the table.
(define add-constant
  (lambda (const data)
    (let* ((ctbl (unbox const-table))
           (size (ctbl->size ctbl))
           (table (ctbl->table ctbl))
           (entry (ctbl-table-lookup table const)))
      (if entry
          entry
          (let ((new-entry (^ctbl-entry size const data)))
            (set-box! const-table (^ctbl (+ size (length data)) (cons new-entry table)))
            new-entry)))))

;Wrapper for add-constant, deals with all types of constants.
(define recursive-add-const
  (lambda (const)
    (cond ((number? const) (add-constant const `("T_INTEGER" ,const)))
          ((char? const) (add-constant const `("T_CHAR" ,(char->integer const))))
          ((string? const)
           (add-constant const
                         `("T_STRING"
                           ,(string-length const)
                           ,@(map char->integer (string->list const)))))
          ((symbol? const)
           (let* ((entry (recursive-add-const (symbol->string const)))
                  (new-entry (add-constant const `("T_SYMBOL" ,(ctbl-entry->addr entry))))) ; Add to constant table
             (add-symbol const (ctbl-entry->addr new-entry)) ; Add to symbol table
             new-entry))
          ((pair? const)
           (let ((first (recursive-add-const (car const)))
                 (second (recursive-add-const (cdr const))))
             (add-constant const `("T_PAIR" ,(ctbl-entry->addr first) ,(ctbl-entry->addr second)))))
          ((vector? const)
           (add-constant const
                         `("T_VECTOR"
                           ,(vector-length const)
                           ,@(map (lambda (c) (ctbl-entry->addr (recursive-add-const c))) (vector->list const)))))
          ((boolean? const) (if const
                                (^ctbl-entry "SOB_TRUE" #t `("T_BOOL" 1))
                                (^ctbl-entry "SOB_FALSE" #f `("T_BOOL" 0))))
          ((void? const) (^ctbl-entry "SOB_VOID" const `("T_VOID")))
          ((null? const) (^ctbl-entry "SOB_NIL" const `("T_NIL")))
          (else (error "recursive-add-const" (format "~s" const)))
          )))

;Defines the constant table.
(define define-const-table
  (lambda ()
    (let* ((table (ctbl->table (unbox const-table)))
           (all-data (apply append (reverse (map ctbl-entry->data table))))
           (data-string (map (lambda (value) (string-append (to-string value) ", ")) all-data))
           (const-array (string-append "long const_table[] = { " (apply string-append data-string) "};")))
      (document 1 (string-append (indent 1) const-array nl) "Constant table definition"))))

;Loads the constant table to the memory.
(define load-const-table
  (lambda ()
    (document 1
              (string-append
               (indent 1)
               "memcpy((void*) &ADDR("
               (to-string *const-table-location*)
               "), (void*) const_table, "
               (to-string (- (const-table-end) *const-table-location*))
               " * WORD_SIZE);" nl)
              "Copy constant table to machine memory")))

;Defines the constant table end.
(define const-table-end (lambda () (ctbl->size (unbox const-table))))

;Make free vars table
(define ^ftbl
  (lambda (base-address lst)
    (letrec ((f (lambda (lst address)
                  (if (null? lst)
                      lst
                      (cons `(,address ,(caar lst) ,(symbol->string (cadar lst)))
                            (f (cdr lst) (add1 address)))))))
      (f lst base-address))))

;Add a free var to the table.
(define add-free-var
  (lambda (name)
    (if (not (assoc name (unbox free-vars)))
        (set-box! free-vars (cons (list name 'undef) (unbox free-vars))))))

;Wrapper for ^ftbl
(define build-fvar-table
  (lambda (base-address)
    (^ftbl base-address (reverse (unbox free-vars)))))

;Free vars table initialization
(define fvar-table
  (box '()))

;Getter: address out of free vars table entry.
(define ftbl-entry->addr car)
;Getter: name out of free vars table entry.
(define ftbl-entry->name cadr)
;Getter: value out of free vars table entry.
(define ftbl-entry->value caddr)

;Lookup a free variable name in the free vars table.
(define ftbl-lookup
  (lambda (name)
    (ormap (lambda (entry) (if (eq? name (ftbl-entry->name entry)) entry #f))
           (unbox fvar-table))))

;Defines the free var table
(define define-fvar-table
  (lambda ()
    (let* ((table (unbox fvar-table))
           (all-data (map ftbl-entry->value table))
           (data-string (map (lambda (value) (string-append (to-string value) ", ")) all-data))
           (fvar-array (string-append "long fvar_table[] = { " (apply string-append data-string) "};")))
      (document 1 (string-append (indent 1) fvar-array nl) "Free-var table definition"))))

;Loads the free variables table to memory
(define load-fvar-table
  (lambda ()
    (document 1
              (string-append
               (indent 1)
               "memcpy((void*) &ADDR("
               (to-string (const-table-end))
               "), (void*) fvar_table, "
               (to-string (length (unbox fvar-table)))
               " * WORD_SIZE);" nl)
              "Copy free-var table to machine memory")))

;Defines the free variables table end.
(define fvar-table-end (lambda () (+ (const-table-end) (length (unbox fvar-table)))))

;Code-gen for define expression.
(define code-gen-define
  (lambda (pe env params depth)
    (let* ((body (define->body pe))
           (var (define->var pe))
           (var-name (fvar->name var))
           (entry (ftbl-lookup var-name))
           (doc (^doc depth "Define: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (-> (indent (add1 depth))))
      (if entry
          (begin
            (fwrite pre-doc)
            (code-gen body env params (add1 depth))
            (fwrite -> "MOV(IND(" (to-string (ftbl-entry->addr entry)) "), R0);" nl)
            (fwrite -> "MOV(R0, IMM(SOB_VOID));" nl)
            (fwrite post-doc))
          (error "code-gen-define" (string-append "Unknown free variable: " (to-string var-name)))))))


;Code-gen for tail call application.
(define code-gen-tc-applic
  (lambda (pe env params depth)
    (let* ((args (applic->args pe))
           (proc (applic->proc pe))
           (m (length args))
           (> (indent depth))
           (-> (string-append > "\t"))
           (doc (^doc depth "TC-Applic: " pe))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (loop-label (^label-tc-applic-loop))
           (exit-label (^label-tc-applic-exit)))
      (begin
        (fwrite pre-doc)
        (fwrite -> "PUSH(IMM(SOB_NIL));" nl)
        (rmap (lambda (pe)
                (begin
                  (code-gen pe env params (add1 depth))
                  (fwrite -> "PUSH(R0);" nl)))
              (reverse args))
        (fwrite -> "PUSH(" (to-string (add1 m)) ");" nl)
        (code-gen proc env params (add1 depth))
        (fwrite -> "CMP(IND(R0), IMM(T_CLOSURE));" nl)
        (fwrite -> "JUMP_NE(Lnot_proc);" nl)
        (fwrite -> "PUSH(INDD(R0, 1));" nl)
        (fwrite -> "PUSH(FPARG(-1));" nl)
        (fwrite )
        (fwrite -> "MOV(R1, FPARG(-2));" nl)
        (fwrite )
        (fwrite -> "MOV(R2, FP);" nl)
        (fwrite -> "SUB(R2, IMM(4));" nl)
        (fwrite -> "SUB(R2, NUM_ARGS); /* Address of copy destination */" nl)
        (fwrite -> "MOV(R3, SP);" nl)
        (fwrite -> "SUB(R3, FP); /* Number of memory cells to copy down (= SP - FP) */" nl)
        (fwrite -> "MOV(R4, FP); /* Address of bottom cell to be copied (copy source) */" nl)
        (fwrite > loop-label ":" nl)
        (fwrite -> "CMP(R3, IMM(0));" nl)
        (fwrite -> "JUMP_LE(" exit-label ");" nl)
        (fwrite -> "MOV(STACK(R2), STACK(R4));" nl)
        (fwrite -> "INCR(R2);" nl)
        (fwrite -> "INCR(R4);" nl)
        (fwrite -> "DECR(R3);" nl)
        (fwrite -> "JUMP(" loop-label ");" nl)
        (fwrite > exit-label ":" nl)
        (fwrite -> "MOVSP(R2);" nl)
        (fwrite -> "MOV(FP, R1);" nl)
        (fwrite -> "/* Call the procedure */" nl)
        (fwrite -> "JUMPA(INDD(R0, 2));" nl))
        (fwrite post-doc)
      )))

;Make entry to the symbol table.
(define stbl-make-entry
  (lambda (rel-addr symbol data next)
    (list rel-addr symbol data next)))

;Getter: address out of symbol table entry.
(define stbl-entry->rel-addr car)
;Getter: symbol out of symbol table entry.
(define stbl-entry->symbol cadr)
;Getter: data out of symbol table entry.
(define stbl-entry->data caddr)
;Getter: next symbol out of symbol table entry.
(define stbl-entry->next cadddr)
;Getter: full data(all the symbols in an entry) out of symbol table entry.
(define stbl-entry->full-data
  (lambda (entry)
    (list (stbl-entry->data entry) (stbl-entry->next entry))))
(define *stbl-entry-data-size* 2)
;Lookup symbol inside the symbol table.
(define sym-table-lookup
  (lambda (sym)
    (ormap (lambda (entry) (if (eq? sym (stbl-entry->symbol entry)) entry #f))
           (unbox sym-tab))))
;Adding a symbol to the symbol table.
(define add-symbol
  (lambda (sym data)
    (let ((entry (sym-table-lookup sym))
          (table (unbox sym-tab)))
      (if (not entry)
          (begin
            (set! *sym-tab-location* (- *sym-tab-location* *stbl-entry-data-size*))
            (if (null? table)
                (set-box! sym-tab (list (stbl-make-entry 0 sym data 0)))
                (let ((first-entry-addr (stbl-entry->rel-addr (car table))))
                  (set-box! sym-tab
                            (cons (stbl-make-entry (- first-entry-addr *stbl-entry-data-size*) sym data first-entry-addr)
                                  (unbox sym-tab))))))))))

;Shifts the symbol table according to offset which we know only after creating all the other tables(free vars, etc...)
(define shift-sym-tab
  (lambda (offset)
    (letrec ((shift (lambda (table offset)
                      (if (null? table)
                          table
                          (with (car table)
                                (lambda (rel-addr symbol data next)
                                  (cons (stbl-make-entry (+ offset rel-addr)
                                                         symbol
                                                         data
                                                         (if (null? (cdr table))
                                                             next
                                                             (+ offset next)))
                                        (shift (cdr table) offset))))))))
      (set! *sym-tab-location* (+ *sym-tab-location* offset))
      (set-box! sym-tab (shift (unbox sym-tab) offset)))))
;Getter: size of symbol table.
(define sym-table-size
  (lambda ()
    (* (length (unbox sym-tab)) *stbl-entry-data-size*)))

;Defines the symbol table.
(define define-sym-table
  (lambda ()
    (let* ((table (unbox sym-tab))
           (all-data (apply append (map stbl-entry->full-data table)))
           (data-string (map (lambda (value) (string-append (to-string value) ", ")) all-data))
           (const-array (string-append "long sym_table[] = { " (apply string-append data-string) "};")))
      (document 1 (string-append (indent 1) const-array nl) "(Initial) Symbol table definition"))))

;Loads the symbol table to memory.
(define load-sym-table
  (lambda ()
    (document 1
              (string-append
               (indent 1)
               "memcpy((void*) &ADDR("
               (to-string *sym-tab-location*)
               "), (void*) sym_table, "
               (to-string (sym-table-size))
               " * WORD_SIZE);" nl)
              "Copy symbol table to machine memory")))

;Defines the symbol table end.
(define define-sym-table-head
  (lambda ()
    (string-append
     "#define sym_tab_head IND(" (to-string (+ *sym-tab-location* (sym-table-size))) ")" nl)))
;Loads the symbol table head to memory.
(define load-sym-table-head
  (lambda ()
    (document 1
              (string-append
               (indent 1)
               "MOV(sym_tab_head, IMM(" (if (> (sym-table-size) 0) (to-string *sym-tab-location*) "0") "));"
               nl)
              "'Head' of symbol table linked list")))
(define *sym-table-head-size* 1) ; Constant for size of "head" variable of the symbol table

;Getter: symbol table location.
(define *sym-tab-location* *stbl-entry-data-size*)
;Initialization of symbol table.
(define sym-tab (box '()))
;End location of symbol table.
(define sym-table-end
  (lambda ()
    (+ *sym-tab-location* (sym-table-size))))

;Defines the end of all the tables.
(define total-tables-end
  (lambda ()
    (+ (sym-table-end) *sym-table-head-size*)))

;Initializes all tables(free vars, constants and symbols).
(define static-analyze
  (lambda (pe)
    (cond ((null? pe) pe)
          ((not (pair? pe)) pe)
          ((type? pe 'const) (recursive-add-const (const->value pe)))
          ((type? pe 'fvar) (add-free-var (fvar->name pe)))
          (else (map static-analyze pe)))
    *void-object*))

;Code-gen main procedure.
(define code-gen
  (lambda (pe env params depth)
    (cond
      ((null? pe) "")
      ((type? pe 'seq) (code-gen-seq pe env params depth))
      ((type? pe 'if3) (code-gen-if3 pe env params depth))
      ((type? pe 'or) (code-gen-or pe env params depth))
      ((type? pe 'const) (code-gen-const pe env params depth))
      ((type? pe 'fvar) (code-gen-fvar pe env params depth))
      ((type? pe 'applic) (code-gen-applic pe env params depth))
      ((type? pe 'tc-applic) (code-gen-tc-applic pe env params depth))
      ((type? pe 'lambda-simple) (code-gen-lambda-simple pe env params depth))
      ((type? pe 'lambda-opt) (code-gen-lambda-opt pe env params depth))
      ((type? pe 'lambda-variadic) (code-gen-lambda-var pe env params depth))
      ((type? pe 'pvar) (code-gen-pvar pe env params depth))
      ((type? pe 'bvar) (code-gen-bvar pe env params depth))
      ((type? pe 'define) (code-gen-define pe env params depth))
      (else (error "code-gen" (format "~s" pe))))
    ))

;Prologue for the entire CISC code. a fixed code that has to be at the beginning of every CISC program.
(define prologue
  (lambda (output-name)
    (let ((-> (indent 1)))
      (string-append
       "/* " output-name nl
       " * Auto-generated file created by the compiler." nl
       " * " nl
       " * Programmer: You." nl
       " */" nl
       nl
       "#include <stdio.h>" nl
       "#include <stdlib.h>" nl
       "#include <string.h>" nl
       nl
       "/* Disable debug printouts (not that we used them anyway) */" nl
       "#define DO_SHOW 0" nl
       nl
       "#include \"cisc.h\"" nl
       "#include <lib/scheme/types.inc>" nl
       nl
       "#define SOB_VOID     1" nl
       "#define SOB_NIL      2" nl
       "#define SOB_FALSE    3" nl
       "#define SOB_TRUE     5" nl
       "#define undef        14" nl
       (define-sym-table-head) nl
       nl
       "int main()" nl
       "{" nl
       -> "START_MACHINE;" nl
       nl
       -> "/* Allocate memory for constants and free variables */" nl
       -> "PUSH(" (to-string (total-tables-end)) ");" nl
       -> "CALL(MALLOC);" nl
       -> "DROP(1);" nl
       -> "" nl
       -> "MOV(IND( 1), IMM(T_VOID));" nl
       -> "MOV(IND( 2), IMM(T_NIL));" nl
       -> "MOV(IND( 3), IMM(T_BOOL));" nl
       -> "MOV(IND( 4), IMM(0))" nl
       -> "MOV(IND( 5), IMM(T_BOOL));" nl
       -> "MOV(IND( 6), IMM(1));" nl
       -> "MOV(IND( 6), IMM(1));" nl
       -> "MOV(IND( 7), IMM(T_STRING));" nl
       -> "MOV(IND( 8), IMM(5));" nl
       -> "MOV(IND( 9), IMM('u'));" nl
       -> "MOV(IND(10), IMM('n'));" nl
       -> "MOV(IND(11), IMM('d'));" nl
       -> "MOV(IND(12), IMM('e'));" nl
       -> "MOV(IND(13), IMM('f'));" nl
       -> "MOV(IND(14), IMM(T_SYMBOL));" nl
       -> "MOV(IND(15), IMM(7));" nl
       -> "" nl
       -> "MOV(R0, IMM(SOB_VOID));" nl
       -> "" nl
       -> "JUMP(CONTINUE);" nl
       nl
       "#include \"io.lib\"" nl
       "#include \"system.lib\"" nl
       "#include \"scheme.lib\"" nl
       "#include \"extra.lib\"" nl
       nl
       "CONTINUE:" nl
       -> "MOV(R0, R0); /* To avoid an error of not having a statement after a label */" nl 
       "#include \"closures.asm\"" nl
       nl
       (define-const-table) nl
       (define-fvar-table) nl
       (define-sym-table) nl
       (load-const-table) nl
       (load-fvar-table) nl
       (load-sym-table) nl
       (load-sym-table-head) nl
       
       nl))))

;Epilogue for the entire CISC code. a fixed code that has to be at the end of every CISC program.
(define epilogue
  (let* ((> (indent 0))
         (-> (string-append > "\t")))
    (string-append
     -> "JUMP(Fin);" nl
     -> nl
     > "Lnot_proc:" nl
     -> "putchar('E');" nl
     -> "putchar('r');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar(':');" nl
     -> "putchar(' ');" nl
     -> "putchar('N');" nl
     -> "putchar('o');" nl
     -> "putchar('t');" nl
     -> "putchar(' ');" nl
     -> "putchar('a');" nl
     -> "putchar(' ');" nl
     -> "putchar('p');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('c');" nl
     -> "putchar('e');" nl
     -> "putchar('d');" nl
     -> "putchar('u');" nl
     -> "putchar('r');" nl
     -> "putchar('e');" nl
     -> "putchar('\\n');" nl
     -> "JUMP(Fin);" nl
     -> nl
     > "Lnot_pair:" nl
     -> "putchar('E');" nl
     -> "putchar('r');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar(':');" nl
     -> "putchar(' ');" nl
     -> "putchar('N');" nl
     -> "putchar('o');" nl
     -> "putchar('t');" nl
     -> "putchar(' ');" nl
     -> "putchar('a');" nl
     -> "putchar(' ');" nl
     -> "putchar('p');" nl
     -> "putchar('a');" nl
     -> "putchar('i');" nl
     -> "putchar('r');" nl
     -> "putchar('\\n');" nl
     -> "JUMP(Fin);" nl
     -> nl
     > "Lstack_overflow:" nl
     -> "putchar('E');" nl
     -> "putchar('r');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar(':');" nl
     -> "putchar(' ');" nl
     -> "putchar('S');" nl
     -> "putchar('t');" nl
     -> "putchar('a');" nl
     -> "putchar('c');" nl
     -> "putchar('k');" nl
     -> "putchar(' ');" nl
     -> "putchar('o');" nl
     -> "putchar('v');" nl
     -> "putchar('e');" nl
     -> "putchar('r');" nl
     -> "putchar('f');" nl
     -> "putchar('l');" nl
     -> "putchar('o');" nl
     -> "putchar('w');" nl
     -> "putchar('\\n');" nl
     -> "JUMP(Fin);" nl
     -> nl
     > "Lstack_underflow:" nl
     -> "putchar('E');" nl
     -> "putchar('r');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar(':');" nl
     -> "putchar(' ');" nl
     -> "putchar('S');" nl
     -> "putchar('t');" nl
     -> "putchar('a');" nl
     -> "putchar('c');" nl
     -> "putchar('k');" nl
     -> "putchar(' ');" nl
     -> "putchar('u');" nl
     -> "putchar('n');" nl
     -> "putchar('d');" nl
     -> "putchar('e');" nl
     -> "putchar('r');" nl
     -> "putchar('f');" nl
     -> "putchar('l');" nl
     -> "putchar('o');" nl
     -> "putchar('w');" nl
     -> "putchar('\\n');" nl
     -> "JUMP(Fin);" nl
     -> nl
     > "Lout_of_mem:" nl
     -> "putchar('E');" nl
     -> "putchar('r');" nl
     -> "putchar('r');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar(':');" nl
     -> "putchar(' ');" nl
     -> "putchar('O');" nl
     -> "putchar('u');" nl
     -> "putchar('t');" nl
     -> "putchar(' ');" nl
     -> "putchar('o');" nl
     -> "putchar('f');" nl
     -> "putchar(' ');" nl
     -> "putchar('m');" nl
     -> "putchar('e');" nl
     -> "putchar('m');" nl
     -> "putchar('o');" nl
     -> "putchar('r');" nl
     -> "putchar('y');" nl
     -> "putchar('\\n');" nl
     -> "JUMP(Fin);" nl
     -> nl
     > "Fin:" nl
     -> "STOP_MACHINE;" nl
     -> nl
     -> "return 0;" nl
     > "}" nl
     )))

(define ^label-print-result (^^label "LPrintResEnd"))

;Prints the result(value) of an expression.
(define print-result
  (lambda ()
    (let* ((print-label (^label-print-result))
           (doc (^doc 1 "Print evaluation result"))
           (pre-doc (doc->pre doc))
           (post-doc (doc->post doc))
           (-> (indent 1)))
      (begin
        (fwrite pre-doc)
        (fwrite -> "CMP(IND(R0), IMM(T_VOID));" nl)
        (fwrite -> "JUMP_EQ(" print-label ");" nl)
        (fwrite -> "PUSH(R0);" nl)
        (fwrite -> "CALL(WRITE_SOB);" nl)
        (fwrite -> "CALL(NEWLINE);" nl)
        (fwrite print-label ":" nl)
        (fwrite post-doc)
        ))))

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

;Gets the file name out of "require" expressions (of the form '(require "filename"))
(define require->filename cadr)

;Handles "require" expressions prior to the parsing
(define pre-process
  (lambda (sexprs)
    (if (or (null? sexprs) (not (pair? sexprs)))
        sexprs
        (let ((first (car sexprs)))
          (if (type? first 'require)
              (pre-process (append (file->sexprs (require->filename first)) (cdr sexprs)))
              (cons first (pre-process (cdr sexprs))))))))

;Main function - compiles the given source file into the given target file
(define compile-scheme-file
  (lambda (source-file target-file)
    (let ((code (pe->lex-pe (map (lambda (pe) (annotate-tc (parse pe))) (pre-process (cons '(require "support-code.scm") (file->sexprs source-file)))))))
      (begin
        (static-analyze code)
        (set-box! fvar-table (build-fvar-table (const-table-end)))
        (shift-sym-tab (+ (fvar-table-end) (sym-table-size) (- *stbl-entry-data-size*)))
        
        (fopen target-file)
        (fwrite (prologue target-file))
        (rmap (lambda (pe)
                (begin
                  (code-gen pe (new-env) (new-params) 1)
                  (print-result)))
              code)
        (fwrite epilogue)
        (fclose)
        ))))

;Initializes free variables.
(define free-vars
  (box '((cons prim_cons)
         (boolean? is_boolean)
         (+ prim_plus)
         (- prim_minus)
         (* prim_multiple)
         (/ prim_quotient)
         (< prim_less_than)
         (> prim_greater_than)
         (<= prim_less_equals)
         (>= prim_greater_equals)
         (= prim_numeric_equals)
         (car prim_car)
         (cdr prim_cdr)
         (apply prim_apply)
         (zero? prim_is_zero)
         (null? prim_is_null)
         (number? prim_is_number)
         (pair? prim_is_pair)
         (integer? prim_is_integer)
         (char? prim_is_char)
         (procedure? prim_is_procedure)
         (string? prim_is_string)
         (symbol? prim_is_symbol)
         (vector? prim_is_vector)
         (void? prim_is_void)
         (set-car! prim_set_car)
         (set-cdr! prim_set_cdr)
         (string-length prim_string_length)
         (string-ref prim_string_ref)
         (string-set! prim_string_set)
         (char->integer prim_char_to_int)
         (integer->char prim_int_to_char)
         (string->list prim_string_to_list)
         (vector-length prim_vector_length)
         (vector-ref prim_vector_ref)
         (vector-set! prim_vector_set)
         (remainder prim_remainder)
         (make-vector prim_make_vector)
         (make-string prim_make_string)
         (symbol->string prim_symbol_to_string)
         (string->symbol prim_string_to_symbol)
         (eq? prim_is_eq)
         )))
