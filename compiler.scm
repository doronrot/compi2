(load "pattern-matcher.scm")

(define simple-const? 
	(lambda (exp)
		(or (number? exp)
			(null? exp)
			(boolean? exp)
			(vector? exp)
			(char? exp)
			(string? exp) )))

(define not-reserved?
	(lambda (x)
		(not(member x '(and begin cond define do else if lambda 
					  	     let let* letrec or quasiquote unquote
					 	     unquote-splicing quote set!)))))

;TODO: still have a problem when number appears at the beginning
(define var?
	(lambda (x)
		(and (symbol? x)
			 (not-reserved? x))))

; (define is-car-var?
; 	(lambda (x)
; 		(if (not (list? x))
; 			#f
; 			(var? (car x)))))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))
                                
(define parse
	(let ((run (compose-patterns

					;Constants;
					(pattern-rule
						`(quote ,(? 'c))
						(lambda (c) `(const ,c)))
					(pattern-rule
						(? 'c simple-const?)
						(lambda (c) `(const ,c)))

					;Conditionals;
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit))
						(lambda (test dit) 
							`(if3 ,(parse test) ,(parse dit) (const ,(void)))))
					(pattern-rule 
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif)
							`(if3 ,(parse test) ,(parse dit) ,(parse dif))))
					
					;Disjunctions;
					(pattern-rule
						`(or)
						(lambda () #f))
					(pattern-rule 
						`(or ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
							`(or ,(map parse
								 	   (cons expr exprs)))))
                                        
                    ;Lambda forms;       
					(pattern-rule
						`(lambda ,(? 'argl) ,(? 'expr).,(? 'exprs))
						(lambda (argl expr exprs)
							(identify-lambda    argl 
                                                (lambda (s) `(lambda-simple ,s ,(parse `(begin ,@(cons expr exprs)))))
                                                (lambda (s opt) `(lambda-opt ,s ,opt ,(parse `(begin ,@(cons expr exprs)))))
                                                (lambda (var) `(lambda-var ,var ,(parse `(begin ,@(cons expr exprs))))))))
                
		    		;Define;
					(pattern-rule	
						`(define ,(? 'var var?) ,(? 'expr))
						(lambda (var expr)
							`(def (var ,var) ,(parse expr))))
					
					 ;TODO: check if is-car-var? is needed for var_args.
					 (pattern-rule
					 	`(define ,(? 'var_args) ,(? 'expr) . ,(? 'exprs))
					 	(lambda (var_args expr exprs)
					 		`(def (var ,(car var_args)) ,(parse `(lambda ,(cdr var_args) ,expr ,@exprs)))))


 					;Assignments;
				    (pattern-rule
					 	`( set! ,(? 'v) ,(? 'pexpr))
					 	(lambda (v pexpr)
							`(set (var ,v) ,(parse pexpr)))) 

				    ;Applications;
				    (pattern-rule
					 	`( ,(? 'expr not-reserved?) .,(? 'exprs))
					 	(lambda (expr exprs)

							`(applic ,(parse expr) ,(map parse exprs))))
		    		
		    		;Sequences;
					(pattern-rule
						`(begin)
						 (lambda () `(const ,(void))))
					(pattern-rule
						`(begin ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
                            (if (null? exprs)
                                (parse expr)
							   `(seq ,(map parse (cons expr exprs) )))))
                     
					;Variables;
					;TODO: forum question about "var?"
					(pattern-rule
					 	(? 'v var?)				
					 	(lambda (v) `(var ,v))))	
					))
				
		(lambda (sexpr)
			(run sexpr (lambda () 'fail!)))))
