(load "pattern-matcher.scm")

(define simple-const? 
	(lambda (exp)
		(or (number? exp)
			(null? exp)
			(boolean? exp)
			(vector? exp)
			(char? exp)
			(string? exp) )))


(define var?
	(lambda (x)
		(and (symbol? x)
			 (not (member x '(and begin cond define do else if lambda 
					  	     let let* letrec or quasiquote unquote
					 	     unquote-splicing quote set!))))))
;still have a problem when number appear at the beginning

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      ;;;TODO: var?
			(else 
				(identify-lambda (cdr argl)
								 (lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
							     (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
						    	 (lambda (var) (ret-opt `(,(car argl)) var)))))))

(define parse
	(let ((run (compose-patterns
					;quote
					(pattern-rule
						`(quote ,(? 'c))
						(lambda (c) `(const ,c)))

					;const
					(pattern-rule
						(? 'c simple-const?)
						(lambda (c) `(const ,c)))

					;if2
					(pattern-rule
						`(if ,(? 'test) ,(? 'dit))
						(lambda (test dit) 
							`(if3 ,(parse test) ,(parse dit) (const ,(void)))))
					;if3
					(pattern-rule 
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif)
							`(if3 ,(parse test) ,(parse dit) ,(parse dif))))
					;or0
					(pattern-rule
						`(or)
						(lambda () #f))
					;or+
					(pattern-rule 
						`(or ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
							`(or ,(map parse
								 	   (cons expr exprs)))))

					;lambdas
					(pattern-rule
						`(lambda (? 'x) ,(? 'expr) .,(? 'exprs))
						(lambda (x expr exprs)
						`( ,(identify-lambda x  
											(lambda (s) `(simple ,s)) 
											(lambda (s opt) `((required ,s) (opt ,opt))) 
											(lambda (var) `(var ,var)))
							,(parse `(begin (cons expr exprs))))))

					; (pattern-rule
					; 	`(lambda () ,(? 'expr).,(? 'exprs))
					; 	(lambda (expr exprs)
					; 		`(lambda-simple () 
					; 			,(parse `(begin (cons expr exprs))))))
					; (pattern-rule
					; 	`(lambda ( ,(? 'param) .,(? 'params)) ,(? 'expr).,(? 'exprs))
					; 	(lambda (param params expr exprs)
					; 		`(lambda-simple ( ,param ,@params ) 
					; 			,(parse `(begin (cons expr exprs))))))

					; (pattern-rule
					; 	`(lambda ( (,(? 'param) .,(? 'params)) . ,(? 'rest)) ,(? 'expr).,(? 'exprs))
					; 	(lambda (param params rest expr exprs)
					; 		`(lambda-simple ( ,param ,@params) 
					; 			,(parse `(begin (cons expr exprs))))))

					; (pattern-rule
					; 	`(lambda ,(? 'args) ,(? 'expr).,(? 'exprs))
					; 	(lambda (args expr exprs)
					; 		`(lambda-var ,args 
					; 			,(parse `(begin (cons expr exprs))))))

					;define
					(pattern-rule	
						`(define ,(? 'var var?) ,(? 'expr))
						(lambda (var expr)
							`(def (var ,var) ,(parse expr))))
					;defineMitFunc
					(pattern-rule
						`(define (,(? 'var var?) ,(? 'argsl)) ,(? 'expr) . ,(? 'exprs))
						(lambda (var expr)
							`(def (var ,var) ,(parse expr))))


					;sequence
					(pattern-rule
						`(seq ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
							`(seq ,(map parse (cons expr exprs) ))))

					;var
					(pattern-rule
					 	(? 'v var?)				;forum question about "var?"
					 	(lambda (v) `(var ,v)))	
					)))
		(lambda (sexpr)
			(run sexpr (lambda () 'fail!)))))


;  (load "compiler.scm")

 ; (parse '(lambda args a b c))
