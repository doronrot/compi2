(load "pattern-matcher.scm")
(load "qq.scm")


;TBD: 
; is fail (ours) == error(meir)?
; begin
; VAR - still have a problem when number appears at the beginning


(define simple-const? 
	(lambda (exp)
		(or (number? exp)
			;(null? exp)
			(boolean? exp)
			(vector? exp)
			(char? exp)
			(string? exp) )))

(define not-reserved?
	(lambda (x)
		(not(member x '(and begin cond define do else if lambda 
					  	     let let* letrec or quasiquote unquote
					 	     unquote-splicing quote set!)))))
(define cons_false 
	(lambda (var)
		(cons var (list #f))))

(define cons_set
	(lambda (pair)
	 	(cons 'set! pair)))

;; (define flat_begins
;; 	(letrec ((flabe (lambda (lst)
;; 						(if (null? lst)
;; 							;return what?
;; 							(if (equal? (caar lst) 'begin)
;; 								(cdr lst) 	; begin removed, but expressions are in parenthesis
;; 								(list lst)	; so that the parenthesis will be as above
;; 								)))))
;; 		(flabe lst)))
						 ;    		;Sequences;

					; (pattern-rule
					; 	`(begin ,(? 'expr).,(? 'exprs))
					; 	(lambda (expr exprs)
     ;                        (if (null? exprs)
     ;                            (parse-2 expr)
					; 		   `(seq ,(map parse-2 
					; 		   			   (map flat_begins 
					; 		   			   		(cons expr exprs)))) )))


; (define first_el_qq?
; 	(lambda (lst)
; 		(if (not (list? lst))
; 			#f
; 			(list lst))))


; (pattern-rule
; `(quasiquote . ,(? 'args))
(define till_point
	(lambda (lst new_lst)
		(if (pair? lst)
			(till_point (cdr lst)
						(append new_lst (list (car lst))))
			new_lst)))
			

(define till_point_help
	(lambda (lst)
		(till_point lst (list))))


(define no-duplications?
	(lambda (lst)
		(cond
			;you are improper list if: 
			((and (not (list? lst)) (pair? lst)) (no-duplications? (till_point_help lst)))
			;you are not a list at all if:
			((not (pair? lst)) #t)
			;else, you are probably a list:
			((null? lst) #t) 
      		((member (car lst) (cdr lst)) #f)
      		(else (no-duplications? (cdr lst)) ))))

;TODO: still have a problem when number appears at the beginning
(define var?
	(lambda (x)
		(and (symbol? x)
			 (not-reserved? x))))

(define is-car-var?
	(lambda (x)
		(if (not (pair? x))
			#f
			(var? (car x)))))

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))
                                
(define parse-2
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
							`(if3 ,(parse-2 test) ,(parse-2 dit) (const ,(void)))))
					(pattern-rule 
						`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
						(lambda (test dit dif)
							`(if3 ,(parse-2 test) ,(parse-2 dit) ,(parse-2 dif))))
					
					;Disjunctions;
					(pattern-rule
						`(or)
						(lambda () (parse-2 #f)))
					(pattern-rule 
						`(or ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
							(if (null? exprs)
								(parse-2 expr)
								`(or ,(map parse-2
									 	   (cons expr exprs))))))
                                        
                    ;Lambda forms;       
					(pattern-rule
						`(lambda ,(? 'argl no-duplications?) ,(? 'expr).,(? 'exprs))
						(lambda (argl expr exprs)
							(identify-lambda    argl 
                                                (lambda (s) `(lambda-simple ,s ,(parse-2 `(begin ,@(cons expr exprs)))))
                                                (lambda (s opt) `(lambda-opt ,s ,opt ,(parse-2 `(begin ,@(cons expr exprs)))))
                                                (lambda (var) `(lambda-var ,var ,(parse-2 `(begin ,@(cons expr exprs))))))))
                
		    		;Define;
					(pattern-rule	
						`(define ,(? 'var var?) ,(? 'expr) .,(? 'exprs))
						(lambda (var expr exprs)
							`(def (var ,var) ,(parse-2 `(begin ,@(cons expr exprs))))))
					
					 ;Define MIT;
					 (pattern-rule
					 	`(define ,(? 'var_args is-car-var?) ,(? 'expr) . ,(? 'exprs))
					 	(lambda (var_args expr exprs)
					 		`(def (var ,(car var_args)) ,(parse-2 `(lambda ,(cdr var_args) ,expr ,@exprs)))))


 					;Assignments;
				    (pattern-rule
					 	`(set! ,(? 'v var?) ,(? 'pexpr))
					 	(lambda (v pexpr)
							`(set (var ,v) ,(parse-2 pexpr)))) 

				    ;Applications;
				    (pattern-rule
					 	`( ,(? 'expr not-reserved?) .,(? 'exprs))
					 	(lambda (expr exprs)
							`(applic ,(parse-2 expr) ,(map parse-2 exprs))))
		    		
		    		;Sequences;
					(pattern-rule
						`(begin)
						 (lambda () `(const ,(void))))
					(pattern-rule
						`(begin ,(? 'expr).,(? 'exprs))
						(lambda (expr exprs)
                            (if (null? exprs)
                                (parse-2 expr)
							   `(seq ,(map parse-2 
							   			   (map flat_begins 
							   			   		(cons expr exprs)))) )))
                     
					;Variables;
					;TODO: forum question about "var?"
					(pattern-rule
					 	(? 'v var?)				
					 	(lambda (v) `(var ,v)))

					;;;;;;; Macro - Expansions ;;;;;;;;
					;And; 
					(pattern-rule
						`(and)
						(lambda () (parse-2 #t)))
					
					(pattern-rule
					 	`(and ,(? 'expr) .,(? 'exprs)) 				
					 	(lambda (expr exprs)
						    (if (null? exprs)
                                (parse-2 expr)
							    (parse-2 `(if ,expr (and ,@exprs) #f)))))

					;Cond;
					; (pattern-rule
					; 	`(cond)
					; 	(lambda () (parse-2 #f)))

					(pattern-rule
					 	`(cond ,(? 'expr pair?) .,(? 'exprs)) 				
					 	(lambda (expr exprs)
						    (if (null? exprs)
                                (if (equal? (car expr) 'else)
                                	(parse-2 `(begin ,@(cdr expr)))
                                	(parse-2 `(if ,(car expr) (begin ,@(cdr expr)))))
							 	(parse-2 `(if ,(car expr) (begin ,@(cdr expr)) (cond ,@exprs))))))

					;Let;
					(pattern-rule
						`(let ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(parse-2 `((lambda ,(map car pairs) ,@(cons body bodies))
									 ,@(map cadr pairs)))))

					;Let*;
					(pattern-rule
						`(let* ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(cond ((null? pairs)
									(parse-2 `(let () ,@(cons body bodies))))
								  ((null? (cdr pairs))
								  	(parse-2 `(let (,(car pairs)) ,@(cons body bodies))))
								  (else 
								    (parse-2 `(let (,(car pairs)) (let* ,(cdr pairs) ,@(cons body bodies))))))))
					;Letrec;
					(pattern-rule
						`(letrec ,(? 'pairs list?) ,(? 'body) .,(? 'bodies))
						(lambda (pairs body bodies)
							(parse-2 `(let ,(map cons_false (map car pairs))
										 ,@(map cons_set pairs)
										 ((lambda () ,@(cons body bodies)))))))

                                        ;QQ;
					 (pattern-rule
					 	`(quasiquote .,(? 'expr))
					 	(lambda (expr)
					 		(parse-2 (expand-qq (car expr)))))



					)))
				
		(lambda (sexpr)
			(run sexpr (lambda () 'fail!)))))
