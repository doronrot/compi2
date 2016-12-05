;;; pattern matcher

(define with (lambda (s f) (apply f s)))


(define match
	(lambda (pat e)
		(cond ((and (pair? pat) (pair? e))
				(append (match (car pat) (car e))
						(match (car pat) (car e))))
			   ((and (vector? pat) (vector? e)
					   (= (vector-length pat) (vector-length e)))
				(match (vector->lost pat) (vector->list e)))
			   ((procedure? pat (list e))
			   ((and (symbol? pat) (symbol? e) (eq? pat e)) '())
			   (else (error 'match (format "Not yet: ~s" e)))))))

> (match '(a b c) '(a b c))
()
> (match `(a ,cos c) '(a b c))
(b)
> (match `(a ,cos ,log) '(a b c))
(b c)
> (match `(cond ((zero? n) one) (else ,list)) '(a b c) '(cond ((zero? n) one) (else (* n (fact (- n 1))))))
((* n (fact (- n 1))))

(define match
	(letrec ((match
				(lambda (pat e ret-dict ret-no-match)
					(cond ((and (pair? pat) (pair? e))
							(match (car pat) (car e)
									(lambda (car-dict)
										(match (cdr pat) (cdr e)
												(lambda (cdr-dict) (ret-dict (append car-dict cdr-dict)))
												ret-no-match))
									ret-no-match))
						   ((and (vector? pat) (vector? e)
								   (= (vector-length pat) (vector-length e)))
							(match (vector->lost pat) (vector->list e) ret-dict ret-no-match))
						   ;;match with unification
						   ((procedure? pat)
						   	(if (pat e)
						   		(ret-dict `(,e))
						   		(ret-no-match)))
						   ((or (and (char? pat) (char? e) (char=? pat e))
						   		(and (string? pat) (string? e) (string=? pat e))
						   		(and (symbol? pat) (symbol? e) (eq? pat e))
						   		(and (number? pat) (number? e) (= pat e))
						   		(eq? pat e))
						   (ret-dict '()))
						   (else (ret-no-match)))))))
		(lambda (pat e ret-match ret-no-match)
			(match pat e (lambda (dict) (apply ret-match dict)) ret-no-match)))

(define ?
	(lambda (name . guards)
		(lambda (e)
			(andmap (lambda (pred?) (pred? e)) guards))))

;andmap returns #t or #f
;? returns procedure that returns #t or #f => line 41-42 (match)					


> (match `(lambda ,cos . ,tan) '(lambda (a b c) (set! a b) (set! a (* a b c)) (list a b c)) (lambda (dict) dict) (lambda () 'no-match))
((a b c) ((set! a b) (set! a (* a b c)) (list a b c)))
> (match `((a ,(? 'number1)) (b ,(? number2))) '((a 4) (b 6)) (lambda (dict) dict) (lambda () 'no-match))
(4 6)
> (match `((a ,(? 'number1 odd?)) (b ,(? number2))) '((a 4) (b 6)) (lambda (dict) dict) (lambda () 'no-match))
no-match
> (match `((a ,(? 'number1 odd?)) (b ,(? number2))) '((a 41) (b 6)) (lambda (dict) dict) (lambda () 'no-match))
(41 6)
> (match `((a ,(? 'number1 odd? positive?)) (b ,(? number2))) '((a 41) (b 6)) (lambda (dict) dict) (lambda () 'no-match))
(41 6)
> (match `((a ,(? 'number1 odd? positive?)) (b ,(? number2))) '((a 41) (b 6)) (lambda (number1 number2) `((number1: ,number1) (number2: ,number2))) (lambda () 'no-match))
((number1: 41) (number2: 6))
> (match `(define ,(? 'name symbol?) ,(? 'expr)) '(define foo 3) (lambda (name expr) #t) (lambda () 'no-match))
#t
> (match `(define ,(? 'name symbol?) ,(? 'expr)) '(define foo 3) (lambda (name expr) `((name ,name) (expr ,expr))) (lambda () 'no-match))
((name foo) (expr 3))
> (match `(define ,(? 'name symbol?) ,(? 'expr)) '(define "moshe" 3) (lambda (name expr) `((name ,name) (expr ,expr))) (lambda () 'no-match))
no-match
> (match `(begin . ,(? 'exprs)) '(begin a b c d) (lambda (exprs) exprs) (lambda () 'no-match))
(a b c d)
> (match `(begin . ,(? 'exprs)) '(begin a b c . d) (lambda (exprs) exprs) (lambda () 'no-match))
(a b c . d)
> (match `(begin . ,(? 'exprs list?)) '(begin a b c . d) (lambda (exprs) exprs) (lambda () 'no-match))
no-match
> (match `(begin ,(? expr1) . ,(? 'exprs list?)) '(begin a b c . d) (lambda (expr1 exprs) `(,expr1 ,@exprs)) (lambda () 'no-match))
no-match
> (match `(begin ,(? expr1) . ,(? 'exprs list?)) '(begin a b c d) (lambda (expr1 exprs) `(,expr1 ,@exprs)) (lambda () 'no-match))
(a b c d)
> (match `(begin . ,(? exprs pair? list?)) '(begin) (lambda (exprs) exprs) (lambda () 'no-match))
no-match
> (match `(begin . ,(? exprs pair? list?)) '(begin a) (lambda (exprs) exprs) (lambda () 'no-match))
(a)


;;; tag parser

(define parse
	(let ((run 
			(compose-patterns
				(pattern-rule
					(? 'c simple-const?)
					(lambda (c) `(const ,c)))
				(pattern-rule
					`(quote ,(? 'c))
					(lambda (c) `(const ,c)))
				(pattern-rule
					(? 'v var?)
					(lambda (v) `(var ,v)))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit))
					(lambda (test dit) `(if ,(parse test) ,(parse dit) (const ,*void-object*))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if ,(parse test) ,(parse dit) ,(parse dif))))
				;;let*
				(pattern-rule
					`(let* () ,(? 'expr) . ,(? 'exprs list?))
					(lambda (expr exprs) (parse (beginify (cons expr exprs)))))
				(pattern-rule
					`(let* ((,(? 'var var?) ,(? val?)) . ,(? 'rest)) . ,(? 'exprs))
					(lambda (var val rest exprs) (parse `(let ((,var val)) (let* ,rest . ,exprs)))))
				;; add more rules here
				)))
			(lambda (e)
				(run e
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))

(define beginify
	(lambda (s)
		(cond
			((null? s) *void-object*)
			((null? (cdr s)) (cdr s))
			(else `(begin ,@s)))))


;;; identifying lambdas

`(lambda ,(? 'expr1) . ,(? 'exprs list?))
`(lambda ,(? 'argl) ,(? 'exprs pair? list?))

argl = (a b c d)
argl = (a b c . d)
argl = v

(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl))      ;;;TODO: var?
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) ;simple
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) ;opt
					(lambda (var) (ret-opt `(,(car argl)) var)))))))

> (identify-lambda '(a b c) (lambda (s) `(simple ,s)) (lambda (s opt) `((required ,s) (opt ,opt))) (lambda (var) `(var ,var)))
(simple (a b c))

> (identify-lambda '(a b c d e . f) (lambda (s) `(simple ,s)) (lambda (s opt) `((required ,s) (opt ,opt))) (lambda (var) `(var ,var)))
((required (a b c d e) (opt f))

> (identify-lambda 'moshe (lambda (s) `(simple ,s)) (lambda (s opt) `((required ,s) (opt ,opt))) (lambda (var) `(var ,var)))
(var moshe)
