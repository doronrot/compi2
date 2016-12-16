(print-gensym #f)


(define cse
	(lambda (expr)
		(cse_core expr)))



 (define cse_core
 	(lambda (expr)
		(if (or (not (list? expr)) (null? expr)) 
			expr
 			(let ((first_applic (find_first_applic expr)))
 				(if (occur_again? first_applic expr)
 					(let* ((new_name (gensym))
 					  	  (new_list (replace_occurances first_applic expr new_name)))
 						(cse_core new_list))
 					(append (list (car expr))
 						    (cse_core (cdr expr))))))))



(define replace_occurances
	(lambda (first_applic orig_lst new_name)
		(if (or (null? orig_lst)(atom? orig_lst))
			orig_lst
			(if (equal? first_applic orig_lst)
				new_name
				(append (list (replace_occurances first_applic (car orig_lst) new_name))
					    (replace_occurances first_applic (cdr orig_lst) new_name))))))




(define occur_again?
	(lambda (first_applic orig_lst)
		(> (occur_again_help first_applic orig_lst) 1)))

(define occur_again_help
	(lambda (first_applic orig_lst)
			(if (or (null? orig_lst) (atom? orig_lst))
				0
				(if (equal? first_applic orig_lst)
					1
					(+ (occur_again_help first_applic (car orig_lst))
					   (occur_again_help first_applic (cdr orig_lst)))))))





;input: list (we check that before)
;output: the first expression which is application (no lambda)
(define find_first_applic
	(lambda (lst)
		(find_first_applic_help lst (list))))

(define find_first_applic_help
	(lambda (orig_lst acc)
		(if (null? orig_lst)
			acc
			(let ((first (car orig_lst))
				  (rest (cdr orig_lst)))
				(if (or (atom? first) 
					    (null? first)
					    (equal? (car first) 'quote)
					    (equal? first `'())
					    (and (equal? (car first) 'list)
					 		 (null? (cdr first))))
					(find_first_applic_help rest (append acc (list first)))
					(find_first_applic_help first (list)))))))
