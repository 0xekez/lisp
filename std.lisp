;; True evaluates to true
(set '#t '#t)
;; The only false value is the empty list. For convience #f evaluates
;; to that.
(set '#f ())

;; Folds a list of values with a function and accumulator.
(set 'fold (fn (func list accum)
	       (if (eq list ())
		   accum
		 (fold func (cdr list) (func accum (car list))))))

;; Calculates the sum of a list.
(set 'sum (fn (list)
	      (fold (fn (a i) (add a i)) list 0)))

;; Adds a list of numbers.
(set '+ (fn (& args)
	    (sum args)))

;; Subtracts a list of numbers from the first argument.
(set '- (fn (first & args)
	    (sub first (sum args))))

;; Multiplies a list of numbers together.
(set '* (fn (& args)
	    (fold (fn (a i) (mul a i)) args 1)))

;; Divides a list of numbers taking the first one and dividing by the
;; second and removing the second until there are no more items.
(set '/ (fn (& args)
	    (fold (fn (a i) (div a i)) (cdr args) (car args))))

;; Logical and of two arguments.
(set 'and (fn (a b)
	      (if a
		  (if b
		      #t
		    #f)
		#f)))

;; Returns a list which is the result of calling OP on each item in
;; LIST.
(set 'map (fn (op list)
	      (if list
		  (cons (op (car list)) (map op (cdr list)))
		())))

;; Return's true if all items in the list evaluate to true.
(set 'all (fn (list)
	      (if (eq list ())
		  #t
		(if (car list)
		    (all (cdr list))
		  ()))))

;; Return's true if any items in the list evaluate to true.
(set 'any (fn (list)
	      (if (eq list ())
		  #f
		(if (car list)
		    #t
		  (any (cdr list))))))

;; Returns true if all of its arguments are true.
(set '&& (fn (& args) (all args)))

;; Returns true if any if its arguments are true.
(set '|| (fn (& args) (any args)))

(set '== (fn (first second & rest)
	     (if (eq first second)
		 (all (map (fn (i) (eq i second)) rest))
	       #f)))

;; Converts its arguments into a list.
(set 'list (fn (& args) args))

;; Appends ITEM to L.
(set 'append (fn (l item)
		 (if (eq l ())
		     (list item)
		   (cons (car l) (append (cdr l) item)))))

;; Concatinates two lists.
(set 'concat (fn (l r)
		 (if (eq r ())
		     l
		   (concat (append l (car r)) (cdr r)))))

;; Returns the last item in a list.
(set 'last (fn (l)
	       (if (eq (cdr l) ())
		   (car l)
		 (last (cdr l)))))

;; Evaluates each argument in order and then returns the result of
;; evaluating the last one.
(set 'do (fn (& ops) (last ops)))

;; When COND is true executes and returns BODY's value.
(set 'when (macro (cond body) (list 'if cond body ())))

;; Much like Scheme's condition. Takes a list of pairs where the first
;; argument and the second is an expression to evaluate if the
;; argument is true. Iterates over that list and return's the body of
;; the first one that evaluates to true. Optionally there can be a
;; special pair with the form (else body) that must appear in the
;; terminal position. If no other pairs evaluate to true that arm will
;; run.
(set 'cond (macro (& cases)
		  (do
		   (let 'arm-matches (fn (arm)
					 (if (eval (car arm))
					     (cdr arm)
					   #f)))
		   (let 'is-else-arm (fn (arm)
					 (eq (car arm) 'else)))
		   (let 'find-match (fn (cases)
					(if cases
					    (if (is-else-arm (car cases))
						(if (cdr cases)
						    (error '"else branch in non-terminal position")
						  (car (cdr (car cases))))
					      (if (arm-matches (car cases))
						  (car (cdr (car cases)))
						(find-match (cdr cases))))
					  ())))
		   (find-match cases))))
