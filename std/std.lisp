;; True evaluates to true
(let '#t '#t)
;; The only false value is the empty list. For convience #f evaluates
;; to that.
(let '#f ())

;; Quoted versions of let and let using Lust's quaziquote syntax.
(let 'letq (macro (symbol value) `(let ',symbol ,value)))

;; Folds a list of values with a function and accumulator.
(let 'fold (fn (func list accum)
	       (if (eq list ())
		   accum
		 (fold func (cdr list) (func accum (car list))))))

;; Get's the length of a list
(let 'len (fn (list)
	      (fold (fn (a i) (add a 1)) list 0)))

;; Calculates the sum of a list.
(let 'sum (fn (list)
	      (fold (fn (a i) (add a i)) list 0)))

;; Adds a list of numbers.
(let '+ (fn (& args)
	    (sum args)))

;; Subtracts a list of numbers from the first argument.
(let '- (fn (first & args)
	    (sub first (sum args))))

;; Multiplies a list of numbers together.
(let '* (fn (& args)
	    (fold (fn (a i) (mul a i)) args 1)))

;; Divides a list of numbers taking the first one and dividing by the
;; second and removing the second until there are no more items.
(let '/ (fn (& args)
	    (fold (fn (a i) (div a i)) (cdr args) (car args))))

;; Logical and of two arguments.
(let 'and (fn (a b)
	      (if a
		  (if b
		      #t
		    #f)
		#f)))
;; Logical or of two arguments
(let 'or (fn (a b)
	     (if a
		 #t
	       (if b
		   #t
		 #f))))

(let 'not (fn (a)
	      (if a #f #t)))

;; Returns a list which is the result of calling OP on each item in
;; LIST.
(let 'map (fn (op list)
	      (if list
		  (cons (op (car list)) (map op (cdr list)))
		())))

;; Return's true if all items in the list evaluate to true.
(let 'all (fn (list)
	      (if (eq list ())
		  #t
		(if (car list)
		    (all (cdr list))
		  ()))))

;; Return's true if any items in the list evaluate to true.
(let 'any (fn (list)
	      (if (eq list ())
		  #f
		(if (car list)
		    #t
		  (any (cdr list))))))

;; Returns true if all of its arguments are true.
(let '&& (fn (& args) (all args)))

;; Returns true if any if its arguments are true.
(let '|| (fn (& args) (any args)))

(let '== (fn (first second & rest)
	     (if (eq first second)
		 (all (map (fn (i) (eq i second)) rest))
	       #f)))

;; Converts its arguments into a list.
(let 'list (fn (& args) args))

;; Appends ITEM to L.
(let 'append (fn (l item)
		 (if (eq l ())
		     (list item)
		   (cons (car l) (append (cdr l) item)))))

;; Concatinates two lists.
(let 'concat (fn (l r)
		 (if (eq r ())
		     l
		   (concat (append l (car r)) (cdr r)))))

;; Returns the last item in a list.
(let 'last (fn (l)
	       (if (eq (cdr l) ())
		   (car l)
		 (last (cdr l)))))

;; Evaluates each argument in order and then returns the result of
;; evaluating the last one.
(let 'do (fn (& ops) (last ops)))

;; When COND is true executes and returns BODY's value.
;; WARNING: I beleive that this is broken
(letq when (macro (cond body) `(if ,cond ,body ())))

;; Returns a list containing the elements in ITEMS that PRED returns
;; true for.
(letq filter (fn (items pred)
		 (if items
		   (do
		    (letq first (car items))
		    (if (pred first)
			(cons first (filter (cdr items) pred))
		      (filter (cdr items) pred)))
		   ())))

;; Generates a list of values in range [start, end)
(let 'range (fn (start end)
                (do
                 (let 'mover (if (lt start end) sub add))
                 (let 'helper (fn (start end accum)
                                  (if (eq start end)
                                      accum
                                    (helper start (mover end 1) (cons end accum)))))
                 (helper (sub start 1) (sub end 1) ()))))


;; Much like Scheme's condition. Takes a list of pairs where the first
;; argument and the second is an expression to evaluate if the
;; argument is true. Iterates over that list and return's the body of
;; the first one that evaluates to true. Optionally there can be a
;; special pair with the form (else body) that must appear in the
;; terminal position. If no other pairs evaluate to true that arm will
;; run.
(let 'cond (macro (& cases)
		  `(condlist ,cases)))

(let 'condlist (macro (items)
		      (do
		       (let 'rest (cdr items))
		       (if items
			   `(if ,(car (car items))
				,(car (cdr (car items)))
			      (condlist ,rest))
			()))))

;; `define` special form with the same form as scheme's. Allows for
;; defining functions and variables as follows:
;;
;;   lust> (define one 1)
;;   => 1
;;   lust> (define I (x) x)
;;   => (fn (x) x)
;;
;; Demonstrates use of the quaziquote form, cond, and errors.
(let 'define (macro (symbol & args)
     	     (cond
		((eq (len args) 1) `(letq ,symbol ,(car args)))
		((eq (len args) 2) `(letq ,symbol ,`(fn ,(car args) ,(car (cdr args)))))
		(else (error "wrong number of arguments for define macro")))))

(letq char (macro (s)
      	   `(if (eq (len ,s) 1)
	       (car ,s)
	     (error "can not convert to char"))))
