;; Tokenizer for Lust in Lust

(let #t (eq 1 1))
(let #f (eq 0 1))

(let len (fn (list)
	     (if (eq list ())
		 0
		 (add 1 (len (cdr list))))))

(let last (fn (list)
	      (if (eq list ())
		  ()
		  (if (eq (cdr list) ())
		      (car list)
		      (last (cdr list))))))

(let do (fn (& args)
	    (last args)))

(let and (fn (a b)
	     (if a (if b #t #f) #f)))

(let list (fn (& args) args))

(let list-starts-with (fn (list pred)
			  (if (and (null? list) (not (null? pred))) #f
			      (if (eq pred ()) #t
				  (if (not (eq (car pred) (car list))) #f
				      (if (gt (len pred) (len list)) #f
					  (list-starts-with (cdr list) (cdr pred))))))))

(let remove-n (fn (list n)
		  (if (null? list) ()
		      (if (eq n 0) list
			  (remove-n (cdr list) (sub n 1))))))

(let split-until (fn (lst wedge)
		     (if (not (null? lst))
			 (if (list-starts-with lst wedge)
			     (list () (remove-n lst (len wedge)))
			     (do
			      (let res (split-until (cdr lst) wedge))
			      (if (null? res)
				  (list lst ())
				  (list (cons (car lst) (car res)) (car (cdr res))))))
			 ())))

(let split (fn (lst wedge)
	       (if (not (null? lst))
		   (do
		    (let res (split-until lst wedge))
		    (let start (car res))
		     (let rest (car (cdr res)))
		     (if (null? start)
			 (split rest wedge)
			 (cons start (split rest wedge))))
		   ())))

(let append (fn (lst item)
		(if (null? lst)
		    (list item)
		    (cons (car lst) (append (cdr lst) item)))))

(let concat (fn (lst tail)
		(if (null? tail)
		    lst
		    (concat (append lst (car tail)) (cdr tail)))))

(let replace (fn (str target replacement)
		 (if (null? str)
		     ()
		     (if (list-starts-with str target)
			 (concat replacement (remove-n str (len target)))
			 (cons (car str) (replace (cdr str) target replacement))))))

(let tokenize (fn (str)
		  (do
		   (let prep (fn (str)
				 (replace (replace str "(" " ( ") ")" " ) ")))
		   (split (prep str) " "))))

(println (tokenize "(add 1 a)"))

;; TESTING CODE:

(let strings-kinda-eq (fn (a b)
			  (if (null? a) #t
			      (if (not (eq (len a) (len b))) #f
				  (if (eq (car a) (car b))
				      (strings-kinda-eq (cdr a) (cdr b))
				      #f)))))

(let list-kinda-eq (fn (a b)
		 (if (not (eq (len a) (len b))) #f
		     (do
		      (let helper (fn (a b)
				      (if (null? a) #t
					  (if (strings-kinda-eq (car a) (car b))
					      (helper (cdr a) (cdr b))
					      #f))))
		      (helper a b)))))

(list-kinda-eq (tokenize "(add foo bar)") (list "(" "add" "foo" "bar" ")"))
