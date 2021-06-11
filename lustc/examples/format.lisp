;; String formatting for Lust in Lust

(let #t (eq 1 1))
(let #f (eq 0 1))

(let list (fn (& args) args))
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

(let len (fn (list)
	     (if (eq list ())
		 0
		 (add 1 (len (cdr list))))))

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
		    (cons start (split rest wedge)))
		   ())))

(let append (fn (lst item)
		(if (null? lst)
		    (list item)
		    (cons (car lst) (append (cdr lst) item)))))

(let concat (fn (lst tail)
		(if (null? tail)
		    lst
		    (concat (append lst (car tail)) (cdr tail)))))

(let list-kinda-eq (fn (a b)
		 (if (not (eq (len a) (len b))) #f
		     (do
		      (let helper (fn (a b)
				      (if (null? a) #t
					  (if (eq (car a) (car b))
					      (helper (cdr a) (cdr b))
					      #f))))
		      (helper a b)))))

;; Without an apply primitive we can't use sprintf here.
(let printf (fn (lst & args)
		 (if (not (null? lst))
		   (do
		    (let sections (split lst "{}"))
		    (let helper (fn (split args)
				    (if (not (null? split))
					(do
					 (let split-next (if (not (null? split))
					     (do
					      (let s (car split))
					      (print s)
					      (cdr split))
					     ()))
					 (let args-next (if (not (null? args))
					     (do
					      (let a (car args))
					      (print a)
					      (cdr args))
					     ()))
					 (helper split-next args-next))
				       ())))
		    (helper sections args)
		    (print "\n"))
		   ())))

(let sprintf (fn (lst & args)
		 (if (not (null? lst))
		   (do
		    (let res ())
		    (let sections (split lst "{}"))
		    (let helper (fn (split args)
				    (if (not (null? split))
					(do
					 (let split-next (if (not (null? split))
					     (do
					      (let s (car split))
					      (set res (concat res s))
					      (cdr split))
					     ()))
					 (let args-next (if (not (null? args))
					     (do
					      (let a (car args))
					      (set res (concat res a))
					      (cdr args))
					     ()))
					 (helper split-next args-next))
				       ())))
		    (helper sections args)
		    res)
		   ())))

(println (sprintf "hello {} {}!" "zeke" "medley"))
(printf "hello {}!" "zeke")

;; This line just exists so that we can test these functions.
(list-kinda-eq (sprintf "hello there {}! your lucky number is {} :)" "zeke" "10") "hello there zeke! your lucky number is 10 :)")
