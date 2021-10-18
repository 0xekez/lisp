(let #t (eq 1 1))
(let #f (eq 0 1))

(let divides? (fn (d n)
		 (let helper (fn (n c)
				 (if (gt c n)
				     #f
				     (if (eq c n)
					 #t
					 (helper n (add c d))))))
		 (helper n d)))

(let prime? (fn (n)
		(let helper (fn (n i)
		     (if (eq i n)
			 #t
			 (if (divides? i n)
			     #f
			     (helper n (add1 i))))))
	        (if (lt n 2)
		    #f
		    (helper n 2))))



(println (prime? 967))
