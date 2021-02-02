(set 'fib (fn (n)
	      (if (lt n 2)
		  n
		(add (fib (sub n 1)) (fib (sub n 2))))))
(println (fib 25))
