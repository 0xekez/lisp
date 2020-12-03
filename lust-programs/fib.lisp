(import 'std)

(letq fib (fn (n)
	      (if (lt n 2)
		  n
		(add
		 (fib (sub n 2))
		 (fib (sub n 1))))))

(println (fib 20))
