(set '#t '#t)
(set '#f ())
(set 'sub (fn (l r) (add l (negate r))))

(set 'fib (fn (n)
	      (if (lt n 2)
		  n
		(add (fib (sub n 1)) (fib (sub n 2))))))

(set 'factorial-accum (fn (n a) (if (lt n 0) a (factorial-accum (sub n 1) (add a n)))))
(set 'factorial (fn (n) (factorial-accum n 0)))
