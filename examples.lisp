(set 'sub (fn (l r) (add l (negate r))))
(set 'gt (fn (a b)
	     (lt (sub b a) 0)))

(set 'fib (fn (n)
	      (if (lt n 2)
		  n
		(add (fib (sub n 1)) (fib (sub n 2))))))

(set 'factorial-accum (fn (n a) (if (lt n 0) a (factorial-accum (sub n 1) (add a n)))))
(set 'factorial (fn (n) (factorial-accum n 0)))

(set 'fib-tail (fn (a b n)
		   (if (gt n 0)
		       (fib-tail b (add a b) (sub n 1))
		     a)))
(set 'fib (fn (n) (fib-tail 0 1 n)))
