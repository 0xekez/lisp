;; This demonstrates the semantics of Lust's let expressions in more
;; detail than most people probably care about.

(let fib 10)
(set fib (fn (n)
	     (if (lt n 2)
		 n
	       (add (fib (sub n 2)) (fib (sub n 1))))))
(fib 10)
