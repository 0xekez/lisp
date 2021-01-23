(let foo (fn (n)
	     (if (eq n 0)
		 42
	       ((fn (n) (foo (sub n 1))) n))))
(foo 10)
