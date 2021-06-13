;; A simple recursive fib function with a twist that shows the return
;; values of let expressions at work.

(let fib (fn (n)
	     (if (lt n 2)
		 n
	       (add (fib (sub n 2)) (fib (sub n 1))))))

(fib 25)
