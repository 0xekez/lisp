;; A simple recursive fib function with a twist that shows the return
;; values of let expressions at work.

(let one (let fib (fn (n)
	     (if (lt n 2)
		 n
	       (add (one (sub n 2)) (fib (sub n 1)))))))

(fib 10)
