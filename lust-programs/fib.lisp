(import 'std)
(import 'format)

(let fib (fn (n)
	      (do
	       (let fibtail (fn (n a b)
				 (cond
				  ((eq n 0) a)
				  ((eq n 1) b)
				  (#t (fibtail (sub n 1) b (add a b))))))
	       (fibtail n 0 1))))

(let n 40)

(printf "fib({}) => {}\n" n (fib n))
