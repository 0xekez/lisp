;; A memoized fib function

;; A map of known fib values
(let known-map (cons (cons 1 1)
		     (cons (cons 0 0) ())))

;; Lookup a value in the map given the key.
(let lookup (fn (n)
		(let search (fn (n map)
				(if (eq map ())
				    ()
				  (if (eq (car (car map)) n)
				      (cdr (car map))
				    (search n (cdr map))))))
		(search n known-map)))

;; Insert a value into the map
(let insert (fn (k v)
	     (set known-map (cons (cons k v) known-map))))

(let fib (fn (n)
	     (let compute-and-insert (fn (n)
					 (let v (add (fib (sub n 1)) (fib (sub n 2))))
					 (insert n v)
					 v))
	     (let v? (lookup n))
	     (if (eq v? ())
		 (compute-and-insert n)
	       v?)))

(fib 40)
