;; Closures can mutate their captured variables with the set expression

(let make-point (fn (x y)
		    (let get (fn () (cons x y)))
		    (let set-x (fn (n) (set x n)))
		    (let set-y (fn (n) (set y n)))
		    (cons get
			  (cons set-x
				(cons set-y ())))))

(let point (make-point 10 10))
(let set-x (car (cdr point)))
(let get (car point))
(set-x 11)
(get)
