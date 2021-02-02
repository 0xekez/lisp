(let foo (fn (a b)
	     (let bar (add a b))
	     ((fn (c) (add c 1)) bar)))
(foo 1 2)
