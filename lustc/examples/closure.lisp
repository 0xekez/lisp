(let make-adder (fn (base)
		    (fn (n) (add n base))))

(let add3 (make-adder 3))

(add3 39)
