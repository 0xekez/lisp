(let last (fn (list)
	      (if (eq list ())
		  ()
		  (if (eq (cdr list) ())
		      (car list)
		      (last (cdr list))))))

(let do (fn (& args)
	    (last args)))

(do
 (let a (add1 10))
 a)
