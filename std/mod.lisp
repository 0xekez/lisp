(import 'std)

(let 'floor (fn (n)
		(do
                 (let 'helper (fn (n cmp accum step)
				  (if (cmp n 0)
                                      (sub accum step)
				    (helper (sub n step) cmp (add accum step) step))))
                 (if (eq n 0)
                     0
                   (if (lt n 0)
                       (helper n gt 0 -1)
                     (helper n lt 0 1))))))

(let 'abs (fn (n) (if (lt n 0) (sub 0 n) n)))

(let 'mod (fn (a b)
	      (sub a (mul b (floor (div a b))))))
