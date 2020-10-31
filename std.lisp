(set '#t '#t)
(set '#f ())

(set 'sub (fn (l r) (add l (negate r))))
(set 'gt (fn (a b)
	     (lt (sub b a) 0)))

(set 'sum (fn (a) (if (eq (first a) ()) 0 (add (first a) (sum (rest a))))))
