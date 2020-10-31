(set '#t '#t)
(set '#f ())

(set 'sub (fn (l r) (add l (negate r))))
(set 'gt (fn (a b)
	     (lt (sub b a) 0)))
