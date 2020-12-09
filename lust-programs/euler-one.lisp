;; Program to solve the first project euler problem. This doesn't make
;; much of an effort to be efficent.

(import 'std)
(import 'mod)
(import 'format)

(let 'range (fn (start end)
		(cond
		 ((eq start end) ())
		 ((lt start end) (cons start (range (add start 1) end)))
		 ((gt start end) (cons start (range (sub start 1) end))))))

(let 'filter-sum (fn (items pred)
		     (sum (filter items pred))))

(let 'pred (fn (n)
	       (cond
		((eq 0 (mod n 3)) #t)
		((eq 0 (mod n 5)) #t)
		(#t ()))))

(let 'answer (filter-sum (range 1 1000) pred))

(printf "The sum of numbers divisible by 3 & 5 less than 1000 is {}" answer)
