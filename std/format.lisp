;; Split string into sections by {}. Print each section followed by
;; its corresponding item in the argument list.

(import 'std)
(import 'listutils)

;; Printf in Lust!
(let printf (fn (lst & args)
		 (when lst
		   (do
		    (let sections (split lst "{}"))
		    (let helper (fn (split args)
				     (when split
				       (do
					(let s (car split))
					(when s (print s))
					(let a (car args))
					(when a (print a))
					(helper (cdr split) (cdr args))))))
		    (helper sections args)
		    (print "\n")))))
