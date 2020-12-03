;; Split string into sections by {}. Print each section followed by
;; its corresponding item in the argument list.

(import 'std)

;; Checks LIST starts with PRED
(let 'starts-with (fn (list pred)
		      (cond
		       ((eq pred ()) #t)
		       ((not (eq (car pred) (car list))) #f)
		       ((gt (len pred) (len list)) #f)
		       (#t (starts-with (cdr list) (cdr pred))))))

;; Removes the first N items from LST
(let 'remove-n (fn (lst n)
		   (when lst
		     (if (eq n 0)
			 lst
		       (remove-n (cdr lst) (sub n 1))))))

;; Splits LST into two parts the first containing all the items in LST
;; until WEDGE and the second containing all the items in LST after
;; WEDGE.
(let 'split-until (fn (lst wedge)
		      (when lst
			(if (starts-with lst wedge)
			    (list () (remove-n lst (len wedge)))
			  (do
			   (let 'res (split-until (cdr lst) wedge))
			   (list (cons (car lst) (car res)) (car (cdr res))))))))

;; Splits LST into sublists delineated by WEDGE.
(let 'split (fn (lst wedge)
		(when lst
		  (do
		   (let 'res (split-until lst wedge))
		   (let 'start (car res))
		   (let 'rest (car (cdr res)))
		   (cons start (split rest wedge))))))

;; Printf in Lust!
(let 'printf (fn (lst & args)
		 (when lst
		   (do
		    (let 'sections (split lst '"{}"))
		    (when (not (eq (len sections) (add (len args) 1)))
		      (error '"wrong number of arguments to printf"))
		    (let 'helper (fn (split args)
				     (when split
				       (do
					(let 's (car split))
					(when s (print s))
					(let 'a (car args))
					(when a (print a))
					(helper (cdr split) (cdr args))))))
		    (helper sections args)))))

(printf '"hello {}!\n" '"printf")
