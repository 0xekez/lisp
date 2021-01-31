(let data-one (quote (1 2 3)))
(let data-two (quote 1))
(let data-three (quote (1 () 2)))
(let data-four (fn () (quote (1 2 3))))

;; Per a requirement of schemes quoted constants must always evaluate
;; to the same object.
(cons (eq (data-four) (data-four))
      (cons (car data-one)
	    (cons data-two
		  (car (cdr data-three)))))
