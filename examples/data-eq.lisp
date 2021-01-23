

(let get-data (fn () (quote (1 2 3))))
(let get-not-data (fn ()
		      (cons 1
			    (cons 2
				  (cons 3 ())))))

;; Scheme semantics tell use that quoted data must always evaluate to
;; the same object and hence these two statements are not the same.
(cons (eq (get-data) (get-data))          ; true
      (eq (get-not-data) (get-not-data))) ; false
