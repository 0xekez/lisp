(let x 10)
(let new-x (fn ()
	       (let x 11))) ;; Here, let creates a new x variable as x
			    ;; was not defined in the current scope.
(new-x)
(let x (add x 1)) ;; Here, because x was defined in the current scope
		  ;; the variable x is modified to contain a new
		  ;; value.
x
