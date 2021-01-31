(let foo 10)
(let get-foo (fn ()
		 foo))
(let set-foo (fn (a) (set foo a)))

;; This creates a new foo variable that shadows the old one.
(let foo 11)
;; The foo captured in our functions closures can still be modified
;; with the set expression.
(set-foo 12)

(cons foo (get-foo))
