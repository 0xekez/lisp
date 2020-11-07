;; Demo program showing how to extract lines that start with a comment
;; from a Lust string.

(import 'std)

;; A sample string
(letq sample '";; This is a comment\n(fn (a) a)\n(fn (b) b)\n;;here's another comment\n; and another one!")

;; A function to pop a line from a STR.
(letq pop-line (fn (str accum)
		   (do
		    (letq first (car str))
		    (if (or (eq first (car '"\n")) (eq str ()))
			(cons accum (list (cdr str)))
		      (pop-line (cdr str) (append accum first))))))

;; Takes a string STR and splits it into its lines.
(letq split-lines (fn (str)
		      (do
		       (letq res (pop-line str ()))
		       (letq line (car res))
		       (letq rest (car (cdr res)))
		       (if rest
			   (cons line (split-lines rest))
			 (list line)))))

;; Returns true if LINE starts with ';'
(letq is-comment-line (fn (line)
			  (eq (car line) (car '";"))))

;; Returns a list containing the elements in ITEMS that PRED returns
;; true for.
(letq filter (fn (items pred)
		 (if items
		   (do
		    (letq first (car items))
		    (if (pred first)
			(cons first (filter (cdr items) pred))
		      (filter (cdr items) pred)))
		   ())))

;; Gets all of the lines that begin with ';' from STR.
(letq get-comment-lines (fn (str)
		       (filter (split-lines str) is-comment-line)))

; Print the result
(println (get-comment-lines sample))
