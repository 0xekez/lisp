;; Demo program showing how to extract lines that start with a comment
;; from a Lust string.

(import 'std)

;; A sample string
(letq sample '";; Demo program showing how to extract lines that start with a comment\n;; from a Lust string.\n\n(import 'std)\n\n;; A sample string\n\n;; A function to pop a line from a STR.\n(letq pop-line (fn (str accum)\n\t\t   (do\n\t\t    (letq first (car str))\n\t\t    (if (or (eq first (car '\n)) (eq str ()))\n\t\t\t(cons accum (list (cdr str)))\n\t\t      (pop-line (cdr str) (append accum first))))))\n\n;; Takes a string STR and splits it into its lines.\n(letq split-lines (fn (str)\n\t\t      (do\n\t\t       (letq res (pop-line str ()))\n\t\t       (letq line (car res))\n\t\t       (letq rest (car (cdr res)))\n\t\t       (if rest\n\t\t\t   (cons line (split-lines rest))\n\t\t\t (list line)))))\n\n;; Returns true if LINE starts with \n(letq is-comment-line (fn (line)\n\t\t\t  (eq (car line) (car ))))\n\n;; Returns a list containing the elements in ITEMS that PRED returns\n;; true for.\n(letq filter (fn (items pred)\n\t\t (if items\n\t\t   (do\n\t\t    (letq first (car items))\n\t\t    (if (pred first)\n\t\t\t(cons first (filter (cdr items) pred))\n\t\t      (filter (cdr items) pred)))\n\t\t   ())))\n\n;; Gets all of the lines that begin with from STR.\n(letq get-comment-lines (fn (str)\n\t\t       (filter (split-lines str) is-comment-line)))\n\n; Print the result\n(println (get-comment-lines sample))\n")
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

;; Gets all of the lines that begin with ';' from STR.
(letq get-comment-lines (fn (str)
		       (filter (split-lines str) is-comment-line)))

; Print the result
(get-comment-lines sample)
