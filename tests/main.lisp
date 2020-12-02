;; The beginning of a test suit for Lust.

(import 'std)

(let 'passed 0)
(let 'failed 0)

;; Runs a test and incremets passed / failed as needed.
(let 'test (macro (call res)
		  `(if (eq ,call ,res)
		       (do
			(let 'passed (add passed 1))
			#t)
		     (do
		      (let 'failed (add failed 1))
		      #f))))

(let 'do-test (macro (name expr expected)
		     `(if (test ,expr ,expected)
			  (do
			   (print '"test '")
			   (print ,name)
			   (println '"':\t[passed]"))
			(do
			 (print '"test '")
			 (print ,name)
			 (println '"':\t[failed]")))))

(do-test '"#t" #t #t)
(do-test '"#f" #f ())

(do-test '"fold"
	 (fold (fn (a i) (add a i)) '(1 2 3) 0)
	 6)

(do-test '"len" (len '(1 2 3 '(4 5 6))) 4)

(do-test '"add" (add 1 2) 3)
(do-test '"+" (+ 1 2 3 4 5) 15)


(print '"\nRan ")
(print (add passed failed))
(println '" tests.")

(print '"\tpassed: ")
(println passed)

(print '"\tfailed: ")
(println failed)
