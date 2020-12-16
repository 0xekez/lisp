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
			   (print "test ")
			   (print ,name)
			   (println "':\t[passed]"))
			(do
			 (print "test ")
			 (print ,name)
			 (println "':\t[failed] 🚨")))))

(do-test "#t" #t #t)
(do-test "#f" #f ())

(do-test "fold"
	 (fold (fn (a i) (add a i)) '(1 2 3) 0)
	 6)

(do-test "len" (len '(1 2 3 '(4 5 6))) 4)

(do-test "sum" (sum '(1 2 3 4 5)) 15)

(do-test "+" (+ 1 2 3 4 5) 15)

(do-test "-" (- 5 4 1) 0)

(do-test "*" (* 10 0.5 0.5) 2.5)

(do-test "/" (/ 10 2 2) 2.5)

(do-test "and #t" (and 10 (and #t 'hello)) #t)
(do-test "and #f" (and 10 (and #t ())) #f)

(do-test "or #t" (or () (or #f 'yes)) #t)
(do-test "or #f" (or () (or #f ())) #f)

(do-test "map"
	 (do
	  (let 'make-adder (fn (a) (fn (b) (add a b))))
	  (map (make-adder 10) '(0 1 2 3)))
	 '(10 11 12 13))

(do-test "all" (all '(1 2 'hello #t)) #t)
(do-test "any" (any '(#f () 'hello #f)) #t)

(do-test "==" (== 1 1 (add 0 1)) #t)

(do-test "list" (list 1 2 3 (add 1 1)) '(1 2 3 2))

(do-test "append" (append (list 1 2 3) 4) '(1 2 3 4))

(do-test "concat"
	 (concat (list 0 1 2 3 4) '(5 6 7 8))
	 '(0 1 2 3 4 5 6 7 8))

(do-test "last" (last '(1 2 3 (add 1 1))) '(add 1 1))

(do-test "do" (do (let 'foo 10) foo) 10)

(do-test "when" (macroexpand (when 1 2)) '(if 1 2 ()))

(do-test "filter" (filter (macroexpand (when 1 2)) (fn (i) (eq i 'if))) '(if))

(do-test "cond" (cond
		  ((eq 1 3) 10)
		  ((let 'foo ()) 15)
		  (#t foo)
		  (#t 6))
	 ())

(do-test "char" (list (char "🦢")) "🦢")

(print "\nRan ")
(print (add passed failed))
(println " tests.")

(print "\tpassed: ")
(println passed)

(print "\tfailed: ")
(println failed)
