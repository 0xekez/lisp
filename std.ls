(def float= (fn (a b)
     (if (< a b) 0
     	 (if (> a b) 0 1))))

(def list= (fn (a b)
     (if (nil? a)
     	 (if (nil? b) 1 0)
	 (if (nil? b) 0
	     (if (= (first a) (first b))
	     	 (= (rest a) (rest b))
	     	 0)))))

(def = (fn (a b)
     (if (err? (+ a 1))
     	 (if (err? (+ b 1)) (list= a b) 0)
	 (if (err? (+ b 1)) 0 (float= a b)))))

(def add-inv-pos (fn (n guess)
     (if (= (+ n guess) 0) guess
     	(add-inv-pos n (+ guess -1)))))

(def add-inv-neg (fn (n guess)
     (if (= (+ n guess) 0) guess
     	 (add-inv-neg n (+ guess 1)))))

(def add-inv (fn (n)
     (if (< n 0)
     	 (add-inv-neg n n)
     	 ( if ( > n 0 )
	   (add-inv-pos n n)
	   0))))

(def - (fn (a b)
     (+ a (add_inv b b))))

(def * (fn (a n)
     (if (= n 0) 0
     	 (+ a (* a (- n 1))))))

