(def = (fn (a b) (if (< a b) 0 (if (> a b) 0 1))))
(def add_inv (fn (n guess) (if (= 0 (+ n guess)) guess (add_inv n (+ guess -1)))))
(def - (fn (a b) (+ a (add_inv b b))))
(def * (fn (a n) (if (= n 0) 0 (+ a (* a (- n 1))))))
