;; Pretty standard stuff here. These are all the builtin "primitives"
;; currently implemented in Lustc

(let one 1)
(let two (add1 one))

(let char (integer->char one))
(let int (char->integer char))

(let is-null (null? int))
(let is-zero (zero? int))

(let is-bool (not (not (boolean? int))))
(let is-int (integer? is-bool))
(let is-pair (pair? (cons 1 (cons 2 ()))))

(let the-num (if is-pair 42 40))

(let math (add (sub (mul 10 10) 59) 1))

(let same (eq math the-num))
