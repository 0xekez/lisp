# Risp

Out of the box, Risp is really quite small. It has the notion of a
conditional, less than and greater than, variable bindings, and
functions. The following definition of a Fibonacci program actually
encompasses the entire language:

```lisp
(def fib (fn (n) (if (< n 2) n (+ (fib (+ n -1)) (fib (+ n -2))))))
```

The goal of risp is to define a language with as little machinery as
possible. As much as possible, Risp should be written in Risp. In
order to have the notion of equality, we define it as not less than
and not more than:

```lisp
(def = (fn (a b) (if (< a b) 0 (if (> a b) 0 1))))
```

For subtraction, we need a way to compute the additive inverse. This
function works only for positive integers, but does the job for them:

```lisp
(def add_inv (fn (n guess) (if (= 0 (+ n guess)) guess (add_inv n (+ guess -1)))))
```

Once we have that, we can define subtraction and multiplication
without too much trouble:

```lisp
(def - (fn (a b) (+ a (add_inv b b))))
(def * (fn (a n) (if (= n 0) 0 (+ a (* a (- n 1))))))
```
