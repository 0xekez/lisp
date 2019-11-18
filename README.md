# Lust

Lust is a very small Lisp-ish programming language written in Rust. If
you have used a programming language in the Lisp family before, much of
Lust should already be familiar to you.


## Getting started

Out of the box, Lust is really quite small. It has the notion of a
conditional, less than and greater than, variable bindings, and
functions. The following definition of a Fibonacci program actually
encompasses the entire language minus list processing:

```lisp
(def fib (fn (n) (if (< n 2) n (+ (fib (+ n -1)) (fib (+ n -2))))))
```

The goal of lust is to define a language with as little machinery as
possible. As much as possible, Lust should be written in Lust. In
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


## Lust Docs

The core goal is Lust is to be as simple as possible. There is no
notion of equality, subtraction, multiplication, or division baked
into the language. As a result, the entire description of the language
can fit into this rather small text file.

### (def [symbol] [expression])

Associates symbol with the result of evaluating expression in the
current environment. For example:

```lisp
(def dog 1)
(def I (fn (w) w))
```

### (fn [arg list] [body expression])

Evaluates to a lambda expression which takes arg list as arguments and
then evaluates the body expression in a new environment with those
arguments associated with whatever was passed into the lambda. The
returned lambda captures its closure.

### (if [cond expression] [true expression] [false expression])

Evaluates to an if expression which first evaluates its cond
expression and the evaluates its true or false expression conditional
on the result of the cond expression. In lust 0 is false and all other
numbers are true. Non-numeric values are not valid conditional
expressions.

### (print [expression])

Evaluates expression and then prints the result to std::oi. Returns 1.

### (list [a_1] .. [a_n])

Returns a list from its arguments. The arguments are evaluated.

### (nil? [list])

Returns 1 if the list is the empty list, 0 otherwise.

### (first [list])

Returns the first item in a list. Fails on an empty list.

### (rest [list])

Returns a list containing all but the first element of list. Fails on
an empty list.

### (eval [expression])

Exposes the evaluator to lust. First resolves / evaluates all its
argument, then evaluates whatever is returned.

### (> [a_1] .. [a_n)

Returns 1 if a_1 > a_2 > ..., 0 otherwise.

### (< [a_1] .. [a_n)

Returns 1 if a_1 < a_2 < ..., 0 otherwise.

### (+ [a_1] .. [a_n)

Returns a_1 + a_2 + a_3 ... + a_n.
