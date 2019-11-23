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
order to have the notion of equality, we must implement it.

```lisp
(def = (fn (a b)
     (if (err? (+ a 1))
     	 (if (err? (+ b 1)) (list= a b) 0)
	 (if (err? (+ b 1)) 0 (float= a b)))))

```

First, we test to see if we can add one to the input. If we can we
call `float=` which compares floats, if we can not, we call `list=`
which compares lists. We can define those two functions as follows:

```lisp
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
```

At this point, you've seen the vast majority of Lust's builtin
functions. Now we'll define some more math operations.

For subtraction, we need a way to compute the additive inverse. At the
moment, I have yet to work out a good way to do this for non-whole
numbers. For whole numbers though, we can define the additive inverse
as follows:

```lisp
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
```

Once we have that, we can define subtraction and multiplication
without too much trouble:

```lisp
(def - (fn (a b)
     (+ a (add_inv b b))))

(def * (fn (a n)
     (if (= n 0) 0
     	 (+ a (* a (- n 1))))))
```

Lust can also read source files. For your convience, all of these
functions can be loaded into a REPL by running:

```
cargo run std.ls
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

### (err? [expression])

Evaluates expression and returns 1 if it resulted in an error, 0
otherwise. The error will not be printed to stderr.

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
