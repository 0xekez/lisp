# Lust Spec

## Syntax

- string literal:
  ```scheme
  "hello"
  ```
- 32 bit float literal
 ```scheme
 1.5
 ```
- define a variable:
  ```scheme
  (define foo 0)
  ```
- define a function:
  ```scheme
  (define I (lambda (x) x))
  ```
- call a function:
  ```scheme
  (I 0)
  ```

## Builtins

- print something to the screen:
  ```scheme
  (display 0)
  ```

## JIT

JIT using
[Cranelift](https://github.com/bytecodealliance/simplejit-demo) as our
backend. Why use Cranelift:

	1. It's much easier to use in a Rust program and I didn't want to
	install things.

The original plan was to compile to the JVM but I couldn't find any
decent bindings to assemble JVM code from Rust.

Idea is to compile into a function called `lust_start_` that is the
"entrypoint" for lust programs. JIT returns a handle to that function
when its done with compilation. No idea how I'll do macros yet, but my
bet is that using a JIT means that it'll be possible to dynamically
compile them. Otherwise it seems easy enough to call into the
interpreter from the JIT.

String literals can be managed by writing them to the data section of
the generated code. See
[this](https://github.com/bytecodealliance/simplejit-demo/blob/736e1501da5caad25f4b4dfceabdec95f2972316/src/toy.rs#L121)
for an exmaple.

I still have no idea how I want to do memory management in Lust so the
idea of non-literal strings is pretty far down the line now.

Lambda's are going to be interesting to compile. To start we can
probably cheat and avoid higher order functions entiely by requiring
that lambdas only ever appear as the second argument to a define
call. Later on though we'll likely have to do some sort of lambda
lifting or otherwise. It's also possible that the jit will be totally
cool with defining functions in functions though which I'm down for.

This will be so interesting! What a wild learning experience it will
be to work all this out. These are problems I genuenly don't know how
to solve.
