# Assignment

An incremental approach recommends handling closures that close over
mutable variables by allocating all variables on the heap and
assigning to their locations. This is a fair approach if you're
interested in keeping things as simple as possible, but just seems
grossly slow.

As one of the goals of Lust is to be reasonably performant we won't do
that. For us, storing something on the heap involves a call to malloc
which is expensive.

## How we'll do it

At compile time we can already determine what variables are going to
be closed over. Rather than allocate all variables on the heap we'll
only allocate those variables that will be closed over on the heap and
the rest will remain on the stack.

Here's the implementation order:

- Add a variable renaming pass so that each variable in a scope is
  unique. For example:

  ```lisp
  (let foo 10)
  (let foo ())
  ```

  Will get transformed into:

  ```lisp
  (let 0_foo 10)
  (let 1_foo ())
  ```

  Performing that pass will make it much easier to determine what
  variables need to be heap allocated because in the case where the
  first foo is captured in a closure and the second is not we won't
  have to do any extra leg work.

- With the variable renaming pass done, do a pass moving through the
  source and annotating all variables that are closed over as heap
  allocated. This will require the addition of a field to the
  Expr::Symbol emum variant.

  Here's roughly how that pass can be done:

  - There are a finite and easily traversable number of lexical
    scopes. They are as follows:
	- The global scope (this is what gets compiled into lust_start).
	- The scope of each function body.
  - For each such scope, do the following:
	- Traverse the scope to find all closure objects that will be
      constructed inside of it.
	- Collect the closed over variables in those closure objects.
	- Move through all of the symbols in the scope and annotate them
      as heap allocated.
	- Note that we will not have to worry about traversing the bodies
      of nested functions as free variable analysis already handles
      that.

- During compilation creation of a symbol that is heap allocated
  involves a call to malloc.
- Assignment / access of a heap allocated symbol involves loading from
  that pointer.

## Renaming Pass

- Store global count of number of variables seen.
- Variables will be renamed to be {num}_oldname.

```lisp
(let foo 10) ; This is 1_foo
(let bar (fn (n)
	         (add n foo))) ; This also uses 1_foo
(let foo 11)
```

## Current let semantics

If the variabe being bound was declared in the current scope, let
assigns to the existing variable. If the varaible being bound was
declared in a different (outer) scope, let creates a new variable and
binds to it in the current scope.

As a result, it is currently impossible to modify a variable in a
scope outside of the current one. i.e. functions may not modify
variables in their closures.

The result of this is that any heap allocation of variables won't
matter.

## Proposed modifications

Let should work like Rust's let. Whenever it is used it ought to
create a new variable. For example:


```lisp
(let foo 10) ; This is 1_foo
(let bar (fn (n)
	         (add n foo))) ; This also uses 1_foo
(let foo 11) ; This is 2_foo
```

The practical implications of this are demonstrated here:

```lisp
(let x 10)
(let show-x (fn () (println x)))
(show-x) ; 10
(let x 11)
(show-x) ; with old semantics: 11
	     ; with new semantics: 10
```

In order to modify an existing variable the `set` expression will need
to be used. `set` will change the value of a variable. If the variable
is not defined `set` will error.

Here's the above example rewritten so that it still prints 11:

```lisp
(let x 10)
(let show-x (fn () (println x)))
(show-x) ; 10
(set x 11)
(show-x) ; 11
```
