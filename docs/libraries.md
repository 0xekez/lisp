# Libraries

Step 14 asks us to compile primitives seperately so that they can be
treated like regular higher order functions and to open the door for
seperately compiled libraries. In the interest of keeping things
simple we'll add an extra "closure" paramater to builtin functions and
treat them as normally as possible.

During construction of `JIT::default()` we'll compile all of the
builtin functions. This will happen in much the same way as
`emit_alloc` except that we'll need to construct a context for
building them and write a function to handle the boilerplate of
emitting a procedure. They will also take one extra argument which
will be their closure pointer. Their closure will always be empty.

As we compile them we'll collect information about them into a
list. This information will later be used to add them to the function
map once all of the user defined functions have been emitted. At this
point builtin functions will behave like regular functions as for all
intents and purposes they will just be regular Lust functions that are
compiled a little different.
