The incremental approach paper doesn't spend much time on closures
which is a bummer as they seem reasonably hard. At a high level (what
the paper provides) here is how they can be done:

- Make a closure object wich is some heap allocated storage.
- The closure stores a label to jump to and values that have been
  captured in its enviroment.
- To make a call to a closure:
  1. Get the label from the closure object
  2. Place a pointer to the closure's variables on the stack.
  3. Jump to the label.
- Naturally this strategy requires that functions know when they need
  to look in their closures for variables.

Our approach will be reasonably similar but we don't deal directly
with the stack so we'll pass the closure object to the function
instead. Here's how it'll be done:

- Make a closure object that contains the address of the function it
  references followed by the values that it closes over. Its tag will
  be 0b110.
- Statically determine the closed over variables for each anonymous
  function and store a mapping between anonymous function and its
  closed over variables.
- During compilation, whenever we encounter an `__anon_fn` symbol,
  look up its mapping, assert that each of the closed over variables
  currently exists, and construct a closure object on the heap.
- During compilation, each function with closed over variables will
  have an additional argument which is a pointer to a heap allocated
  closure that will be passed to it.
- Every function will begin with a preable in which it emits the code
  to load the closure's values into local variables.
- When we encounter a closure in the head position of an expression
  load its label, construct a signature with the apropriate number of
  arguments, and emit a `call_indirect` instruction to jump to it.
