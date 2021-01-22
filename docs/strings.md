Lustc ought to have string support at this point. Unlike the Lust
interpreter it seems reasonable to me that strings are represented as
contigious arrays in memory instead of linked lists for the following
reasons:

- More easily compatible with C functions.
- This is the way that Scheme compilers do it.
- The performance characteristics are closer to what is expected from
  strings.


Implementation details:

- Strings will be null terminated.
- They will be tagged with `0b010`.
- To start, all strings will be heap allocated. Down the line it might
  be reasonable that static strings are stored in the data section of
  their programs but this isn't a priority at the moment.

We'll support the following methods on strings to start:

- `string?` determines if its argument is a string.

Down the line once we have vector support built into the compiler it
would be interesting to let strings and vectors share much of the same
implementation. Before adding methods for working with strings
exclusively we should consider if a shared method works fine as well.
