# Tail Recursion

Lustc ought to support tail recursion. A tail call is a call in the
form `return f()`. Normally when we see a call in that form we'd emit
something like this:

```
val = f()
return val
```

With tail call optimization we'd instead emit the following:

```
jump f
```

If the call was f(a) we'd emit something like this instead:

```
stack_slot_0 = eval(a)
jump f
```

In this case stack slot 0 is intended to represent where an argument
would normally be placed on the stack for a call to f.

## Cranelift

I'm not sure how this is best done in Cranelift. Cranelift does has a
jump instruction and it allows you to pass a number of arguments to
the basic block so this solves our problem but also creates a new
one. I'm not sure how to extract a basic block from a function
pointer in cranelift.

Sadly,
[this](https://github.com/bytecodealliance/wasmtime/issues/1065#issuecomment-518677459)
thread on GitHub makes me think that getting tail calls working might
not be possible for the time being. The trouble being that we'd need
some sort of `tailcall_indirect` instruction to do that without the
ability to emit a jump directly to a function pointer.
