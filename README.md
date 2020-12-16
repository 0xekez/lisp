# Lust

Lust is reasonably small Lisp built on a small, solid core.

```lisp
(import 'format)

(printf "hello {}!" "Lust")
```

You can find documentation about Lust [here](https://zmedley.com/lust).

At the moment, Lust has a functional interpreter and an early stage JIT
[compiler](https://www.github.com/ZekeMedley/lustc) built on Bytecode Alliance's
[Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
code generator. The interpreter is functional in the sense that it supports the
following:

- Tail recursion
- Macros
- Lists, numbers, and unicode strings
- Importing other files
- Reference counting garbage collection

Lust itself is incomplete though in the following ways:

- Only extremely basic I/O support.
- Garbage collection is unable to detect cycles.

At the moment these issues are more design issues than implementation ones. The
design of Lust's compiler seems liable to heavily impact how I decide to build
those in so I'm putting it off until more progress is made on that front.

# Building Lust

To build Lust you'll need a working Rust installation. Once you've got that its
as easy as:

```
git clone https://github.com/ZekeMedley/lust.git
cd lust
cargo build
cargo run
```

In order for Lust to locate files in its standard library you'll need to set the
`LUSTPATH` enviroment variable. Something to the tune of this ought to do that:

```
export LUSTPATH="<path to lust repo>/std/"
```

# Using Lust

You're best bet for using Lust is probably to take a look at some of the example
programs [here](https://github.com/ZekeMedley/lust/tree/master/lust-programs)
and then firing up the REPL with `cargo run`.

Even without the compiler Lust programs should be reasonably quick. If you find
yourself needing some more speed consider using a release build with `cargo run
--release`.
