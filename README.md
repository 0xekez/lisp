# Lust

Lust is a Lisp programming langauge built on a small, solid
core. Here's an example REPL session:

```lisp
>> (import 'format)
>> (let say-hello (fn (name)
                      (printf "hello {}!" name)))
>> (say-hello "Lust")
hello Lust!
```

The idea for Lust stems from a frusturation with other high level
languages which are fun to start but rapidly get too slow to
maintain. Things don't need to be like this. With modern JITs and some
care we can have a **high level language and reasonable performance**.

Lust has both an interpreter and compiler which live in the `lust` and
`lustc` subdirectories respectively. Both are written in Rust. The
compiler uses
[Cranelift](https://github.com/bytecodealliance/wasmtime/tree/main/cranelift)
as a backend and is pretty quick.

You can find documentation about Lust
[here](https://zmedley.com/lust).

## Project Status

Lust is currently under heavy development. Things will break and the
documentation is now slightly incomplete in places under active
development. If you'd like to take a stab on working on the compiler
or interpreter I try and organize myself with GitHub issues and you're
welcome to take a look.

## Building Lust

To build Lust you'll need a working Rust installation. Once you've got
that its as easy as:

```
git clone https://github.com/ZekeMedley/lust.git
cd lust/{lust|lustc}
cargo build
cargo run
```

In order for the Lust interpreter to locate files in its standard
library you'll need to set the `LUSTPATH` enviroment
variable. Something to the tune of this ought to do that:

```
export LUSTPATH="<path to lust repo>/std/"
```

## Using Lust

You're best bet for using Lust is probably to take a look at some of
the example programs
[here](https://github.com/ZekeMedley/lust/tree/master/lust/lust-programs)
and then firing up the REPL with `cargo run`.

Even without the compiler Lust programs should be reasonably quick. If
you find yourself needing some more speed consider using a release
build with `cargo run --release`.
