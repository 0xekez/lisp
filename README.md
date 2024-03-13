A little lisp compiler and interpereter. The compiler is just in time and written with [Cranelift]([url](https://cranelift.dev/)).

```lisp
>> (import 'format)
>> (let say-hello (fn (name)
                      (printf "hello {}!" name)))
>> (say-hello "Lisp")
hello Lisp!
```

## Performance Example

Consider everyone's favorite fib function in this lisp and in Python:

```lisp
(let fib (fn (n)
	     (if (lt n 2)
		 n
	       (add (fib (sub n 2)) (fib (sub n 1))))))
```
```python
def fib(n):
    if n < 2:
        return n
    return fib(n-1) + fib(n-2)
```

The JIT compiler can compute `(fib 40)` in 2.3 seconds. It takes
Python 35 seconds.

Take this with a grain of salt because there are many things Python
can do very elegantly that are still awkward in here and I'm sure
there are places where python's C bindings give it a leg up. The point
is that in this case both languages are very similar looking but one
is much, much quicker.

The compiler also doesn't have a garbage collector yet but who really
needs one if you can run everything faster than a gc cycle ;)
[here](https://github.com/ZekeMedley/lust/tree/main/lustc/examples).
