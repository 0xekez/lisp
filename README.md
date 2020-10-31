ugh. tail recusion isn't going to work without a reasonably large
amount of refactoring in the interpreter so I'll skip that for now.


so JIT. Seems like we need to just accept that we can only JIT a small
portion of the code that can be executed. Let's say two argument
functions without any lists involved.

If that's the case then I think we can just go ahead and write a JIT
compiler for LustData.

The process will probably look something like this:

1. Write a JITData struct. Then write a conversion routine from
   LustData from JItData. Here we can catch issues related to List's
   being involved. This should essentially take some number of
   s-expressions and convert them into expressions with equicalents in
   the SimpleJit demo.

3. Some artificial requirements:
	- The top level JITData must be a function definition.
	- Functions can only have two floating point arguments.
	- Functions must return one floating point.

4. We then have some map between function names and their JIT
   functions. When the interpreter see's a function invocation with
   that name and two floating point arguments it can try and call
   that.

2. At that point we can probably pretty much copy paste over the other
   JIT and call it a day.


# Alternatively

Evaluating a function just returns a new enviroment and ast.

Specifically:

When you evaluate a function call you get back an enviroment and a
`Vec<LustData>` where the enviroment becomes the new enviroment to
eval in and the list of data becomes the new ast to evaluate.

First step is to change enviroments so that they are reference counted
instead of stored as references so that we can appease the borrow
checker.
