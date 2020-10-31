// Type checker for Lust programs. For now let's assume that we're fed
// definitions in order so something like:
//
//   (foo 10)
//   (def foo (fn (x) x))
//
// Wouldn't be allowed.
//
// A def expression installs a definition in the current lexical
// scope. For literals the type is known at the time of evaluation,
// for function's it is known that the id has been bound to a
// function, but not what the argument or return types are.
//
// Lists can have whatever types they want inside of them in current
// Lust. This has the potential to make type checking them very
// hard. One way that we could attempt to type check them would be to
// verify that every operation done on them works for all possible
// items inside of them. This is problematic though because it
// seriously reduces the utility of lists.
//
// Perhaps then there is only a limited amount of type checking that
// we can actually do. For example, we can verify that functions in
// the current lexical enviroment are called with the correct number
// of arguments, but we can't verify that functions inside of lists in
// the current lexical enviroment are called correctly.
