;; Makes calls to open, calloc, read, and printf to print the beginning
;; of a file.
(let fd (foreign-call "open" "examples/file.lisp" 0))
(let bar (foreign-call "calloc" 1 12))
(foreign-call "read" fd bar 11)
(foreign-call "printf" "the string is %s and has len %d\n" bar (foreign-call "strlen" bar))
(foreign-call "free" bar)

;; NOTE: if we had a second call to printf here we would be in trouble
;; as the two calls would have different signatures. Cranelift doesn't
;; support functions with varadic arguments so we'll fail in
;; compilation when it sees that.
;;
;; (foreign-call "printf" "the string is %s\n" bar)
