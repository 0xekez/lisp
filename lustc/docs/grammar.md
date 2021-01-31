# Grammar

```
Expr := Fixnum
	  | Char
	  | "(" List ")"
	  | Bool

Fixnum := [0-9]+

Char := "#\".

List := Expr
	  | Expr List

Bool := "#f"
	  | "#t"
```

Later we'll need floating point numbers as well.
