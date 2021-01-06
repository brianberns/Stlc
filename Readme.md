# Simply-typed lambda calculus
This is an example of a simply-typed lambda calculus in F#. I've provided a single Boolean type, but other types can be added easily. For simplicity, the implementation is a domain-specific language, so there's no parser, but that could also be added.
## Example
Let's implement the Boolean `not` function. Textually, it would be a lambda that looks something like this:
```F#
let not = fun (x : bool) -> if x then false else true
```
This can be converted directly into the DSL as:
```F#
let not = Lambda (Boolean, If (Variable 0, False, True))
```
Now we can call this function with an argument
<!--stackedit_data:
eyJoaXN0b3J5IjpbODYxMTU4NjA5LC0yMTE2MTE2OTI5XX0=
-->