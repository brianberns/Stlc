# Simply-typed lambda calculus
This is an example of a simply-typed lambda calculus in F#. I've provided a single Boolean type, but other types can be added easily. For simplicity, the implementation is a domain-specific language within F#, so there's no parser, but that could also be added.
## Example
Let's implement the Boolean `not` function. Textually, it would be a lambda that looks something like this:
```F#
let not = fun (x : bool) -> if x then false else true
```
This can be converted directly into the DSL as:
```F#
let not = Lambda (Boolean, If (Variable 0, False, True))
```
The lambda has the expected type:
```F#

```
Now we can show that this function inverts its input, as expected:
```F#
// not true => false
let result = Apply(not, True) |> Term.eval
assert(result = False)
```
<!--stackedit_data:
eyJoaXN0b3J5IjpbNTYxMjAzNDMzLDY4MTc0OTIwOSwxNjQ5MD
c3NjExLC0yMTE2MTE2OTI5XX0=
-->