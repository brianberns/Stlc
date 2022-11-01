open Stlc

[<EntryPoint>]
let main argv =
    Apply (Variable 0, Variable 1)
        |> Term.eval
        |> printfn "%A"
    0
