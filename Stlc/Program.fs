[<StructuredFormatDisplay("{String}")>]
type Type =
    | Boolean
    | Function of Input : Type * Output : Type

    member typ.String =
        match typ with
            | Boolean -> "bool"
            | Function (typIn, typOut) ->
                sprintf $"{typIn} -> {typOut}"

type Term =
    | True
    | False
    | If of Condition : Term * TrueBranch : Term * FalseBranch : Term
    | Variable of Index : int   // de Bruijn index
    | Lambda of Type * Term     // e.g. \0:Bool -> 0
    | Apply of Term * Term      // e.g. f x

let rec typeOf env = function
    | True
    | False -> Some Boolean
    | If (cond, termTrue, termFalse) ->
        let nest = typeOf env
        if nest cond = Some Boolean then
            match (nest termTrue), (nest termFalse) with
                | Some typeTrue, Some typeFalse
                    when typeTrue = typeFalse -> Some typeTrue
                | _ -> None
        else None
    | Variable i ->
        env |> List.tryItem i
    | Lambda (typ, term) ->
        let env' = typ :: env
        typeOf env' term
            |> Option.map (fun typ' ->
                Function (typ, typ'))
    | Apply (term, term') ->
        match typeOf env term with
            | Some (Function (typIn, typOut)) ->
                let typIn' = typeOf env term'
                if typIn' = Some typIn then
                    Some typOut
                else None
            | _ -> None

[<EntryPoint>]
let main argv =
    let id = Lambda (Boolean, (Variable 0))                     // fun (x : bool) -> x
    let cnst = Lambda (Boolean, Lambda (Boolean, Variable 1))   // fun (x : bool) -> fun (y : bool) -> x
    let not = Lambda (Boolean, If (Variable 0, False, True))    // fun (x : bool) -> if x then false else true
    let terms = [ id; cnst; not ]
    for term in terms do
        printfn $"{typeOf [] term}"
    0
