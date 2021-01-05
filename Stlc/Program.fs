(*
 * Simply typed lambda calculus.
 *)

/// A type is either:
///    * A primitive type (e.g. Boolean), or
///    * A function type, which maps an input type to
///      an output type (e.g. Int -> Boolean).
[<StructuredFormatDisplay("{String}")>]
type Type =
    | Boolean   // a simple primitive type
    | Function of Input : Type * Output : Type

    /// Pretty-print types.
    member typ.String =
        match typ with
            | Boolean -> "bool"
            | Function (inType, outType) ->
                sprintf $"{inType} -> {outType}"

/// A term is a typed value.
[<StructuredFormatDisplay("{String}")>]
type Term =

    /// Literal true.
    | True

    /// Literal false.
    | False

    /// If/then/else.
    | If of Condition : Term
        * TrueBranch : Term
        * FalseBranch : Term

    /// A variable, using de Bruijn indexing. The nearest
    /// enclosing lambda binds index 0.
    | Variable of Index : int

    /// Lambda abstraction.
    | Lambda of Type * Term

    /// Function application.
    | Apply of Term * Term

    /// Pretty-print terms.
    member term.String =
        match term with
            | True -> "true"
            | False -> "false"
            | If (cond, tBranch, fBranch) ->
                $"(if {cond} then {tBranch} else {fBranch})"
            | Variable i -> $"var{i}"
            | Lambda (argType, body) ->
                $"(fun (_ : {argType}) -> {body})"
            | Apply (lambda, arg) ->
                $"({lambda}) ({arg})"

/// Answers the type of the given term, if it is well-typed.
let typeOf term =

    /// Determines the type of a term within a given environment,
    /// which is a list of types of free variables (i.e. variables
    /// bound at a higher level than this).
    let rec loop env = function

            // Boolean literals
        | True | False -> Boolean

            // if cond then tBranch else fBranch
        | If (cond, tBranch, fBranch) ->
            match loop env cond with
                | Boolean ->   // condition must be a plain Boolean
                    let tType = loop env tBranch
                    let fType = loop env fBranch
                    if tType = fType then tType   // branch types must match
                    else failwith "Branch type mismatch"
                | _ -> failwith "Condition must be of type Boolean"

            // variable in the given environment
        | Variable i ->
            env
                |> List.tryItem i
                |> Option.defaultWith (fun () ->
                    failwith "Unbound variable")

            // fun (var0 : argType) -> body
        | Lambda (argType, body) ->
            let bodyType =
                let env' = argType :: env   // add variable type to environment
                loop env' body
            Function (argType, bodyType)

            // function application
        | Apply (lambda, arg) ->
            match loop env lambda with   // first term must be a function
                | Function (inType, outType) ->
                    if loop env arg = inType then outType   // argument's type must match expected input type
                    else failwith "Unexpected argument type"
                | _ -> failwith "Not a function"

    loop [] term

let rec substitute n term = function
    | True -> True
    | False -> False
    | If (cond, tBranch, fBranch) ->
        If (
            substitute n term cond,
            substitute n term tBranch,
            substitute n term fBranch)
    | Variable i ->
        match compare i n with
             | -1 -> Variable i
             |  0 -> term
             |  1 -> Variable (i - 1)
             |  _ -> failwith "Unexpected"
    | Lambda (argType, body) ->
        let body' = substitute (n+1) term body
        Lambda (argType, body')
    | Apply (lambda, arg) ->
        Apply (
            substitute n term lambda,
            substitute n term arg)

let rec evaluate = function
    | Apply (lambda, arg) ->
        match evaluate lambda with
            | Lambda (_, body) ->
                substitute 0 arg body
                    |> evaluate
            | _ -> failwith "Not a function"
    | If (cond, tTerm, fTerm) ->
        match evaluate cond with
            | True -> evaluate tTerm
            | False -> evaluate fTerm
            | _ -> failwith "Condition must be of type Boolean"
    | term -> term

[<EntryPoint>]
let main argv =

    let id = Lambda (Boolean, (Variable 0))                     // fun (x : bool) -> x
    let cnst = Lambda (Boolean, Lambda (Boolean, Variable 1))   // fun (x : bool) -> fun (y : bool) -> x
    let not = Lambda (Boolean, If (Variable 0, False, True))    // fun (x : bool) -> if x then false else true
    let bad = If (True, id, cnst)
    let unbound = Variable 2

    for term in [ id; cnst; not; bad; unbound ] do
        printfn ""
        printfn $"{term}"
        try
            printfn $"   {typeOf term}"
            for input in [ True; False ] do
                let apply = Apply (term, input)
                printfn $"   {input}: {evaluate apply}"
        with ex ->
            printfn $"** {ex.Message} **"

    0
