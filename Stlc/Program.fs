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
            | Function (typIn, typOut) ->
                sprintf $"{typIn} -> {typOut}"

/// A term is a typed value.
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

/// Answers the type of the given term, if it is well-typed.
let typeOf term =

    /// Determines the type of a term within a given environment,
    /// which is a list of types of free variables (i.e. variables
    /// bound at a higher level than this).
    let rec loop env = function

            // Boolean literals
        | True | False -> Boolean

            // if cond then termTrue else termFalse
        | If (cond, termTrue, termFalse) ->
            match loop env cond with
                | Boolean ->   // condition must be a plain Boolean
                    let typeTrue = loop env termTrue
                    let typeFalse = loop env termFalse
                    if typeTrue = typeFalse then typeTrue   // branch types must match
                    else failwith "Branch type mismatch"
                | _ -> failwith "Condition must be of type Boolean"

            // variable in the given environment
        | Variable i ->
            env
                |> List.tryItem i
                |> Option.defaultWith (fun () ->
                    failwith "Unbound variable")

            // fun (var0 : typeIn) -> termOut
        | Lambda (typeIn, termOut) ->
            let typeOut =
                let env' = typeIn :: env   // add variable type to environment
                loop env' termOut
            Function (typeIn, typeOut)

            // function application
        | Apply (lambda, arg) ->
            match loop env lambda with   // first term must be a function
                | Function (typeIn, typeOut) ->
                    if loop env arg = typeIn then typeOut   // argument's type must match expected input type
                    else failwith "Unexpected argument type"
                | _ -> failwith "Not a function"

    loop [] term

[<EntryPoint>]
let main argv =
    let id = Lambda (Boolean, (Variable 0))                     // fun (x : bool) -> x
    let cnst = Lambda (Boolean, Lambda (Boolean, Variable 1))   // fun (x : bool) -> fun (y : bool) -> x
    let not = Lambda (Boolean, If (Variable 0, False, True))    // fun (x : bool) -> if x then false else true
    let bad = If (True, id, cnst)
    let unbound = Variable 2
    let terms = [ id; cnst; not; bad; unbound ]
    for term in terms do
        printfn "%A:" term
        try
            printfn "   %A" <| typeOf term
        with ex ->
            printfn "   %s" ex.Message
    0
