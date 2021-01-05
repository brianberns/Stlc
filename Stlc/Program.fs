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
        | True | False -> Some Boolean

            // if cond then termTrue else termFalse
        | If (cond, termTrue, termFalse) ->

                // loop with the same environment
            let nest = loop env

                // condition must be a plain Boolean
            if nest cond = Some Boolean then

                    // types of each branch must match
                match (nest termTrue), (nest termFalse) with
                    | Some typeTrue, Some typeFalse
                        when typeTrue = typeFalse -> Some typeTrue
                    | _ -> None

            else None

            // variable in the given environment
        | Variable i ->
            env |> List.tryItem i

            // fun (var0 : typeIn) -> termOut
        | Lambda (typeIn, termOut) ->

                // add the given input type to the environment
            let env' = typeIn :: env

                // find the type of the given output term in that environment
            loop env' termOut
                |> Option.map (fun typeOut ->
                    Function (typeIn, typeOut))

            // function application
        | Apply (lambda, arg) ->

                // first term must be a function
            match loop env lambda with

                    // argument's type must match the expected input type
                | Some (Function (typeIn, typeOut)) ->
                    let typeArg = loop env arg
                    if typeArg = Some typeIn then
                        Some typeOut
                    else None

                | _ -> None

    loop [] term

[<EntryPoint>]
let main argv =
    let id = Lambda (Boolean, (Variable 0))                     // fun (x : bool) -> x
    let cnst = Lambda (Boolean, Lambda (Boolean, Variable 1))   // fun (x : bool) -> fun (y : bool) -> x
    let not = Lambda (Boolean, If (Variable 0, False, True))    // fun (x : bool) -> if x then false else true
    let bad = If (True, id, cnst)
    let terms = [ id; cnst; not; bad ]
    for term in terms do
        typeOf term
            |> Option.map (fun typ -> typ.String)
            |> Option.defaultValue "Type error"
            |> printfn "%A:\r\n   %s" term
    0
