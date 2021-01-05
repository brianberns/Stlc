(*
 * Simply typed lambda calculus.
 *)

/// A type is either:
///    * A primitive type (e.g. Boolean), or
///    * A function type, which maps an input type to an
///      output type (e.g. Boolean -> Boolean).
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

    /// If-then-else.
    | If of Condition : Term
        * TrueBranch : Term
        * FalseBranch : Term

    /// A variable, using de Bruijn indexing. Index 0 corresponds
    /// to the nearest enclosing lambda's input parameter.
    | Variable of Index : int

    /// Lambda abstraction. E.g. fun (_ : bool) -> var0.
    | Lambda of ParamType : Type * Body : Term

    /// Function application. E.g. f(x).
    | Apply of Lambda : Term * Arg : Term

    /// Pretty-print terms.
    member term.String =
        match term with
            | True -> "true"
            | False -> "false"
            | If (cond, tBranch, fBranch) ->
                $"(if {cond} then {tBranch} else {fBranch})"
            | Variable i -> $"var{i}"
            | Lambda (paramType, body) ->
                $"(fun (_ : {paramType}) -> {body})"
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
        | If (cond, trueBranch, falseBranch) ->
            match loop env cond with
                | Boolean ->   // condition must be a plain Boolean
                    let trueType = loop env trueBranch
                    let falseType = loop env falseBranch
                    if trueType = falseType then trueType   // branch types must match
                    else failwith "Branch type mismatch"
                | _ -> failwith "Condition must be of type Boolean"

            // variable in the given environment
        | Variable i ->
            env
                |> List.tryItem i
                |> Option.defaultWith (fun () ->
                    failwith "Unbound variable")

            // fun (_ : paramType) -> body
        | Lambda (paramType, body) ->
            let bodyType =
                let env' = paramType :: env   // add param type to environment
                loop env' body
            Function (paramType, bodyType)

            // function application
        | Apply (lambda, arg) ->
            match loop env lambda with   // first term must be a function
                | Function (inType, outType) ->
                    if loop env arg = inType then outType   // argument's type must match expected input type
                    else failwith "Unexpected argument type"
                | _ -> failwith "Not a function"

    loop [] term

/// Replaces all occurrences of param with arg in body.
let rec subst iParam arg body =
    let cont = subst iParam arg   // shorthand for simple recursive substitution
    match body with

            // no effect on literals
        | True -> True
        | False -> False

            // recursively substitute
        | If (cond, trueBranch, falseBranch) ->
            If (cont cond, cont trueBranch, cont falseBranch)

            // substitute iff variables match
        | Variable iVar ->
            if iVar = iParam then arg
            else body

            // current var0 is known as var1 within
        | Lambda (argType, body') ->
            let body' = subst (iParam+1) arg body'
            Lambda (argType, body')

            // recursively substitute
        | Apply (lambda, arg) ->
            Apply (cont lambda, cont arg)

/// Evaluates the given term.
let rec eval = function

        // function application
    | Apply (lambda, arg) ->
        match eval lambda with
            | Lambda (_, body) ->
                subst 0 arg body
                    |> eval
            | _ -> failwith "Not a function"

        // evaluate correct branch only
    | If (cond, trueBranch, falseBranch) ->
        match eval cond with
            | True -> eval trueBranch
            | False -> eval falseBranch
            | _ -> failwith "Condition must be of type Boolean"

    | term -> term

[<EntryPoint>]
let main argv =

    let id = Lambda (Boolean, (Variable 0))                     // fun (x : bool) -> x
    let tru = Lambda (Boolean, True)                            // fun (x : bool) -> true
    let cnst = Lambda (Boolean, Lambda (Boolean, Variable 1))   // fun (x : bool) -> fun (y : bool) -> x
    let not = Lambda (Boolean, If (Variable 0, False, True))    // fun (x : bool) -> if x then false else true
    let bad = If (True, id, cnst)
    let unbound = Variable 2

    for term in [ True; id; tru; cnst; not; bad; unbound ] do
        printfn ""
        printfn $"{term}"
        try
            printfn $"   {typeOf term}"
            for input in [ True; False ] do
                let apply = Apply (term, input)
                printfn $"   {input}: {eval apply}"
        with ex ->
            printfn $"   ** {ex.Message} **"

        // fun f -> fun x -> f x
    let term =
        Lambda (
            Function(Boolean, Boolean),
            Lambda (Boolean, Apply (Variable 1, Variable 0)))
    printfn ""
    printfn $"{typeOf term}"

        // (fun f x -> f x) not true
    let appl = Apply (Apply (term, not), True)
    printfn $"not true = {eval appl}"

        // or:  fun x -> fun y -> if x then true else y
        // and: fun x -> fun y -> if x then y else false
    let boolOr =
        Lambda (
            Boolean,
            Lambda (Boolean, If (Variable 1, True, Variable 0)))
    let boolAnd =
        Lambda (
            Boolean,
            Lambda (Boolean, If (Variable 1, Variable 0, False)))
    printfn ""
    for x in [True; False] do
        for y in [True; False] do
            let applOr = Apply (Apply (boolOr, x), y)
            let applAnd = Apply (Apply (boolAnd, x), y)
            printfn $"{x} or {y} = {eval applOr}"
            printfn $"{x} and {y} = {eval applAnd}"

    0
