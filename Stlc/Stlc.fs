﻿/// Simply-typed lambda calculus.
namespace Stlc

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
    /// to the most nested lambda's input parameter.
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
            | If (cond, trueBranch, falseBranch) ->
                $"(if {cond} then {trueBranch} else {falseBranch})"
            | Variable i -> $"var{i}"
            | Lambda (paramType, body) ->
                $"(fun (_ : {paramType}) -> {body})"
            | Apply (lambda, arg) ->
                $"({lambda}) ({arg})"

module Term =

    /// Answers the type of the given term, if it is well-typed.
    let typeOf term =

        /// Determines the type of a term within a given environment,
        /// which is a list of types of free variables (i.e. variables
        /// bound at a higher level than this).
        let rec loop env = function

                // Boolean literals
            | True | False -> Boolean

                // if cond then trueBranch else falseBranch
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

    (*
     * Maintaining indexes is a bit tricky during parameter substitution,
     * so it helps to have a clear picture of how de Bruijn indexes work.
     * When we wrap an existing term in a lambda, it's like adding a story
     * to a building:
     *
     *                               +---------+
     *                               |  * 2 *  |
     *    +---------+                +---------+
     *    |    1    |   New lambda   |    1    |
     *    +---------+  ===========>  +---------+
     *    |    0    |                |    0    |
     *    +---------+                +---------+
     *
     * So when we unwrap a lambda (via function application), the indexes
     * of variables nested *below* that lambda are unaffected. However,
     * the lambda body might also contain indexes defined *above* its level
     * ("free variables"). In this case, those indexes fall down one level
     * during the process (like Jenga blocks):
     *
     * +---------+         +---------+
     * |    3    |         |    3    |
     * +---------+         +---------+         +---------+
     * | xx 2 xx |                             | 3 => 2  |
     * +---------+  ====>  +---------+  ====>  +---------+
     * |    1    |         |    1    |         |    1    |
     * +---------+         +---------+         +---------+
     * |    0    |         |    0    |         |    0    |
     * +---------+         +---------+         +---------+
     *)

    /// Replaces all occurrences of param with arg in body.
    let rec private subst iParam arg body =
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
                match compare iVar iParam with
                     | -1 -> body                  // not a match: no effect
                     |  0 -> arg                   // match: substitute value
                     |  1 -> Variable (iVar - 1)   // free variable: shift to maintain external references to it
                     |  _ -> failwith "Unexpected"

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
