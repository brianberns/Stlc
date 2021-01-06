namespace Stlc.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Stlc

[<TestClass>]
type TestClass () =

    /// Function application.
    let apply lambda arg =
        Apply (lambda, arg)
            |> Term.eval

    /// fun (x : bool) -> x
    let id = Lambda (Boolean, (Variable 0))

    // fun (x : bool) -> if x then false else true
    let not = Lambda (Boolean, If (Variable 0, False, True))

    [<TestMethod>]
    member __.ID() =

            // check type
        Assert.AreEqual(
            Function(Boolean, Boolean),
            Term.typeOf id)

            // check function application
        for arg in [ True; False ] do
            Assert.AreEqual(arg, apply id arg)

    [<TestMethod>]
    member __.Not() =

            // check type
        Assert.AreEqual(
            Function(Boolean, Boolean),
            Term.typeOf id)

            // check function application
        let cases =
            [
                True, False
                False, True
            ]
        for (arg, expected) in cases do
            Assert.AreEqual(expected, apply not arg)

    [<TestMethod>]
    member __.TypeError() =
        let term = If (True, True, id)
        Assert.ThrowsException(fun () ->
            Term.typeOf term |> ignore)
                |> ignore

    [<TestMethod>]
    member __.NestedLambda() =

            // fun f -> (fun x -> f x)
        let lambda =
            Lambda (
                Function(Boolean, Boolean),
                Lambda (Boolean, Apply (Variable 1, Variable 0)))
        let term = Apply (Apply (lambda, not), True)
        Assert.AreEqual(False, Term.eval term)

            // fun x -> (fun f -> f x)
        let lambda =
            Lambda (
                Function(Boolean, Boolean),
                Lambda (Boolean, Apply (Variable 0, Variable 1)))
        let term = Apply (Apply (lambda, True), not)
        Assert.AreEqual(False, Term.eval term)

    [<TestMethod>]
    member __.Or() =

            // fun x -> fun y -> if x then true else y
        let lambda =
            Lambda (
                Boolean,
                Lambda (Boolean, If (Variable 1, True, Variable 0)))

        let cases =
            [
                True,  True,  True
                True,  False, True
                False, True,  True
                False, False, False
            ] 
        for (x, y, expected) in cases do
            let term = Apply (Apply (lambda, x), y)
            Assert.AreEqual(expected, Term.eval term)

    [<TestMethod>]
    member __.And() =

            // fun x -> fun y -> if x then y else false
        let lambda =
            Lambda (
                Boolean,
                Lambda (Boolean, If (Variable 1, Variable 0, False)))

        let cases =
            [
                True,  True,  True
                True,  False, False
                False, True,  False
                False, False, False
            ] 
        for (x, y, expected) in cases do
            let term = Apply (Apply (lambda, x), y)
            Assert.AreEqual(expected, Term.eval term)

    [<TestMethod>]
    member __.OpenTerm() =

            // fun x -> x y
        let lambda =
            Lambda (
                Function(Boolean, Boolean),
                Apply (Variable 0, Variable 1))
        let term = Apply (lambda, id)
        Assert.AreEqual(Variable 0, Term.eval term)
