namespace Stlc.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Stlc

[<TestClass>]
type TestClass() =

    /// Function application.
    let apply lambda arg =
        Apply (lambda, arg)
            |> Term.eval

    /// fun (x : bool) -> x
    let id = Lambda (Boolean, (Variable 0))

    // fun (x : bool) -> if x then false else true
    let not = Lambda (Boolean, If (Variable 0, False, True))

    [<TestMethod>]
    member _.ID() =

            // check type
            //    id : bool -> bool
        Assert.AreEqual<_>(
            Ok (Function (Boolean, Boolean)),
            Term.typeOf id)

            // check function application
            //    id true = true
            //    id false = false
        for arg in [ True; False ] do
            Assert.AreEqual<_>(Ok arg, apply id arg)

    [<TestMethod>]
    member _.Not() =

            // check type
            //    not : bool -> bool
        Assert.AreEqual<_>(
            Ok (Function (Boolean, Boolean)),
            Term.typeOf not)

            // check function application
            //   not true = false
            //   not false = true
        let cases =
            [
                True, False
                False, True
            ]
        for arg, expected in cases do
            Assert.AreEqual<_>(Ok expected, apply not arg)

    [<TestMethod>]
    member _.TypeError() =
        let term = If (True, True, id)
        Assert.AreEqual<_>(
            Error "Branch type mismatch",
            Term.typeOf term)

    [<TestMethod>]
    member _.NestedLambda() =

            // fun f -> (fun x -> f x)
            //    λ not true = false
        let lambda =
            Lambda (
                Function (Boolean, Boolean),
                Lambda (Boolean, Apply (Variable 1, Variable 0)))
        let term = Apply (Apply (lambda, not), True)
        Assert.AreEqual<_>(Ok False, Term.eval term)

            // fun x -> (fun f -> f x)
            //   λ true not = false
        let lambda =
            Lambda (
                Function (Boolean, Boolean),
                Lambda (Boolean, Apply (Variable 0, Variable 1)))
        let term = Apply (Apply (lambda, True), not)
        Assert.AreEqual<_>(Ok False, Term.eval term)

    [<TestMethod>]
    member _.Or() =

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
            Assert.AreEqual<_>(Ok expected, Term.eval term)

    [<TestMethod>]
    member _.And() =

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
            Assert.AreEqual<_>(Ok expected, Term.eval term)

    [<TestMethod>]
    member _.OpenTerm() =

            // fun x -> x y
            //    λ id = y
        let lambda =
            Lambda (
                Function (Boolean, Boolean),
                Apply (Variable 0, Variable 1))
        let term = Apply (lambda, id)
        Assert.AreEqual<_>(Ok (Variable 0), Term.eval term)
