module Tests.Eval

open Rune.Interpreter
open Rune
open Yute.Testing

let testEval expr expected =
    let env = Rune.env ()

    let result = expr |> Reader.readMany env |> Evaluator.evalMany env
    let got = result.Repr()
    Test.equalMessage $"{expr}" got expected

let (==) expr expected = testEval expr expected

let primitives =
    testGroup "primitives" [
        test "number" { "1" == "1" }
        test "strings" { "\"a\"" == "\"a\"" }
    ]

let arithmetic =
    testGroup "arithmetic" [
        test "add" { "(+ 6 2)" == "8" }
        test "sub" { "(- 6 2)" == "4" }
        test "mul" { "(* 6 2)" == "12" }
        test "div" { "(/ 6 2)" == "3" }
    ]

let logic =
    testGroup "logic" [
        test "not" {
            "(not true)" == "false"
            "(not false)" == "true"
        }
        test "and" {
            "(and true true)" == "true"
            "(and false true)" == "false"
            "(and true false)" == "false"
            "(and false false)" == "false"
        }
        test "or" {
            "(or true true)" == "true"
            "(or false true)" == "true"
            "(or true false)" == "true"
            "(or false false)" == "false"
        }
        test "xor" {
            "(xor true true)" == "false"
            "(xor false true)" == "true"
            "(xor true false)" == "true"
            "(xor false false)" == "false"
        }
    ]

let comparison =
    testGroup "comparison" [
        test "greater than" {
            "(> 2 1)" == "true"
            "(> 1 1)" == "false"
            "(> 1 2)" == "false"
        }
        test "greater or equal" {
            "(>= 2 1)" == "true"
            "(>= 1 1)" == "true"
            "(>= 1 2)" == "false"
        }
        test "less than" {
            "(< 2 1)" == "false"
            "(< 1 1)" == "false"
            "(< 1 2)" == "true"
        }
        test "less or equal" {
            "(<= 2 1)" == "false"
            "(<= 1 1)" == "true"
            "(<= 1 2)" == "true"
        }
        test "equal" {
            "(= 2 1)" == "false"
            "(= 1 1)" == "true"
        }
        test "not equal" {
            "(!= 2 1)" == "true"
            "(!= 1 1)" == "false"
        }
    ]

let call =
    testGroup "call" [
        test "0 args" { "(defn f [] 5) (f)" == "5" }
        test "1 args" { "(defn f [a] a) (f 5)" == "5" }
        test "2 args" { "(defn f [a b] (+ a b)) (f 3 2)" == "5" }
    ]

let meta = testGroup "meta" [ test "eval" { "(eval '(+ 1 2))" == "3" } ]

let evalTests =
    testGroup "eval" [
        primitives
        arithmetic
        logic
        comparison
        call
        meta
    ]
