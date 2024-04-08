module Tests.Indexing

open Rune.Interpreter
open Rune
open Yute.Testing

let testEval expr expected =
    let env = Rune.env ()

    let result = expr |> Reader.readMany env |> Evaluator.evalMany env
    let got = result.Repr()
    Test.equalMessage $"{expr}" got expected

let (==) expr expected = testEval expr expected

let getTests =
    testGroup "get" [
        test "nil nil" { "(get nil nil)" == "nil" }
        test "table nil" { "(get {:foo 1} nil)" == "nil" }
        test "table keyword" { "(get {:foo 1} :foo)" == "1" }
        test "vec nil" { "(get [1] nil)" == "nil" }
        test "vec num" { "(get [1] 0)" == "1" }
    ]

let tableTests =
    testGroup "table" [
        test "nil" { "({:foo 1} nil)" == "nil" }
        test "keyword" { "({:foo 1} :foo)" == "1" }
    ]

let keywordTests =
    testGroup "keyword" [
        test "nil" { "(:foo nil)" == "nil" }
        test "table" { "(:foo {:foo 1})" == "1" }
    ]

let vecTests =
    testGroup "vec" [
        test "nil" { "([1] nil)" == "nil" }
        test "num" { "([1] 0)" == "1" }
    ]

let indexingTests =
    testGroup "indexing" [ getTests; tableTests; keywordTests; vecTests ]
