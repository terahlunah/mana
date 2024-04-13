module Tests.Compiler

open Mana
open Tests.Testing

let inline runTest (code: string) (expected: string) =

    let m = Mana()

    let got = m.run code

    let got = Value.repr got

    Test.equalMessage code got expected

let inline (==) o expected = runTest o expected

let compilerTests =

    testGroup "list bindings" [
        test "1 number" { "let [1] = [1]" == "nil" }
        test "2 numbers" { "let [1, 2] = [1, 2]" == "nil" }
        test "1 symbol" { "let [a] = [1] \n a" == "1" }
        test "2 symbols" { "let [a, b] = [1, 2] \n b" == "2" }
        test "rest only" { "let [..l] = [1, 2] \n l" == "[1, 2]" }
        test "rest first" { "let [1, ..l] = [1, 2] \n l" == "[2]" }
        test "rest middle" { "let [1, ..l, 4] = [1, 2, 3, 4] \n l" == "[2, 3]" }
        test "rest last" { "let [1, 2, ..l] = [1, 2, 3] \n l" == "[3]" }
    ]
