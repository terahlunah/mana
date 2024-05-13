module Tests.Optimizer

open Mana
open Tests.Testing

let testOptimize code expected =
    let tokens = Lexer.lex code
    let ast = Parser.parseMany tokens

    let got = sprintf $"%A{ast}"
    let expected = sprintf $"%A{expected}"

    Test.equalMessage code got expected

let (==) code expected = testOptimize code expected

let optimizerTests =

    testGroup "optimizer" [
        test "constant" { "3 + 2 * 4" == Expr.Num 11 }
        test "single expr block" { "(0)" == Expr.Num 0 }
    ]
