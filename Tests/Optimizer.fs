module Tests.Optimizer

open Mana
open Yute.Testing
open Yute

let testOptimize code expected =
    let tokens = Lexer.lex code
    let ast = Parser(tokens).parseExpr 0
    let optimized = Optimizer.optimize ast

    let got = sdebug optimized
    let expected = sdebug expected

    Test.equalMessage code got expected

let (==) code expected = testOptimize code expected

let optimizerTests =

    testGroup "optimizer" [
        test "constant" { "3 + 2 * 4" == Ast.Num 11 }
        test "chain 0 arg" {
            "\"test\".uppercase"
            == Ast.Call("uppercase", [ Ast.Str "test" ])
        }
        test "chain 1 arg" {
            "[0].map { it }"
            == Ast.Call(
                "map",
                [
                    Ast.List [ Ast.Num 0 ]
                    Ast.Closure([], [ Ast.Call("it", []) ])
                ]
            )
        }
    ]
