module Tests.Parser

open Mana
open Yute.Testing
open Yute

let testParseExpr code expected =
    let tokens = Lexer.lex code
    let ast = Parser.parseExprRaw tokens

    let got = sdebug ast
    let expected = sdebug expected

    Test.equalMessage code got expected

let testParseMany code expected =
    let tokens = Lexer.lex code
    let ast = Parser.parseManyRaw tokens

    let got = sdebug ast
    let expected = sdebug expected

    Test.equalMessage code got expected

let primitives =
    let (==) code expected = testParseExpr code expected

    testGroup "primitives" [
        test "nil" { "nil" == Ast.Nil }
        test "true" { "true" == Ast.Bool true }
        test "false" { "false" == Ast.Bool false }
        test "number" { "3.14" == Ast.Num 3.14 }
        test "string" { "\"a\"" == Ast.Str "a" }
        test "list 0" { "[]" == Ast.List [] }
        test "list 1" { "[1]" == Ast.List [ Ast.Num 1 ] }
        test "list 2" { "[1, 2]" == Ast.List [ Ast.Num 1; Ast.Num 2 ] }
    ]

let multiline =

    let (==) code expected = testParseMany code expected

    testGroup "multi lines" [
        test "all newlines" { "1\n2\n" == [ Ast.Num 1; Ast.Num 2 ] }
        test "all but last newlines" { "1\n2" == [ Ast.Num 1; Ast.Num 2 ] }
        test "multiple" { "\n\n1\n\n\n2\n" == [ Ast.Num 1; Ast.Num 2 ] }
    ]

let bindings =

    let (==) code expected = testParseExpr code expected

    testGroup "bindings" [
        test "number" { "let x = 2" == Ast.Let("x", Ast.Num 2) }
        test "list" { "let x = [2]" == Ast.Let("x", Ast.List [ Ast.Num 2 ]) }
    ]

let lambdas =

    let (==) code expected = testParseExpr code expected

    testGroup "lambdas" [
        test "lambda empty" { "let f = {}" == Ast.Let("f", Ast.Closure([], [])) }
        test "lambda with no arg" {
            "let f = { 4 }"
            == Ast.Let("f", Ast.Closure([], [ Ast.Num 4 ]))
        }
        test "lambda with implicit arg" {
            "let f = { it }"
            == Ast.Let("f", Ast.Closure([], [ Ast.Call("it", []) ]))
        }
        test "lambda with 1 arg" {
            "let f = { |x| x }"
            == Ast.Let("f", Ast.Closure([ "x" ], [ Ast.Call("x", []) ]))
        }
        test "lambda with 1 arg no body" { "let f = { |x| }" == Ast.Let("f", Ast.Closure([ "x" ], [])) }
        test "lambda with 2 args" {
            "let f = { |x, y| x}"
            == Ast.Let("f", Ast.Closure([ "x"; "y" ], [ Ast.Call("x", []) ]))
        }
        test "lambda returning lambda" {
            "let addN = { |n| {|x| add(x, n) } }"
            == Ast.Let(
                "addN",
                Ast.Closure(
                    [ "n" ],
                    [
                        Ast.Closure(
                            [ "x" ],
                            [
                                Ast.Call("add", [ Ast.Call("x", []); Ast.Call("n", []) ])
                            ]
                        )
                    ]
                )
            )
        }
    ]

let calls =

    let (==) code expected = testParseExpr code expected

    testGroup "function calls" [
        test "f()" { "f()" == Ast.Call("f", []) }
        test "f(1)" { "f(1)" == Ast.Call("f", [ Ast.Num 1 ]) }
        test "f(1,2)" { "f(1, 2)" == Ast.Call("f", [ Ast.Num 1; Ast.Num 2 ]) }
        test "f" { "f" == Ast.Call("f", []) }
        test "f 1" { "f 1" == Ast.Call("f", [ Ast.Num 1 ]) }
        test "f 1 2" { "f 1 2" == Ast.Call("f", [ Ast.Num 1; Ast.Num 2 ]) }
        test "f(1) 2" { "f(1) 2" == Ast.Call("f", [ Ast.Num 1; Ast.Num 2 ]) }
        test "f(1) (2) 3" {
            "f(1) (2) 3"
            == Ast.Call("f", [ Ast.Num 1; Ast.Num 2; Ast.Num 3 ])
        }
        test "f(1) 2 3" {
            "f(1) 2 3"
            == Ast.Call("f", [ Ast.Num 1; Ast.Num 2; Ast.Num 3 ])
        }
        test "f g" { "f g" == Ast.Call("f", [ Ast.Call("g", []) ]) }
    ]

let operators =

    let (==) code expected = testParseExpr code expected

    testGroup "operators" [
        test "unary" { "-1" == Ast.Call("__neg", [ Ast.Num 1 ]) }
        test "binary" { "1 + 2" == Ast.Call("__add", [ Ast.Num 1; Ast.Num 2 ]) }
        test "precedence +*" {
            "1 + 2 * 3"
            == Ast.Call(
                "__add",
                [
                    Ast.Num 1
                    Ast.Call("__mul", [ Ast.Num 2; Ast.Num 3 ])
                ]
            )
        }
        test "precedence *+" {
            "1 * 2 + 3"
            == Ast.Call(
                "__add",
                [
                    Ast.Call("__mul", [ Ast.Num 1; Ast.Num 2 ])
                    Ast.Num 3
                ]
            )
        }
        test "chain" {
            "[1, 2].map {\nit + 1\n}"
            == Ast.Call(
                "__chain",
                [
                    Ast.List[Ast.Num 1
                             Ast.Num 2]
                    Ast.Call(
                        "map",
                        [
                            Ast.Closure(
                                [ "it" ],
                                [
                                    Ast.Call("__add", [ Ast.Call("it", []); Ast.Num 1.0 ])
                                ]
                            )
                        ]
                    )
                ]
            )
        }

        test "nested chain" {
            "[1, 2].map {it}.len"
            == Ast.Call(
                "__chain",
                [
                    Ast.Call(
                        "__chain",
                        [
                            Ast.List[Ast.Num 1
                                     Ast.Num 2]
                            Ast.Call("map", [ Ast.Closure([ "it" ], [ Ast.Call("it", []) ]) ])
                        ]
                    )
                    Call("len", [])
                ]
            )
        }
    ]

let parserTests =
    testGroup "parser" [
        primitives
        multiline
        bindings
        lambdas
        calls
        operators
    ]
