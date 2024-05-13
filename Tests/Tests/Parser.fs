module Tests.Parser

open Mana
open Tests.Testing

let testParseExpr code expected =
    let tokens = Lexer.lex code
    let ast = Parser.parseExprRaw tokens

    let got = sprintf $"%A{ast}"
    let expected = sprintf $"%A{expected}"

    Test.equalMessage code got expected

let testParseMany code expected =
    let tokens = Lexer.lex code
    let ast = Parser.parseManyRaw tokens

    let got = sprintf $"%A{ast}"
    let expected = sprintf $"%A{expected}"

    Test.equalMessage code got expected

let primitives =
    let (==) code expected = testParseExpr code expected

    testGroup "primitives" [
        test "nil" { "nil" == Expr.Nil }
        test "true" { "true" == Expr.Bool true }
        test "false" { "false" == Expr.Bool false }
        test "number" { "3.14" == Expr.Num 3.14 }
        test "string" { "\"a\"" == Expr.Str "a" }
        test "list 0" { "[]" == Expr.List [] }
        test "list 1" { "[1]" == Expr.List [ Expr.Num 1 |> Elem ] }
        test "list 2" {
            "[1, 2]"
            == Expr.List [ Expr.Num 1 |> Elem; Expr.Num 2 |> Elem ]
        }
        test "table" {
            "#[\"foo\": \"bar\"]"
            == Expr.Table [ (Expr.Str "foo", Expr.Str "bar") ]
        }
    ]

let multiline =

    let (==) code expected = testParseMany code expected

    testGroup "multi lines" [
        test "all newlines" { "1\n2\n" == Expr.Block[Expr.Num 1 Expr.Num 2] }
        test "all but last newlines" { "1\n2" == Expr.Block[Expr.Num 1 Expr.Num 2] }
        test "multiple" { "\n\n1\n\n\n2\n" == Expr.Block[Expr.Num 1 Expr.Num 2] }
    ]

let lambdas =

    let (==) code expected = testParseExpr code expected

    testGroup "lambdas" [
        test "lambda empty" { "{}" == Expr.Closure([], Expr.Block []) }
        test "lambda with no arg" { "{ 4 }" == Expr.Closure([], Expr.Block [ Expr.Num 4 ]) }
        test "lambda with implicit arg" {
            "{ it }"
            == Expr.Closure([ "it" ], Expr.Block[Expr.Call("it", [])])
        }
        test "lambda with 1 arg" {
            "{ |x| x }"
            == Expr.Closure([ "x" ], Expr.Block[Expr.Call("x", [])])
        }
        test "lambda with 1 arg no body" { "{ |x| }" == Expr.Closure([ "x" ], Expr.Block []) }
        test "lambda with 2 args" {
            "{ |x, y| x}"
            == Expr.Closure([ "x"; "y" ], Expr.Block[Expr.Call("x", [])])
        }
        test "lambda returning lambda" {
            "{ |n| {|x| add(x, n) } }"
            == Expr.Closure(
                [ "n" ],
                Expr.Block [
                    Expr.Closure(
                        [ "x" ],
                        Expr.Block [
                            Expr.Call("add", [ Expr.Call("x", []); Expr.Call("n", []) ])
                        ]
                    )
                ]
            )
        }
    ]

let calls =

    let (==) code expected = testParseExpr code expected

    testGroup "function calls" [
        test "f()" { "f()" == Expr.Call("f", []) }
        test "f(1)" { "f(1)" == Expr.Call("f", [ Expr.Num 1 ]) }
        test "f(1,2)" { "f(1, 2)" == Expr.Call("f", [ Expr.Num 1; Expr.Num 2 ]) }
        test "f" { "f" == Expr.Call("f", []) }
        test "f 1" { "f 1" == Expr.Call("f", [ Expr.Num 1 ]) }
        test "f 1 2" { "f 1 2" == Expr.Call("f", [ Expr.Num 1; Expr.Num 2 ]) }
        test "f(1) 2" { "f(1) 2" == Expr.Call("f", [ Expr.Num 1; Expr.Num 2 ]) }
        test "f(1) (2) 3" {
            "f(1) (2) 3"
            == Expr.Call(
                "f",
                [
                    Expr.Num 1
                    Expr.Block [ Expr.Num 2 ]
                    Expr.Num 3
                ]
            )
        }
        test "f(1) 2 3" {
            "f(1) 2 3"
            == Expr.Call("f", [ Expr.Num 1; Expr.Num 2; Expr.Num 3 ])
        }
        test "f g" { "f g" == Expr.Call("f", [ Expr.Call("g", []) ]) }
        test "f (g 0)" {
            "f (g 0)"
            == Expr.Call("f", [ Expr.Call("g", [ Expr.Num 0 ]) ])
        }
    ]

let operators =

    let (==) code expected = testParseExpr code expected

    testGroup "operators" [
        test "unary" { "-1" == Expr.Call("__neg", [ Expr.Num 1 ]) }
        test "binary" { "1 + 2" == Expr.Call("__add", [ Expr.Num 1; Expr.Num 2 ]) }
        test "precedence +*" {
            "1 + 2 * 3"
            == Expr.Call(
                "__add",
                [
                    Expr.Num 1
                    Expr.Call("__mul", [ Expr.Num 2; Expr.Num 3 ])
                ]
            )
        }
        test "precedence *+" {
            "1 * 2 + 3"
            == Expr.Call(
                "__add",
                [
                    Expr.Call("__mul", [ Expr.Num 1; Expr.Num 2 ])
                    Expr.Num 3
                ]
            )
        }
        test "chain" {
            "[1, 2].map {\nit + 1\n}"
            == Expr.Call(
                "map",
                [
                    Expr.List[Expr.Num 1 |> Elem Expr.Num 2 |> Elem]
                    Expr.Closure([ "it" ], Expr.Block[Expr.Call("__add", [ Expr.Call("it", []); Expr.Num 1.0 ])])

                ]
            )
        }

        test "nested chain" {
            "[1, 2].map {it}.len"
            == Expr.Call(
                "len",
                [
                    Expr.Call(
                        "map",
                        [
                            Expr.List[Expr.Num 1 |> Elem Expr.Num 2 |> Elem]
                            Expr.Closure([ "it" ], Expr.Block[Expr.Call("it", [])])
                        ]
                    )

                ]
            )
        }
    ]

let bindings =

    let (==) code expected = testParseExpr code expected

    testGroup "bindings" [

        testGroup "match" [
            test "number" {
                "match x | 0 -> true | _ -> false"
                == Expr.Match(
                    Expr.Call("x", []),
                    [
                        {
                            pattern = Pattern.Num 0
                            guard = None
                            body = Expr.Bool true
                        }
                        {
                            pattern = Pattern.Wildcard
                            guard = None
                            body = Expr.Bool false
                        }
                    ]
                )
            }
            test "list" {
                "match x | [] -> (true) | [0] -> (false)"
                == Expr.Match(
                    Expr.Call("x", []),
                    [
                        {
                            pattern = Pattern.List []
                            guard = None
                            body = Expr.Block [ Expr.Bool true ]
                        }
                        {
                            pattern = Pattern.List [ ListPatternItem.Single(Pattern.Num 0) ]
                            guard = None
                            body = Expr.Block [ Expr.Bool false ]
                        }
                    ]
                )
            }
        ]

        testGroup "patterns" [
            test "nil" { "let nil = nil" == Expr.Let(Pattern.Nil, Expr.Nil) }
            test "true" { "let true = nil" == Expr.Let(Pattern.Bool true, Expr.Nil) }
            test "false" { "let false = nil" == Expr.Let(Pattern.Bool false, Expr.Nil) }
            test "number" { "let 3.14 = nil" == Expr.Let(Pattern.Num 3.14, Expr.Nil) }
            test "string" { "let \"a\" = nil" == Expr.Let(Pattern.Str "a", Expr.Nil) }
            test "symbol" {
                "let x = [2]"
                == Expr.Let(Pattern.Symbol "x", Expr.List [ Expr.Num 2 |> Elem ])
            }
            test "list 0" { "let [] = nil" == Expr.Let(Pattern.List [], Expr.Nil) }
            test "list 1" {
                "let [1] = nil"
                == Expr.Let(Pattern.List [ ListPatternItem.Single(Pattern.Num 1) ], Expr.Nil)
            }
            test "list symbol" {
                "let [a] = [1]"
                == Expr.Let(
                    Pattern.List [ ListPatternItem.Single(Pattern.Symbol "a") ],
                    Expr.List [ Expr.Num 1 |> Elem ]
                )
            }
        ]
    ]

let parserTests =
    testGroup "parser" [
        primitives
        multiline
        lambdas
        calls
        operators
        bindings
    ]
