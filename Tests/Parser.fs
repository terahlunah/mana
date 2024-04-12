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
        test "table" {
            "#[\"foo\": \"bar\"]"
            == Ast.Table [ (Ast.Str "foo", Ast.Str "bar") ]
        }
    ]

let multiline =

    let (==) code expected = testParseMany code expected

    testGroup "multi lines" [
        test "all newlines" {
            "1\n2\n"
            == Ast.Block[Ast.Num 1
                         Ast.Num 2]
        }
        test "all but last newlines" {
            "1\n2"
            == Ast.Block[Ast.Num 1
                         Ast.Num 2]
        }
        test "multiple" {
            "\n\n1\n\n\n2\n"
            == Ast.Block[Ast.Num 1
                         Ast.Num 2]
        }
    ]

let lambdas =

    let (==) code expected = testParseExpr code expected

    testGroup "lambdas" [
        test "lambda empty" { "{}" == Ast.Closure([], Ast.Block []) }
        test "lambda with no arg" { "{ 4 }" == Ast.Closure([], Ast.Block [ Ast.Num 4 ]) }
        test "lambda with implicit arg" {
            "{ it }"
            == Ast.Closure([ "it" ], Ast.Block[Ast.Call("it", [])])
        }
        test "lambda with 1 arg" {
            "{ |x| x }"
            == Ast.Closure([ "x" ], Ast.Block[Ast.Call("x", [])])
        }
        test "lambda with 1 arg no body" { "{ |x| }" == Ast.Closure([ "x" ], Ast.Block []) }
        test "lambda with 2 args" {
            "{ |x, y| x}"
            == Ast.Closure([ "x"; "y" ], Ast.Block[Ast.Call("x", [])])
        }
        test "lambda returning lambda" {
            "{ |n| {|x| add(x, n) } }"
            == Ast.Closure(
                [ "n" ],
                Ast.Block [
                    Ast.Closure(
                        [ "x" ],
                        Ast.Block [
                            Ast.Call("add", [ Ast.Call("x", []); Ast.Call("n", []) ])
                        ]
                    )
                ]
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
            == Ast.Call("f", [ Ast.Num 1; Ast.Block [ Ast.Num 2 ]; Ast.Num 3 ])
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
                            Ast.Closure([ "it" ], Ast.Block[Ast.Call("__add", [ Ast.Call("it", []); Ast.Num 1.0 ])])
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
                            Ast.Call(
                                "map",
                                [
                                    Ast.Closure([ "it" ], Ast.Block[Ast.Call("it", [])])
                                ]
                            )
                        ]
                    )
                    Call("len", [])
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
                == Ast.Match(
                    Ast.Call("x", []),
                    [
                        {
                            pattern = Pattern.Num 0
                            body = Ast.Bool true
                        }
                        {
                            pattern = Pattern.Wildcard
                            body = Ast.Bool false
                        }
                    ]
                )
            }
            test "list" {
                "match x | [] -> (true) | [0] -> (false)"
                == Ast.Match(
                    Ast.Call("x", []),
                    [
                        {
                            pattern = Pattern.List []
                            body = Ast.Block [ Ast.Bool true ]
                        }
                        {
                            pattern = Pattern.List [ ListPatternItem.Single(Pattern.Num 0) ]
                            body = Ast.Block [ Ast.Bool false ]
                        }
                    ]
                )
            }
        ]

        testGroup "patterns" [
            test "nil" {
                "let nil = nil"
                == Ast.Let(Pattern.Nil, Ast.Nil, Ast.Block [])
            }
            test "true" {
                "let true = nil"
                == Ast.Let(Pattern.Bool true, Ast.Nil, Ast.Block [])
            }
            test "false" {
                "let false = nil"
                == Ast.Let(Pattern.Bool false, Ast.Nil, Ast.Block [])
            }
            test "number" {
                "let 3.14 = nil"
                == Ast.Let(Pattern.Num 3.14, Ast.Nil, Ast.Block [])
            }
            test "string" {
                "let \"a\" = nil"
                == Ast.Let(Pattern.Str "a", Ast.Nil, Ast.Block [])
            }
            test "symbol" {
                "let x = [2]"
                == Ast.Let(Pattern.Symbol "x", Ast.List [ Ast.Num 2 ], Ast.Block [])
            }
            test "list 0" {
                "let [] = nil"
                == Ast.Let(Pattern.List [], Ast.Nil, Ast.Block [])
            }
            test "list 1" {
                "let [1] = nil"
                == Ast.Let(Pattern.List [ ListPatternItem.Single(Pattern.Num 1) ], Ast.Nil, Ast.Block [])
            }
            test "list symbol" {
                "let [a] = [1]"
                == Ast.Let(
                    Pattern.List [ ListPatternItem.Single(Pattern.Symbol "a") ],
                    Ast.List [ Ast.Num 1 ],
                    Ast.Block []
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
