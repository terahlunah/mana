open Mana
open Mana.Ast
open FsToolkit.ErrorHandling

let env = Env.empty |> Env.set "add" (Value.Fun Mana.Std.Core.add)

// let ast =
//     Expr.Block [
//         Let("f", Closure([ "a"; "b" ], Call("add", [ Ident "a"; Ident "b" ])))
//         Call("f", [ Num 2; Num 3 ])
//     ]

let ast =
    Match(
        List [
            List [ Num 1.0; Str "hello" ]
            List [ Num 2.0; Str "world" ]
        ],
        [
            {
                pattern =
                    ListPattern [
                        ListPattern [ NumPattern 1.0; StrPattern "hello" ]
                        ListPattern [ NumPattern 2.0; StrPattern "world" ]
                    ]
                body = Str "Matched nested list"
            }
            {
                pattern = Underscore
                body = Str "Matched anything else"
            }
        ]
    )

let value =
    Compiler.compileExpr ast
    |> Result.anyBind (fun code -> code env)

match value with
| Ok v -> debug v
| Error err -> debug err
