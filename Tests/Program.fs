open Mana
open Mana.Ast

let env = Env.empty |> Env.set "add" (Value.Native Mana.Std.Core.add)

let ast = Expr.Call("add", [ NumberLiteral 2; NumberLiteral 3 ])

let code = Compiler.compileExpr ast

match code with
| Ok code -> printfn $"%A{code env}"
| Error err -> printfn $"%A{err}"
