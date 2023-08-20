open Mana
open Mana.Ast
open FsToolkit.ErrorHandling
open Mana.Lexer

// let doubleDefinition: Definition = {
//     name = "double"
//     args = [ Argument "x" ]
//     body = Call("add", [ Ident "x"; Ident "x" ])
// }
//
// let mainDefinition: Definition = {
//     name = "main"
//     args = []
//     body =
//         Call(
//             "print",
//             [
//                 Str "Result: "
//                 Call("Main.double", [ Num 3.14 ])
//             ]
//         )
// }
//
// let mainModule: Module = {
//     name = "Main"
//     definitions = [ doubleDefinition; mainDefinition ]
// }
//
// let program: Program = { modules = [ mainModule ] }

printfn "↵"
printfn "\u21B5"

let source =
    """def + a b = add a b
def main = + 2 3
"""

let lexer = Lexer(source)
let tokens = lexer.lex () |> Result.unwrap
let parser = Parser(tokens)
let program = parser.parse () |> Result.unwrap

debug program

let value =
    Engine.init
    |> Engine.set "add" (Value.Fun Mana.Std.Core.add)
    |> Engine.set "print" (Value.Fun Mana.Std.Core.display)
    |> Engine.loadProgram program
    |> Result.unwrap
    |> Engine.run "main" []
    |> Result.unwrap

value |> Value.toString |> display
