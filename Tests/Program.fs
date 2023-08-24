open Mana
open Mana.Parser
open FsToolkit.ErrorHandling
open System

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

let source =
    """def pi () = 3.14
def tau () = 2 * pi ()
def main () = tau ()
"""

let lexer = Lexer(source)
let tokens = lexer.lex () |> Result.unwrap
let parser = Parser(tokens)

let program =
    parser.parse ()
    |> Result.teeError (fun e -> printfn $"Error: %s{e.pretty ()}")
    |> Result.unwrap

debug program

let value =
    Engine.init
    |> Engine.withStd
    |> Engine.loadProgram program
    |> Result.unwrap
    |> Engine.run "main" [ Value.Unit ]
    |> Result.unwrap

value |> Value.toString |> display
