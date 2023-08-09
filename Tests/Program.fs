open Mana
open Mana.Ast
open FsToolkit.ErrorHandling

let doubleDefinition: Definition = {
    name = "double"
    args = [ "x" ]
    body = Call("add", [ Ident "x"; Ident "x" ])
}

let mainDefinition: Definition = {
    name = "main"
    args = []
    body =
        Call(
            "print",
            [
                Str "Result: "
                Call("Main.double", [ Num 3.14 ])
            ]
        )
}

let mainModule: Module = {
    name = "Main"
    definitions = [ doubleDefinition; mainDefinition ]
}

let program: Program = { modules = [ mainModule ] }

let value =
    Engine.init
    |> Engine.set "add" (Value.Fun Mana.Std.Core.add)
    |> Engine.set "print" (Value.Fun Mana.Std.Core.display)
    |> Engine.loadProgram program
    |> Result.unwrap
    |> Engine.run "Main.main" []
    |> Result.unwrap

value |> Value.toString |> display
