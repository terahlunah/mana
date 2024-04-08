module Mana.Optimizer

// Also performs desugaring
let rec optimize (ast: Ast) : Ast =
    match ast with
    | Ast.Call(name, args) ->
        let args = args |> List.map optimize

        match name, args with
        // Constants
        | "__neg", [ Ast.Num n ] -> Ast.Num -n
        | "__add", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a + b)
        | "__sub", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a - b)
        | "__mul", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a * b)
        | "__div", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a / b)
        | "__pow", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a ** b)
        | "__mod", [ Ast.Num a; Ast.Num b ] -> Ast.Num(a % b)

        // Desugaring
        | "__chain", [ x; Ast.Call(name, args) ] -> Ast.Call(name, optimize x :: List.map optimize args)
        | _ -> Ast.Call(name, args)

    | Ast.List(items) -> items |> List.map optimize |> Ast.List
    | Ast.Table(items) ->
        items
        |> List.map (fun (k, v) -> optimize k, optimize v)
        |> Ast.Table
    | Ast.Let(name, body) -> Ast.Let(name, body |> optimize)
    | Ast.Closure(args, body) -> Ast.Closure(args, body |> List.map optimize)
    | _ -> ast
